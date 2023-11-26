#include <cassert>
#include <optional>
#include <getopt.h>

#include "colors.h"
#include "inc/hypergraph.h"


class ColorScheme {
    Hypergraph& g;

    typedef size_t color_index_t;

    struct pending_merge {
        Hypergraph::Vertex* u1;
        Hypergraph::Vertex* u2;
        Hypergraph::Assumptions assumptions;
    };

    std::vector<pending_merge> to_merge;

    mutable std::map<Hypergraph::Vertex::id_t, color_index_t> color_map;
    color_index_t color_parents[Colors::MAX_COLORS];

public:
        ColorScheme(Hypergraph& g) : g(g) {
        /* the ghost color is a hack */
        auto gh = byName("ghost");
        if (gh)
            ghost = { .u = *gh, .idx = lookup(*gh) };
        else
            ghost = { .u = NULL, .idx = Colors::UNDEF };
    }

    void locateAll();

    void veryInefficientCompact();

    static const Hypergraph::label_t CLR_MERGE;
    static const Hypergraph::label_t CLR_SUPER;

    color_index_t lookup(Hypergraph::Vertex* clr) const {
        auto it = color_map.find(clr->id);
        if (it != color_map.end()) return it->second;
        else {
            color_index_t new_idx = color_map.size();
            assert(new_idx < Hypergraph::MAX_COLORS);
            std::cout << "// color [" << new_idx << "] --> " << clr->id << std::endl;
            color_map[clr->id] = new_idx;
            return new_idx;
        }
    }

    std::optional<Hypergraph::Vertex*> byName(const std::string& name) const {
        auto it = g.edges_by_kind.find(Hypergraph::str2label(name));
        if (it != g.edges_by_kind.end())
            return it->second[0]->target();
        else
            return std::nullopt;
    }

    std::optional<color_index_t> lookup(std::string name) const {
        auto it = g.edges_by_kind.find(Hypergraph::str2label(name));
        if (it != g.edges_by_kind.end())
            return std::optional(lookup(it->second[0]->target()));
        else
            return std::nullopt;
    }

    void prepareHierarchyTable(Colors::Hierarchy& h);

    Hypergraph::Assumptions ghost;

protected:
    Hypergraph::Vertex* reprOf(Hypergraph::Vertex* u, color_index_t clr) const;
    bool compatVertices(Hypergraph::Vertex* u1, Hypergraph::Vertex* u2,
                        color_index_t clr) const;
    bool compatSlices(
        const std::vector<Hypergraph::Vertex*>& a1,
        const std::vector<Hypergraph::Vertex*>& a2, 
        int start, Hypergraph::Assumptions assumptions) const;

    void merge(Hypergraph::Vertex* u1, Hypergraph::Vertex* u2,
               Hypergraph::Assumptions assumptions);
    void merge(Hypergraph::Edge* e1, Hypergraph::Edge* e2,
               Colors::color_index_t clr);
    void join(Hypergraph::Edge* e, Hypergraph::Vertex *u,
              Colors::color_index_t clr);

    void canonicalize(Hypergraph::Edge* e, Colors::color_index_t clr);

    void prepareParents();
    void veryInefficientCompact(Hypergraph::Edge* e);
};


const Hypergraph::label_t ColorScheme::CLR_MERGE = Hypergraph::str2label("?~");
const Hypergraph::label_t ColorScheme::CLR_SUPER = Hypergraph::str2label("?>");

void ColorScheme::locateAll() {
    std::vector<Hypergraph::Edge*> to_drop;

    auto es = g.edges_by_kind[CLR_MERGE];
    for (auto e : es) {
        if (e->vertices.size() <= 2) {  // singleton colored class
            to_drop.push_back(e);
            continue;
        }
        size_t j = lookup(e->target());
        if (j == ghost.idx) assert(e->target() == ghost.u);
        canonicalize(e, j);
        auto vertices = e->vertices; // safe iteration
        for (auto it = vertices.begin() + 1; it != vertices.end(); it++) {
            auto ej = (*it)->color_index[j];
            if (ej == NULL) {
                (*it)->color_index[j] = e;
            }
            else if (ej != e) {
                assert(ej->target() == e->target());
                // symmetry breaking: must be consistent along all participating vertices.
                // otherwise color edges may be lost...
                auto ei = e;
                if (ej > ei) {
                    std::swap(ei, ej);
                    (*it)->color_index[j] = ej;
                }
                merge(ej, ei, j);
                to_drop.push_back(ei);
            }
        }

        if (j == 0) continue;
        j = ghost.idx;
        Hypergraph::Edge e0 = *e;
        e0.vertices[0] = ghost.u;
        //auto vertices = e->vertices; // safe iteration
        for (auto it = vertices.begin() + 1; it != vertices.end(); it++) {
            auto ej = (*it)->color_index[j];
            if (ej == NULL) {                
                (*it)->color_index[j] = g.addEdge(e0);
            }
            else {
                assert(ej->target() == ghost.u);
                merge(ej, &e0, j);
            }
        }        
    }

    for (auto e : to_drop) g.removeEdge(e);
}

/**
 * Gets the representative of the colored e-class containing u.
 * Respects the color hierarchy.
 */
Hypergraph::Vertex* ColorScheme::reprOf(Hypergraph::Vertex* u,
                                        Colors::color_index_t clr) const {
    if (clr == -1) return u;
    // canonicalize u according to parent color, if any, to find color edge
    u = reprOf(u, color_parents[clr]);
    return (u->color_index[clr] == NULL) ? u
                : u->color_index[clr]->vertices[1];
}

/**
 * This behavior should eventually replace `Hypergraph::compatVertices`
 * in some capacity.
 */
bool ColorScheme::compatVertices(Hypergraph::Vertex* u1,
                                 Hypergraph::Vertex* u2,
                                 color_index_t clr) const {
    return reprOf(u1, clr) == reprOf(u2, clr);
}

void ColorScheme::merge(Hypergraph::Edge* e1, Hypergraph::Edge* e2,
                        Colors::color_index_t clr) {
    for (auto it = e2->vertices.begin() + 1; it != e2->vertices.end(); it++) {
        if (!e1->containsSource(*it)) {
            join(e1, *it, clr);
        }
    }
}

void ColorScheme::merge(Hypergraph::Vertex* u1, Hypergraph::Vertex* u2,
                        Hypergraph::Assumptions assumptions) {
    auto clr = assumptions.idx;
    std::cout << "// merge " << u1-> id << "~" << u2->id << " @ " << assumptions.u->id << std::endl;
    u1 = reprOf(u1, color_parents[clr]);
    u2 = reprOf(u2, color_parents[clr]);
    auto e1 = u1->color_index[clr], e2 = u2->color_index[clr];
    if (e1) {
        if (e2) merge(e1, e2, clr);
        else    join(e1, u2, clr);
    }
    else if (e2) {
        join(e2, u1, clr);
    }
    else {
        g.addEdge({.kind = CLR_MERGE, .vertices = {assumptions.u, u1, u2}});
    }
}

void ColorScheme::join(Hypergraph::Edge* e, Hypergraph::Vertex *u,
                       Colors::color_index_t clr) {
    e->addVertex(u);
    u->color_index[clr] = e;
}

void ColorScheme::canonicalize(Hypergraph::Edge* e, Colors::color_index_t clr) {
    auto pclr = color_parents[clr];
    if (pclr != -1) {
        for (size_t i = 1; i < e->vertices.size(); i++) {
            e->setVertex(i, reprOf(e->vertices[i], pclr));
        }
    }
}


bool ColorScheme::compatSlices(
        const std::vector<Hypergraph::Vertex*>& a1,
        const std::vector<Hypergraph::Vertex*>& a2, 
        int start, Hypergraph::Assumptions assumptions) const
{
    if (a1.size() != a2.size()) return false;
    int n = a1.size();
    for (int i = start; i < n; i++)
        if (!compatVertices(a1[i], a2[i], assumptions.idx)) return false;
    return true;
}

void ColorScheme::veryInefficientCompact(Hypergraph::Edge* e) {
    auto color = e->target();
    Hypergraph::Assumptions assumptions = { .u = color, .idx = lookup(color) };
    for (auto it1 = e->vertices.begin() + 1; it1 != e->vertices.end(); it1++) {
        auto& edges1 = (*it1)->edges;
        for (auto it2 = it1 + 1; it2 != e->vertices.end(); it2++) {
            auto& edges2 = (*it2)->edges;
            for (auto& e1 : edges1) {
                if (e1.index > 0 && g.isFunctional(e1.e->kind)) {
                    for (auto& e2 : edges2) {
                        if (e1.index == e2.index && e2.e->kind == e1.e->kind &&
                            compatSlices(e1.e->vertices, e2.e->vertices, 1, assumptions)) {
                            auto v1 = e1.e->target(),
                                 v2 = e2.e->target();
                            if (!compatVertices(v1, v2, assumptions.idx)) {
                                std::cout << "// " 
                                    << v1->id << "~" << v2->id << "  @ "
                                    << color->id << " (compact)" << std::endl;
                                to_merge.push_back({v1, v2, assumptions});
                            }
                        }
                    }
                }
            }
        }
    }
}

void ColorScheme::veryInefficientCompact() {
    for (auto e : g.edges_by_kind[CLR_MERGE]) {
        veryInefficientCompact(e);
    }

    for (auto& p : to_merge) merge(p.u1, p.u2, p.assumptions);
    to_merge.resize(0);
}

void ColorScheme::prepareParents() {
    for (size_t i = 0; i < Colors::MAX_COLORS; i++)
        color_parents[i] = -1;

    for (auto e : g.edges_by_kind[CLR_SUPER]) {
        auto parent = lookup(e->target()), child = lookup(e->vertices[1]);
        color_parents[child] = parent;
    }
}

/**
 * Constructs the parent relationships between colors.
 * @note Hypergraph `g` must be compacted beforehand (`g.compact()`)
 *   to canonize color ids.
 */
void ColorScheme::prepareHierarchyTable(Colors::Hierarchy& h) {
    prepareParents();

    for (color_index_t i = 0; i < Colors::MAX_COLORS; ++i) {
        for (color_index_t j = 0; j < Colors::MAX_COLORS; ++j) {
            h[i][j] = i == j ? Colors::EQ : Colors::NONE;
        }
    }

    for (color_index_t i = 0; i < Colors::MAX_COLORS; ++i) {
        auto p = color_parents[i];
        while (p != -1) {
            h[p][i] = Colors::SUP;
            h[i][p] = Colors::SUB;
            p = color_parents[p];
        }
    }
}


struct cmdline_args_t {
    std::string input_dir;
    size_t rw_depth;
};

struct cmdline_args_t opts = {
    .input_dir = "data/tmp",
    .rw_depth = 8
};

void parse_cmdline(int argc, char **argv)
{
    int opt, opt_index;
    static struct option long_options[] = {
        {"rw-depth",       required_argument, 0,  0 },
        {0, 0, 0, 0}
    };

    while ((opt = getopt_long(argc, argv, "o:", long_options, &opt_index)) != -1) {
        switch (opt) {
            case 0:
                switch (opt_index) {
                    case 0: opts.rw_depth = atoi(optarg); break;
                    default: assert(false);
                }
            case 'o':
                /* todo */
                break;
        }
    }

    if (optind < argc) {
        assert(optind == argc - 1);
        opts.input_dir = argv[optind];
    }
}


int main(int argc, char *argv[]) {

    parse_cmdline(argc, argv);

    Hypergraph g;
    g.reserve(16000000, 20000000);

    /* read initial term(s) */
    std::ifstream fgraph(opts.input_dir + "/input");
    g.fromText(fgraph);

    std::vector<RewriteRule> rw_rules;
    const int rw_depth = opts.rw_depth;

    {  /* read rewrite rules */
        std::ifstream frules(opts.input_dir + "/rules");
        RewriteRule::fromTextMultiple(frules, rw_rules);
    }

    g.compact();  /* needed before `prepareHierarchyTable` */

    Chronicles chron(g);
    ColorScheme cs(g);
    cs.prepareHierarchyTable(g.color_hierarchy);
    if (cs.ghost.u)
        g.color_mask[cs.ghost.idx] = false;

    /* Rewrite Frenzy! */
    int gen = 0;
    for (int i = 0; i < rw_depth; i++) {
        g.compact();
        g.gen++;
        cs.locateAll(); cs.veryInefficientCompact();
        for (auto& rule : rw_rules)
            rule.apply(g, gen, RewriteRule::GEQ, &chron);
        gen = g.gen;
    }
    g.compact();

    cs.locateAll();
    g.toText(std::cout);

#if 0
    // Also output the colors associated with vertices
    for (auto& u : g.vertices) {
        if (!u.merged && u.color) {
            std::cout << "?. " << u.color->id << " " << u.id << std::endl;
        }
    }
#endif

    return 0;
}