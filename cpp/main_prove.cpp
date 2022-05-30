#include "inc/hypergraph.h"


class ColorScheme {
    Hypergraph& g;

    std::vector<Hypergraph::Edge> pending;

public:
    ColorScheme(Hypergraph& g) : g(g) { }

    void locateAll();
    void merge(Hypergraph::Edge* e1, Hypergraph::Edge* e2);

    void join(Hypergraph::Edge* e, Hypergraph::Vertex *u);

    void veryInefficientCompact();

    static const Hypergraph::label_t COND_MERGE;

protected:
    void merge(Hypergraph::Vertex* u1, Hypergraph::Vertex* u2);
    void veryInefficientCompact(Hypergraph::Edge* e);
};


const Hypergraph::label_t ColorScheme::COND_MERGE = Hypergraph::str2label("?~");

void ColorScheme::locateAll() {
    std::vector<Hypergraph::Edge*> to_drop;

    for (auto e : g.edges_by_kind[Hypergraph::str2label("?~")]) {
        std::cout << "// " << e->vertices[1]->id << "~" << e->vertices[2]->id << std::endl;
        for (auto it = e->vertices.begin() + 1; it != e->vertices.end(); it++) {
            if ((*it)->color_index) {
                if ((*it)->color_index != e) {
                    merge((*it)->color_index, e);
                    to_drop.push_back(e);
                }
            }
            else (*it)->color_index = e;
        }
    }

    for (auto e : to_drop) g.removeEdge(e);
}

void ColorScheme::merge(Hypergraph::Edge* e1, Hypergraph::Edge* e2) {
    for (auto it = e2->vertices.begin() + 1; it != e2->vertices.end(); it++) {
        if (!e1->containsSource(*it)) {
            join(e1, *it);
        }
    }
}

void ColorScheme::merge(Hypergraph::Vertex* u1, Hypergraph::Vertex* u2) {
    auto e1 = u1->color_index, e2 = u2->color_index;
    if (e1) {
        if (e2) merge(e1, e2);
        else {
            u2->color_index = e1;
            e1->vertices.push_back(u2);
        }
    }
    else if (e2) {
        u1->color_index = e2;
        e2->vertices.push_back(u1);
    }
    else {
        pending.push_back({.kind = COND_MERGE, .vertices = {u1, u2}});
    }
}

void ColorScheme::join(Hypergraph::Edge* e, Hypergraph::Vertex *u) {
    int index = e->vertices.size();
    e->vertices.push_back(u);
    u->edges.push_back({.index = index, .e = e});
    u->color_index = e;
}


bool slices_compat(Hypergraph& g,
        const std::vector<Hypergraph::Vertex*>& a1,
        const std::vector<Hypergraph::Vertex*>& a2, 
        int start, Hypergraph::Assumptions assumptions)
{
    if (a1.size() != a2.size()) return false;
    int n = a1.size();
    for (int i = start; i < n; i++)
        if (!g.compatVertices(a1[i], a2[i], assumptions)) return false;
    return true;
}

void ColorScheme::veryInefficientCompact(Hypergraph::Edge* e) {
    auto color = e->target();
    for (auto it1 = e->vertices.begin() + 1; it1 != e->vertices.end(); it1++) {
        auto& edges1 = (*it1)->edges;
        for (auto it2 = it1 + 1; it2 != e->vertices.end(); it2++) {
            auto& edges2 = (*it2)->edges;
            for (auto& e1 : edges1) {
                if (e1.index > 0 && g.isFunctional(e1.e->kind)) {
                    for (auto& e2 : edges2) {
                        if (e1.index == e2.index && e2.e->kind == e1.e->kind &&
                            slices_compat(g, e1.e->vertices, e2.e->vertices, 1, color)) {
                            auto v1 = e1.e->target(),
                                 v2 = e2.e->target();
                            if (!g.compatVertices(v1, v2, color)) {
                                std::cout << "// colored congruence closure for " 
                                    << v1->id << "~" << v2->id << std::endl;
                                merge(v1, v2);
                            }
                        }
                    }
                }
            }
        }
    }
}

void ColorScheme::veryInefficientCompact() {
    std::vector<Hypergraph::Edge> pending;

    for (auto e : g.edges_by_kind[COND_MERGE]) {
        veryInefficientCompact(e);
    }

    for (auto& e : pending) g.addEdge(e);
}



int main(int argc, char *argv[]) {

    std::string dataDir = (argc > 1) ? argv[1] : "data/tmp";
    int opt_rw_depth = 8;

    Hypergraph g;
    g.reserve(16000000, 20000000);

    /* read initial term(s) */
    std::ifstream fgraph(dataDir + "/input");
    g.fromText(fgraph);

    std::vector<RewriteRule> rw_rules;
    const int rw_depth = opt_rw_depth;

    {  /* read rewrite rules */
        std::ifstream frules(dataDir + "/rules");
        RewriteRule::fromTextMultiple(frules, rw_rules);
    }

    /* Rewrite Frenzy! */
    int gen = 0;
    ColorScheme cs(g);
    for (int i = 0; i < rw_depth; i++) {
        g.compact();
        g.gen++;
        cs.locateAll(); cs.veryInefficientCompact();
        for (auto& rule : rw_rules)
            rule.apply(g, gen);
        gen = g.gen;
    }
    g.compact();

    g.toText(std::cout);

    for (auto e : g.edges_by_kind[Hypergraph::str2label("?~")]) {
        std::cout << "// " << e->vertices[1]->id << "~" << e->vertices[2]->id << std::endl;
    }

    return 0;
}