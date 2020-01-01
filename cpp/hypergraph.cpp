#include <vector>
#include <map>
#include <set>
#include <string>
#include <iostream>
#include <sstream>
#include <fstream>


class Hypergraph {
public:
    struct Vertex;
    struct Edge;

    typedef uint64_t label_t;
    union scratch_t { void *p; int64_t z; };

    struct Incidence {
        Edge *e;
        int index;
    };

    struct Vertex {
        int id;
        std::vector<Incidence> edges;
        Vertex *merged;
        scratch_t scratch; /* sorry */

        Vertex *rep();
    };

    struct Edge {
        label_t kind;
        std::vector<Vertex*> vertices;
        int gen;
        bool removed;
    };

    typedef std::vector<Vertex*> Valuation;

    std::vector<Vertex> vertices;
    std::vector<Edge> edges;
    std::map<label_t, std::vector<Edge*>> edges_by_kind;

    int id_counter;
    bool dirty;
    int gen;

    Hypergraph() : id_counter(0), gen(0) {
        vertices.reserve(600);
        edges.reserve(24000);
    }

    void reserve(int nv, int ne) {
        vertices.reserve(nv);
        edges.reserve(ne);
    }

    Vertex* addVertex();

    Vertex* dupVertex(Vertex* u, const std::set<Vertex*>& anchored);

    Edge* addEdge(const Edge& edge);

    void removeEdge(Edge* edge);

    typedef const std::function< void(Valuation&, int) >& MatchCb;

    void findEdge(Edge& pattern, int gen_max, int index,
                  Valuation& valuation, MatchCb cb);

    void unifyEdge(Edge& edge, Edge& pattern,
                   Valuation& valuation, MatchCb cb);

    void findSubgraph(std::vector<Edge>& pattern, int nholes,
                      int gen_max, MatchCb cb);

    void findSubgraph(std::vector<Edge>& pattern,
                      int gen_max, int gen_so_far, int index, 
                      Valuation& valuation,
                      MatchCb cb);

    void merge(Vertex* u, Vertex* v);
    void compact(Vertex* u);
    void compact();

    // ------------
    // Input/output
    // ------------

    void fromText(std::istream& in);
    void toText(std::ostream& out);

    static label_t str2label(const std::string& s);
    static std::string label2str(label_t w);
};


//#define VERTEX_MATCH_THRESHOLD 20


Hypergraph::Vertex* Hypergraph::addVertex() {
    assert(vertices.size() < vertices.capacity());  /* can't allow realloc */
    Vertex u;
    u.id = ++id_counter;
    u.merged = NULL;
    vertices.push_back(u);
    return &vertices[vertices.size() - 1];
}

Hypergraph::Vertex* Hypergraph::dupVertex(Vertex* u, const std::set<Vertex*>& anchored) {
    Vertex* uc = addVertex();
    auto uedges = u->edges;
    for (auto& ie : uedges) {
        if (ie.index > 0) {
            Edge e = *ie.e;
            for (auto& v : e.vertices) {
                if (v == u) v = uc;
            }
            Vertex* uv = e.vertices[0];
            if (uv != uc && anchored.find(uv) == anchored.end())
                e.vertices[0] = dupVertex(uv, anchored);
            addEdge(e);
        }
    }
    return uc;
}

Hypergraph::Edge* Hypergraph::addEdge(const Edge& edge) {
    assert(edges.size() < edges.capacity());  /* can't allow realloc */
    edges.push_back(edge);
    Edge* e = &edges[edges.size() - 1];
    for (int i = 0; i < edge.vertices.size(); ++i) {
        Incidence ie = { e, i };
        edge.vertices[i]->edges.push_back(ie);
    }
    e->gen = gen;
    e->removed = false;
    edges_by_kind[edge.kind].push_back(e);
    return e;
}

template <typename T>
void remove_from_vec(std::vector<T>& vec, T elem) {
    auto it = std::find(vec.begin(), vec.end(), elem);
    if (it != vec.end()) {
        *it = vec.back();
        vec.pop_back();
    }
}

template <typename T, typename UnaryPredicate >
void remove_from_vec_if(std::vector<T>& vec, UnaryPredicate cond) {
    auto it = std::find_if(vec.begin(), vec.end(), cond);
    if (it != vec.end()) {
        *it = vec.back();
        vec.pop_back();
    }
}

void Hypergraph::removeEdge(Edge* edge) {
    remove_from_vec(edges_by_kind[edge->kind], edge);
    for (auto u : edge->vertices) {
        remove_from_vec_if(u->edges, [=] (const Incidence& e) {
            return e.e == edge;
        });
    }
    edge->removed = true;
}

void Hypergraph::findEdge(Edge& pattern, int gen_max, int index,
                          Valuation& valuation, MatchCb cb) {

    int n = pattern.vertices.size();
#ifndef VERTEX_MATCH_THRESHOLD
    Vertex *u = NULL;
    for (int i = index; !u && i < n; i++) {
        u = pattern.vertices[i];
        if (u->id < 0) u = valuation[~u->id];
    }
#else
    const int thres = VERTEX_MATCH_THRESHOLD;
    Vertex *u = NULL, *minu = NULL;
    for (int i = index; !(minu && minu->edges.size() < thres) && i < n; i++) {
        u = pattern.vertices[i];
        if (u->id < 0) u = valuation[~u->id];
        if (u && (!minu || u->edges.size() < minu->edges.size())) minu = u;
    }

    u = minu;
#endif

    if (u) {
        for (auto& e : u->edges) {
            unifyEdge(*e.e, pattern, valuation, cb);
        }
    }
    else {
        for (auto& e : edges_by_kind[pattern.kind]) {
            unifyEdge(*e, pattern, valuation, cb);
        }
    }

}

void Hypergraph::unifyEdge(Edge& edge, Edge& pattern, 
                           Valuation& valuation, MatchCb cb) {

    int n = pattern.vertices.size();
    int buf[n];

    if (edge.kind == pattern.kind && edge.vertices.size() == n) {
        int i, j;
        for (i = j = 0; i < n; i++) {
            Vertex *u = pattern.vertices[i];
            Vertex *v = edge.vertices[i];
            if (u->id > 0) {
                if (u != v) break;
            }
            else {
                Vertex*& uu = valuation[~u->id];
                if (uu == NULL) {
                    buf[j++] = ~u->id;
                    uu = v;
                }
                else if (uu != v) break;
            }
        }
        if (i >= n) cb(valuation, edge.gen);
        /* restore valuation to previous value */
        while (j > 0) {
            valuation[buf[--j]] = NULL;
        }
    }
}

void Hypergraph::findSubgraph(std::vector<Edge>& pattern, int k,
                              int gen_max, MatchCb cb) {
    Valuation valuation(k);
    findSubgraph(pattern, gen_max, 0, 0, valuation, cb);
}

void Hypergraph::findSubgraph(std::vector<Edge>& pattern,
                              int gen_max, int gen_so_far, int index, 
                              Valuation& valuation,
                              MatchCb cb) {

    if (index >= pattern.size()) cb(valuation, gen_so_far);
    else {
        Edge& e = pattern[index];
        findEdge(e, gen_max, 0, valuation, [&] (Valuation& valuation, int gen) {
            int ngen = std::max(gen_so_far, gen);
            findSubgraph(pattern, gen_max, ngen, index + 1, valuation, cb);
        });
    }
}

void Hypergraph::merge(Vertex* u, Vertex* v) {
    while (u->merged) u = u->merged;
    while (v->merged) v = v->merged;
    if (u == v) return;

    dirty = true;

    if (v->id < u->id) std::swap(u, v);

    //std::cerr << "merge " << u->id << "~" << v->id << std::endl;

    for (auto& ie : v->edges) {
        ie.e->vertices[ie.index] = u;
        ie.e->gen = gen;
        u->edges.push_back(ie);
    }
    v->merged = u;
}

template <typename T>
bool slices_equal(const std::vector<T>& a1, const std::vector<T>& a2, int start) {
    if (a1.size() != a2.size()) return false;
    int n = a1.size();
    for (int i = start; i < n; i++) if (a1[i] != a2[i]) return false;
    return true;
}

void Hypergraph::compact(Vertex* u) {
    auto& edgeset = u->edges;
    for (int i = 0; i < edgeset.size(); ++i) {
        auto& e1 = edgeset[i];
        if (e1.index > 0) {
            for (int j = i + 1; j < edgeset.size(); ++j) {
                auto& e2 = edgeset[j];
                if (e2.index == e1.index && e2.e->kind == e1.e->kind && 
                     slices_equal(e2.e->vertices, e1.e->vertices, 1)) {
                    Vertex *v1 = e1.e->vertices[0],
                           *v2 = e2.e->vertices[0];
                    if (v1 == v2) removeEdge(e2.e);
                    else merge(v1, v2);
                }
            }
        }
    }
}

void Hypergraph::compact() {
    do {
        dirty = false;
        for (auto& u : vertices) {
            if (!u.merged)
                compact(&u);
        }
    } while(dirty);
}



void Hypergraph::fromText(std::istream& in) {

    std::vector<Hypergraph::Vertex*> vertices;
    std::string line;

    while (std::getline(in, line)) {
        if (line.substr(0, 2) == "//") continue;  // comment

        Hypergraph::Edge e;
        std::istringstream ss(line);
        std::string kind;
        ss >> kind;
        e.kind = Hypergraph::str2label(kind);
        while (ss.good()) {
            int vindex;
            ss >> vindex;
            if (vindex <= 0) throw std::runtime_error("graph format error");
            while (vertices.size() < vindex)
                vertices.push_back(addVertex());
            e.vertices.push_back(vertices[vindex - 1]);
        }
        if (e.vertices.size() > 0)
            addEdge(e);
        else
            break;
    }
}

void Hypergraph::toText(std::ostream& out) {

    for (auto& e : edges) {
        if (!e.removed) {
            out << label2str(e.kind);
            for (auto u : e.vertices) {
                out << " " << u->id;
            }
            out << std::endl;
        }
    }
}


Hypergraph::label_t Hypergraph::str2label(const std::string& s) {
    assert(s.size() <= sizeof(label_t));
    label_t acc = 0;
    for (auto c : s) { acc = (acc << 8) + c; }
    return acc;
}

std::string Hypergraph::label2str(label_t w) {
    std::string s;
    while (w) {
        s = char(w & 0xff) + s;
        w >>= 8;
    }
    return s;
}


Hypergraph::Vertex *Hypergraph::Vertex::rep() {
    auto v = this;
    while (v->merged) v = v->merged;
    return v;
}


class RewriteRule {
public:
    std::string name;
    Hypergraph premise;
    Hypergraph conclusion;

    typedef bool cmp_t;
    static const cmp_t EQ;
    static const cmp_t GEQ;

    RewriteRule() {}
    RewriteRule(const std::string& name) : name(name) {}

    void fromText(std::istream& in);
    void toText(std::ostream& out);

    void apply(Hypergraph& g, int gen_req = 0, cmp_t gen_cmp = GEQ);

    static void putHoles(Hypergraph& g);
};


const RewriteRule::cmp_t RewriteRule::EQ = false;
const RewriteRule::cmp_t RewriteRule::GEQ = true;


void RewriteRule::fromText(std::istream& in) {
    std::string title;
    std::getline(in, title);
    name = title;
    premise.fromText(in);
    putHoles(premise);
    conclusion.fromText(in);
    putHoles(conclusion);
}

void RewriteRule::toText(std::ostream& out) {
    premise.toText(out);
    conclusion.toText(out);
}

void RewriteRule::putHoles(Hypergraph& g) {
    for (auto& u : g.vertices) u.id = -u.id;
}


void RewriteRule::apply(Hypergraph& g, int gen_req, cmp_t gen_cmp) {

    static Hypergraph::label_t label_id = Hypergraph::str2label("id");

    int nholes = premise.vertices.size();
    int gen_max = gen_cmp ? INT32_MAX : gen_req;

    std::vector<Hypergraph::Edge> edges;

    g.findSubgraph(premise.edges, nholes, gen_max, [&] (Hypergraph::Valuation& valuation, int gen) {
        if (gen_cmp ? (gen < gen_req) : (gen != gen_req)) return;

        std::cout << "match [";
        for (auto u : valuation) {
            assert(u);
            std::cout << " " << u->id;
        }
        std::cout << "] " << name << std::endl;

        int k = valuation.size(), n = conclusion.vertices.size();
        Hypergraph::Valuation extras(std::max(0, n - k));

        for (auto& u : extras) u = g.addVertex();
        for (auto e : conclusion.edges) {
            for (auto& u : e.vertices) {
                int i = ~u->id;
                u = (i < k) ? valuation[i] : extras[i - k];
            }
            if (e.kind == label_id)
                g.merge(e.vertices[0], e.vertices[1]);
            else
                edges.push_back(e);
        }
    });

    for (auto& e : edges) g.addEdge(e);
}


class Reconstruct {
public:
    typedef std::function< void(const std::string& term) > TermCb;

    typedef Hypergraph::Vertex Vertex;

    std::set<Vertex*> terminals;
    std::set<Hypergraph::label_t> vocab;

    void addLeaves(Hypergraph& g, int upto);

    void mini(Hypergraph& g, Vertex* u, int depth, TermCb cb);

    std::string mkleaf(Vertex* u) {
        std::ostringstream ss;
        ss << "[" << u->id << "]";
        return ss.str();
    }
    
};


void Reconstruct::addLeaves(Hypergraph& g, int upto) {
    for (auto& u : g.vertices)
        if (u.id <= upto)
            terminals.insert(u.rep());
}


void Reconstruct::mini(Hypergraph& g, Vertex* u, int depth, TermCb cb) {

    bool any = false, atom = false;

    for (auto& e : u->edges) {
        if (e.index == 0) {
            if (vocab.find(e.e->kind) != vocab.end()) {
                auto op = e.e->kind;
                auto sop = Hypergraph::label2str(op);
                if (e.e->vertices.size() == 1) {
                    any = atom = true;
                    cb(sop);
                }
                else if (depth > 0) {
                    if (e.e->vertices.size() == 2) {
                        auto a1 = e.e->vertices[1];
                        mini(g, a1, depth - 1, [&] (const std::string& t1) {
                            any = true;
                            cb("(" + sop + " " + t1 + ")");
                        });
                    }
                    else if (e.e->vertices.size() == 3) {
                        auto a1 = e.e->vertices[1], a2 = e.e->vertices[2];
                        mini(g, a1, depth - 1, [&] (const std::string& t1) {
                            mini(g, a2, depth - 1, [&] (const std::string& t2) {
                                any = true;
                                cb("(" + t1 + " " + sop + " " + t2 + ")");
                            });
                        });
                    }
                    else if (e.e->vertices.size() == 4) {
                        auto a1 = e.e->vertices[1], a2 = e.e->vertices[2], a3 = e.e->vertices[3];
                        mini(g, a1, depth - 1, [&] (const std::string& t1) {
                            mini(g, a2, depth - 1, [&] (const std::string& t2) {
                                mini(g, a3, depth - 1, [&] (const std::string& t3) {
                                    any = true;
                                    cb("(" + sop + " " + t1 + " " + t2 + " " + t3 + ")");
                                });
                            });
                        });
                    }
                    else {
                        any = true;
                        cb("?" + sop + "?");
                    }
                }
            }
        }
    }

    if (terminals.find(u) != terminals.end() && !atom) {
        cb(mkleaf(u));
        any = true;
    }

    if (!any) {
        std::ostringstream ss;
        ss << "?" << u->id;
        cb(ss.str());
    }
}




class CaseSplit {
public:
    std::set<Hypergraph::Vertex*> anchored;

    Hypergraph::label_t splt;
    Hypergraph::label_t app;

    std::map<Hypergraph::Vertex*, std::vector<Hypergraph::Vertex*>> clones;

    CaseSplit() : 
        splt(Hypergraph::str2label("splt")), app(Hypergraph::str2label("@"))
    { }

    void split_all(Hypergraph& g);
    Hypergraph::Vertex* split_one(Hypergraph& g, Hypergraph::Vertex* p,
            Hypergraph::Vertex* x1,
            Hypergraph::Vertex* val1, Hypergraph::Vertex* val2);
};

void CaseSplit::split_all(Hypergraph& g) {
    // Acquire split edges
    auto splt_edges = g.edges_by_kind[splt];
    std::vector<Hypergraph::Edge> splt_edges_copy;

    for (auto e : splt_edges) {
        splt_edges_copy.push_back(*e);
        g.removeEdge(e);
    }

    // Identify anchored vertices
    for (auto& e : g.edges) {
        if (!e.removed && e.vertices.size() == 1)
            anchored.insert(e.vertices[0]);
    }

    // Apply splits and updates map of clones
    for (auto& e : splt_edges_copy) {
        Hypergraph::Vertex* p = e.vertices[1];
        Hypergraph::Vertex* x1 = e.vertices[2];
        Hypergraph::Vertex* val1 = e.vertices[3];
        Hypergraph::Vertex* val2 = e.vertices[4];

        auto& x1_clones = clones[x1];
        auto existing_clones = x1_clones; // copy

        x1_clones.push_back(split_one(g, p, x1, val1, val2));
        // - split (pre-existing) clones of x1 as well
        for (auto xm : existing_clones)
            x1_clones.push_back(split_one(g, p, xm, val1, val2));
    }
}


Hypergraph::Vertex* CaseSplit::split_one(Hypergraph& g, Hypergraph::Vertex* p,
        Hypergraph::Vertex* x1,
        Hypergraph::Vertex* val1, Hypergraph::Vertex* val2) {

    Hypergraph::Vertex* x2 = g.dupVertex(x1, anchored);

    g.addEdge({.kind = app,
                .vertices = std::vector<Hypergraph::Vertex*> { val1, p, x1 } });
    g.addEdge({.kind = app,
                .vertices = std::vector<Hypergraph::Vertex*> { val2, p, x2 } });

    return x2;
}



int main(int argc, char *argv[]) {

    std::string dataDir = (argc > 1) ? argv[1] : "data/concat-snoc";

    Hypergraph g;
    g.reserve(160000, 200000);
    std::ifstream fgraph(dataDir + "/input");
    g.fromText(fgraph);

    CaseSplit cs;
    cs.split_all(g);
    g.toText(std::cout);

    int initial_barrier = g.vertices.size();
    std::set<Hypergraph::label_t> initial_vocab;

    std::vector<RewriteRule> synth_rules;
    const int synth_depth = 2;

    std::vector<RewriteRule> simpl_rules;
    const int simpl_depth = 8;

    {
        std::ifstream frules(dataDir + "/vocab");

        while (!frules.eof()) {
            synth_rules.push_back(RewriteRule());
            synth_rules[synth_rules.size() - 1].fromText(frules);
        }
    }

    {
        std::ifstream frules(dataDir + "/rules");

        while (!frules.eof()) {
            simpl_rules.push_back(RewriteRule());
            simpl_rules[simpl_rules.size() - 1].fromText(frules);
        }
    }

    int gen = 0;
    for (int i = 0; i < synth_depth; i++) {
        g.gen++;
        for (auto& rule : synth_rules)
            rule.apply(g, gen, RewriteRule::EQ);
        gen = g.gen;
    }

    for (auto& e : g.edges_by_kind) initial_vocab.insert(e.first);

    gen = 0;
    for (int i = 0; i < simpl_depth; i++) {
        g.compact();
        g.gen++;
        for (auto& rule : simpl_rules)
            rule.apply(g, gen);
        gen = g.gen;
    }
    g.compact();

    g.toText(std::cout);

#if 0
    {
        Reconstruct r;
        r.vocab.insert(initial_vocab.begin(), initial_vocab.end());
        r.addLeaves(g, initial_barrier);

        for (auto& u : g.vertices) {
            if (!u.merged) {
                std::cout << u.id;
                r.mini(g, &u, 2, [] (const std::string& term) {
                    std::cout << "  " << term;
                });
                std::cout << std::endl;
            }
        }
    }
#endif

    {
        Reconstruct r;
        r.vocab.insert(initial_vocab.begin(), initial_vocab.end());
        r.addLeaves(g, initial_barrier);

        Hypergraph::label_t skel = Hypergraph::str2label("skel");

        std::map<Hypergraph::Vertex*, std::vector<Hypergraph::Vertex*>> skels;

        for (auto e : g.edges_by_kind[skel]) {
            e->vertices[1]->scratch.z = 1;
        }

        for (auto e : g.edges_by_kind[skel]) {
            auto u = e->vertices[1], v = e->vertices[2];
            //(u->scratch.n += (u->scratch.n == 0)) *= v->id;
            u->scratch.z *= (v->id | 0x1);
            skels[v].push_back(u);
        }

#if 0
        for (auto& u : g.vertices) {
            auto it = skels.find(&u);
            if (it != skels.end()) {
                std::cout << u.id;
                for (auto v : it->second) {
                    /*
                    r.mini(g, v, 3, [] (const std::string& term) {
                        std::cout << "  " << term;
                    });*/
                    std::string min_term;
                    r.mini(g, v, 3, [&] (const std::string& term) {
                        if (min_term.size() == 0 || term.size() < min_term.size())
                            min_term = term;
                    });
                    std::cout << "  " << min_term;
                }
                std::cout << std::endl;
            }
        }
#endif

        std::cout << "------" << std::endl;

        std::map<int64_t, std::set<Hypergraph::Vertex*>> skel_by_occ;
        for (auto e : g.edges_by_kind[skel]) {
            auto u = e->vertices[1];
            skel_by_occ[u->scratch.z].insert(u);
        }

        for (auto it : skel_by_occ) {
            if (it.second.size() > 1) {
                std::cout << std::hex << it.first << std::dec
                          << "  |" << it.second.size() << "| ";
                for (auto u : it.second) {
                    std::string min_term;
                    r.mini(g, u, 3, [&] (const std::string& term) {
                        if (min_term.size() == 0 || term.size() < min_term.size())
                            min_term = term;
                    });
                    std::cout << "  " << min_term;
                }
                std::cout << std::endl;
            }
        }
    }
    
    return 0;
}