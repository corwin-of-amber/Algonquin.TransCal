#include "inc/hypergraph.h"


class ColorIndex {
public:
    static void locateAll(Hypergraph& g);
    static void merge(Hypergraph::Edge* e1, Hypergraph::Edge* e2);
};


void ColorIndex::locateAll(Hypergraph& g) {
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

void ColorIndex::merge(Hypergraph::Edge* e1, Hypergraph::Edge* e2) {
    for (auto it = e2->vertices.begin() + 1; it != e2->vertices.end(); it++) {
        if (!e1->containsSource(*it)) {
            e1->vertices.push_back(*it);
            (*it)->color_index = e1;
        }
    }
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
    for (int i = 0; i < rw_depth; i++) {
        g.compact();
        g.gen++;
        ColorIndex::locateAll(g);
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