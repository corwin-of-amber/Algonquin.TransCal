#include "inc/hypergraph.h"


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
        for (auto& rule : rw_rules)
            rule.apply(g, gen);
        gen = g.gen;
    }
    g.compact();

    g.toText(std::cout);
    return 0;
}