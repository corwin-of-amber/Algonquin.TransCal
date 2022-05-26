#include "inc/hypergraph.h"


int main(int argc, char *argv[]) {

    std::string dataDir = (argc > 1) ? argv[1] : "data/concat-snoc";
    bool opt_diagonal = false;

    int opt_synth_height = 2;
    int opt_simpl_depth = 8;

    for (int i = 1; i < argc; i++) {
        std::string opt = argv[i];
        if (opt == "+diag") opt_diagonal = true;
        if (opt.rfind("--height=", 0) == 0)
            opt_synth_height = std::stoi(opt.substr(9));
        if (opt.rfind("--depth=", 0) == 0)
            opt_simpl_depth = std::stoi(opt.substr(8));
    }

    /* Rewrite frenzy */

    Hypergraph g;
    g.reserve(16000000, 20000000);
    std::ifstream fgraph(dataDir + "/input");
    g.fromText(fgraph);

    CaseSplit cs;
    cs.split_all(g);
    g.toText(std::cout);

    int initial_barrier = g.vertices.size();
    std::set<Hypergraph::label_t> initial_vocab;

    std::vector<RewriteRule> synth_rules;
    const int synth_height = opt_synth_height;

    std::vector<RewriteRule> simpl_rules;
    const int simpl_depth = opt_simpl_depth;

    {  /* sygus rules */
        std::ifstream frules(dataDir + "/vocab");

        while (!frules.eof()) {
            synth_rules.push_back(RewriteRule());
            synth_rules[synth_rules.size() - 1].fromText(frules);
        }
    }

    {  /* merge rules */
        std::ifstream frules(dataDir + "/rules");

        while (!frules.eof()) {
            simpl_rules.push_back(RewriteRule());
            simpl_rules[simpl_rules.size() - 1].fromText(frules);
        }
    }

    /* sygus phase */
    int gen = 0;
    for (int i = 0; i < synth_height; i++) {
        g.gen++;
        for (auto& rule : synth_rules)
            rule.apply(g, gen, RewriteRule::EQ);
        gen = g.gen;
    }

    std::cout << "-------------------" << std::endl;

    for (auto& e : g.edges_by_kind) initial_vocab.insert(e.first);

    initial_vocab.insert(Hypergraph::str2label("<\\>"));

    /* merge phase */
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

#ifdef LOG_INSANE
    /* Print all terms in equivalence classes (massive amounts of output) */
    {
        Reconstruct r(g);
        r.vocab.insert(initial_vocab.begin(), initial_vocab.end());
        r.addLeaves(initial_barrier);

        for (auto& u : g.vertices) {
            if (!u.merged) {
                std::cout << u.id;
                r.mini(&u, 2, [] (const std::string& term) {
                    std::cout << "  " << term;
                });
                std::cout << std::endl;
            }
        }
    }
#endif

    {
        Reconstruct r(g);
        r.vocab.insert(initial_vocab.begin(), initial_vocab.end());
        r.addLeaves(initial_barrier);

        Hypergraph::label_t skel = Hypergraph::str2label("skel");

        std::map<Hypergraph::Vertex*, std::vector<Hypergraph::Vertex*>> skels;

        for (auto e : g.edges_by_kind[skel]) {
            auto skel = e->vertices[1], u = e->vertices[2];
            skel->scratch.z = 1;
            if (opt_diagonal)
                coordinate_heuristic(r, u, skel, 3);
        }

        for (auto e : g.edges_by_kind[skel]) {
            auto skel = e->vertices[1], u = e->vertices[2];
            skel->scratch.z *= (u->id | 0x1001);
            skels[u].push_back(skel);
        }

#ifdef LOG_VERBOSE
        /* Print summary of equivalence classes (skeletons only) */
        for (auto& u : g.vertices) {
            auto it = skels.find(&u);
            if (it != skels.end()) {
                std::cout << u.id;
                for (auto v : it->second) {
                    /*
                    r.mini(g, v, 3, [] (const std::string& term) {
                        std::cout << "  " << term;
                    });*/
                    std::cout << "  " << r.minimal(v, 3);
                }
                std::cout << std::endl;
            }
        }
#endif

        std::cout << "|V|=" << g.vertices.size() << ", |E|=" << g.edges.size() << std::endl;
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
                    std::cout << "  " << r.minimal(u, 3);
                }
                std::cout << std::endl;
            }
        }
    }
    
    return 0;
}