const fs = require('fs');


txt = fs.readFileSync('scala-logging.log', 'utf-8');

function getParts() {
    parts = {}; sp = txt.split(/(file name) - (.*)/g);
    for (let i in sp) {
        i = +i;
        if (sp[i] == 'file name' && sp[i+2]) {
            parts[sp[i+1]] = parsePart(sp[i+2]);
        }
    }
    return parts;
}

function parsePart(txt) {
    return {
        timings: [...txt.matchAll(/(?:^| - )(.*) @ .* \[\+(\d+)\]/g)],
        proofs: [...txt.matchAll(/(\d+:\d+:\d+) .* (Trying to prove|Found inductive|Proof Failed|Searching for rules that have become provable|Filtering terms|Finished finding rules)/g)],
        lemmas: [...txt.matchAll(/Done searching for rules:([^]*?)\n.*Done SPBE/g)].map(x => x[1]),
        stats: txt.match(/term count: (\d+)\n.*failed count: (\d+)\n.*retry success count: (\d+)/),
        txt
    };
}

function summarizeTimings(part) {
    ct = 0; times = {};
    for (let [,lbl,tm] of part.timings) {
        tm = +tm; lbl = lbl.replace(/\d/g, 'X');
        elapsed = tm - ct;
        ct = tm;
        times[lbl] = (times[lbl] || 0) + elapsed;
    }
    var proofsTime = 0, proofStart = undefined;
    for (let [,tm,what] of part.proofs) {
        if (what == 'Trying to prove') proofStart = tm;
        else if (proofStart) {
            proofsTime += logTimeDiff(tm, proofStart);
            proofStart = undefined;
        }
    }
    var searchingTime = 0, searchingStart = undefined;
    for (let [,tm,what] of part.proofs) {
        if (what == 'Searching for rules that have become provable') searchingStart = tm;
        else if (searchingStart) {
            searchingTime += logTimeDiff(tm, searchingStart);
            searchingStart = undefined;
        }
    }    
    times['Proofs'] = proofsTime;
    times['Second SOE'] = searchingTime;
    return times;
}

function logTimeDiff(tm1, tm2) {
    pfx = '1970-01-01T';
    return Date.parse(pfx+tm1) - Date.parse(pfx+tm2);
}


function summarizeLemmas(part) {
    return [].concat(...part.lemmas.map(x =>
        x.split(/\n.*?-/).filter(x => x).map(prettify)));
}

function summarizeStats(part) {
    return {terms: +part.stats[1], failed: +part.stats[2],
        retrySuccess: +part.stats[3]};
}

const NAMES = {int: ['x', 'y', 'z'],
    nat: ['x', 'y', 'z'],
    'list(int)': ['l_1', 'l_2', 'l_3'],
    'list(boolean)': ['l_1', 'l_2', 'l_3'],
    ':>(int, boolean)': ['p', 'q', 'r'],
    ':>(int, int)': ['f', 'g', 'h'],
    ':>(int, int, int)': ['f', 'g', 'h'],
    'func': ['f', 'g', 'h'],
    'tree(int)': ['t_1', 't_2', 't_3'],
    'boolean': ['b', 'b_0', 'b_1']
};

const FUNCS = {len: '\\tlen', snoc: '\\snoc', 
    reverse: '\\trev', map: '\\tmap', filter: '\\tfilter', 
    fold: '\\tfold', concat: '\\concat', tmap: '\\treemap', 
    cons: '\\cons', or: '\\lor', and: '\\land', 
    '⊥': '\\tfalse', '⊤': '\\ttrue', 
    forall: '\\tforall', exists: '\\texists', 
    plus: '+', pl: '+', zero: '0', one: '1', suc: '\\tsucc'
};

function prettify(s) {
    return s.replace(/[?]Placeholder_(\d)_type_\{(.*?)\}/g, 
        (_, idx, tpe) => (NAMES[tpe] || ['?','?','?'])[idx])
            .replace(/\b([a-z]{2,})\b/g, (x) => FUNCS[x] || x);
}

function summarize(parts) {
    sparts = {};
    for (let [k, part] of Object.entries(parts)) {
        var spart = {
            timings: summarizeTimings(part),
            lemmas: summarizeLemmas(part),
            stats: summarizeStats(part)
        };
        spart.timings = {
            'Phases 1+2': (spart.timings["Finished symbolic term evaluation depth X "] || 0) +
                          (spart.timings["Finished term creation depth X "] || 0) +
                          (spart.timings["Second SOE"] || 0),
            'Phase 3': (spart.timings["Finished finding rules depth X "] || 0) +
                       (spart.timings["Finished finding rules repeat X depth X "] || 0) +
                       (spart.timings["Retrying failed depth X "] || 0) -
                       (spart.timings["Proofs"] || 0) -
                       (spart.timings["Second SOE"] || 0),
            'Phase 4': (spart.timings["Proofs"] || 0),
            'Retries': (spart.timings["Finished refailed depth X "] || 0)
            //...spart.timings
        };
        sparts[k] = spart;
    }
    return sparts;
}

const EXPE = {
'0 \\tsucc +':                                     ['NatSuc'],     
'[] \\cons \\concat \\snoc':                       ['SnocConcat'],
'\\trev \\cons \\snoc':                            ['ReverseReverse'],
'\\trev \\snoc \\concat \\cons':                   ['ReverseSnocConcat'],
'\\tfilter':                                       ['FilterPFilterQ'],  
'\\tfilter \\concat':                              ['FilterConcat'],    
'0 \\tsum \\tfold +':                              ['FoldSum', 'FoldSumFast'],
'\\ttrue \\tforall \\land \\tfold':                ['FoldForall', 'FoldForallFast'],
'\\tfalse \\texists \\lor \\tfold':                ['FoldExists', 'FoldExistsFast'],
'\\tmap \\circ':                                   ['MapCirc'],
'0 1 + \\trev \\snoc \\tlen':                      ['ReverseLen'],
'+ \\concat \\tlen':                               ['ConcatLen'],
'\\trev \\tswitch \\ttreeflat \\ttreemap \\tid':   ['TreeMapRev'],
'\\tmap \\cons \\snoc \\concat \\tfold \\trev':    ['ListAll']
};

function secs(ms) { return rjust((ms / 1000).toFixed(2)); }
function secs1(ms) { return rjust(ms ? (ms / 1000).toFixed(0) : '<1'); }

function ljust(s,k=44) { while (s.length < k) s += ' '; return s; }
function rjust(s,k=10) { while (s.length < k) s = ' ' + s; return s; }

function math(s) { return `$${s.replace(/ /g, '~')}$`; }
function num(n) { return rjust('' + n, 4); }

function makeTable2(out) {
    for (let [k,v] of Object.entries(EXPE)) {
        var fst = true;
        for (let bench of v) {
            var part = out[`RunSpbe${bench}.tc`];
                tmg = part.timings, lms = part.lemmas, st = part.stats;
            console.log(`${ljust(fst ? math(k) : '')}  &    ${secs(tmg['Phases 1+2'])}  &  ` +
                ` ${secs(tmg['Phase 3']) }     &  ` +
                ` ${secs1(tmg['Phase 4'])}     &  ` +
                ` ${secs(tmg['Retries']) }     &  ` +
                ` ${num(fst ? lms.length : '')} & ` +
                ` ${num(st.failed)} &  ` +
                ` ${num(st.retrySuccess)}  \\\\`);
            fst = false;
        }
        console.log('\\hline');
    }
}

function makeTable1(out){
    for (let [k,[bench]] of Object.entries(EXPE)) {
        console.log(math(k));
        var part = out[`RunSpbe${bench}.tc`].lemmas;
        for (let lemma of part)
            console.log(lemma);
    }
}

var out = summarize(getParts());
makeTable2(out);
makeTable1(out);

