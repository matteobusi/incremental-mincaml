// Generated automatically by nearley, version 2.15.1
// http://github.com/Hardmath123/nearley
(function () {
function id(x) { return x[0]; }
var grammar = {
    Lexer: undefined,
    ParserRules: [
    {"name": "e$string$1", "symbols": [{"literal":"i"}, {"literal":"f"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "e$string$2", "symbols": [{"literal":"t"}, {"literal":"h"}, {"literal":"e"}, {"literal":"n"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "e$string$3", "symbols": [{"literal":"e"}, {"literal":"l"}, {"literal":"s"}, {"literal":"e"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "e", "symbols": ["e$string$1", "_", "bexp", "_", "e$string$2", "_", {"literal":"("}, "e", {"literal":")"}, "_", "e$string$3", "_", {"literal":"("}, "e", {"literal":")"}]},
    {"name": "e$string$4", "symbols": [{"literal":"l"}, {"literal":"e"}, {"literal":"t"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "e$string$5", "symbols": [{"literal":"i"}, {"literal":"d"}, {"literal":"_"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "e$string$6", "symbols": [{"literal":"i"}, {"literal":"n"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "e", "symbols": ["e$string$4", "_", "e$string$5", "id", "_", {"literal":"="}, "_", {"literal":"("}, "e", {"literal":")"}, "_", "e$string$6", "_", {"literal":"("}, "e", {"literal":")"}]},
    {"name": "e", "symbols": [{"literal":"("}, "fid", "_", "nexp", {"literal":")"}]},
    {"name": "e", "symbols": ["nexp"]},
    {"name": "id", "symbols": [/[a-zA-Z]/]},
    {"name": "bval$string$1", "symbols": [{"literal":"f"}, {"literal":"a"}, {"literal":"l"}, {"literal":"s"}, {"literal":"e"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "bval", "symbols": ["bval$string$1"]},
    {"name": "bval$string$2", "symbols": [{"literal":"t"}, {"literal":"r"}, {"literal":"u"}, {"literal":"e"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "bval", "symbols": ["bval$string$2"]},
    {"name": "bval", "symbols": ["bid"]},
    {"name": "digit", "symbols": [/[0-9]/]},
    {"name": "num", "symbols": ["digit"]},
    {"name": "num", "symbols": ["nid"]},
    {"name": "bexp", "symbols": ["bval"]},
    {"name": "bexp", "symbols": [{"literal":"("}, "nexp", "_", "cop", "_", "nexp", {"literal":")"}]},
    {"name": "bexp$string$1", "symbols": [{"literal":"n"}, {"literal":"o"}, {"literal":"t"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "bexp", "symbols": [{"literal":"("}, "bexp$string$1", "_", "bexp", {"literal":")"}]},
    {"name": "cop", "symbols": [{"literal":"="}]},
    {"name": "cop$string$1", "symbols": [{"literal":"<"}, {"literal":">"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "cop", "symbols": ["cop$string$1"]},
    {"name": "cop", "symbols": [{"literal":"<"}]},
    {"name": "cop", "symbols": [{"literal":">"}]},
    {"name": "cop$string$2", "symbols": [{"literal":"<"}, {"literal":"="}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "cop", "symbols": ["cop$string$2"]},
    {"name": "cop$string$3", "symbols": [{"literal":">"}, {"literal":"="}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "cop", "symbols": ["cop$string$3"]},
    {"name": "nexp", "symbols": ["num"]},
    {"name": "nexp", "symbols": [{"literal":"("}, "nexp", "_", "ibop", "_", "nexp", {"literal":")"}]},
    {"name": "ibop", "symbols": [{"literal":"+"}]},
    {"name": "ibop", "symbols": [{"literal":"-"}]},
    {"name": "ibop", "symbols": [{"literal":"*"}]},
    {"name": "_", "symbols": [/[ ]/]},
    {"name": "bid", "symbols": [{"literal":"b"}, "digit", "digit"]},
    {"name": "nid", "symbols": [{"literal":"n"}, "digit", "digit"]},
    {"name": "fid", "symbols": [{"literal":"f"}, "digit", "digit"]}
]
  , ParserStart: "e"
}
if (typeof module !== 'undefined'&& typeof module.exports !== 'undefined') {
   module.exports = grammar;
} else {
   window.grammar = grammar;
}
})();
