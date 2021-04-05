ace.define("ace/mode/dtt_highlighting_rules", ["require", "exports", "module", "ace/lib/oop", "ace/mode/text_highlight_rules"], function (require, exports, module) {
  "use strict";
  var oop = require("../lib/oop");
  var TextHighlightRules = require("./text_highlight_rules").TextHighlightRules;

  var DttHighlightRules = function () {
      
      this.$rules = {
          "start": [
              {
                  token: "keyword",
                  regex: "def|theorem|lemma|axiom|constant|check|eval|assume|have|from|show"
              },
              {
                  token: "keyword.other",
                  regex: "fun|λ|lambda|Pi|Π|∏|∀|forall|∃|exists|fst|snd|inl|inr|->|→|\*|⨯|∧|/\\|+|∨|\\/|Type|Prop|Kind"
              },
              {
                  token: "keyword.operator",
                  regex: "match|with|end|let|in|:|:=|=>|⇒|\|"
              }
          ],
          "comments": [
            {
              token: 'punctuation.definition.comment',
              regex: '--.*',
              push_: [
                {
                  token: 'comment.line.double-dash',
                  regex: '$',
                  next: 'pop'
                },
                {
                  defaultToken: 'comment.line.double-dash'
                }
              ]
            }
          ]
      };

      this.normalizeRules();
  };

  oop.inherits(DttHighlightRules, TextHighlightRules);
  exports.DttHighlightRules = DttHighlightRules;
});

ace.define("ace/mode/dtt", ["require", "exports", "module", "ace/lib/oop", "ace/mode/text", "ace/mode/dtt_highlighting_rules"], function (require, exports, module) {
  "use strict";

  var oop = require("../lib/oop");
  var TextMode = require("./text").Mode;
  var DttHighlightRules = require("./dtt_highlighting_rules").DttHighlightRules;

  var Mode = function () {
      this.HighlightRules = DttHighlightRules;
      this.$behaviour = this.$defaultBehaviour;
  };
  oop.inherits(Mode, TextMode);

  (function () {

      this.lineCommentStart = "#";

      this.$id = "ace/mode/";
      this.snippetFileId = "ace/snippets/dtt";
  }).call(Mode.prototype);

  exports.Mode = Mode;

}); (function () {
  ace.require(["ace/mode/dtt"], function (m) {
      if (typeof module == "object" && typeof exports == "object" && module) {
          module.exports = m;
      }
  });
})();