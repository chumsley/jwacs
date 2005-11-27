;;;; test-lexer.lisp
;;;
;;; Unit tests for the Javascript lexer.
(in-package :jwacs-tests)

;;;; Helper functions
(defun non-null (x)
  "Return T if X is non-null.  This is a convenience function that
   frees us from the necessity of having to know exactly which non-null
   value a test should expect."
  (not (null x)))

;;;; Test categories
(defnote lexer "tests for the lexer")
(defnote regexp "tests for individual regular expressions")

;;;; Tests
(deftest regexp-re/1 :notes (lexer regexp)
  (non-null (scan regexp-re "/hello/"))
  t)

(deftest regexp-re/2 :notes (lexer regexp)
  (non-null (scan regexp-re "/.\\n/"))
  t)

(deftest regexp-re/3 :notes (lexer regexp)
  (non-null (scan regexp-re "/(this)/g"))
  t)

(deftest regexp-re/4 :notes (lexer regexp)
  (non-null (scan regexp-re "/(this)/gi"))
  t)

(deftest regexp-re/5 :notes (lexer regexp)
  (scan regexp-re "\"hi\"")
  nil)

(deftest regexp-re/6 :notes (lexer regexp)
  (scan regexp-re "/\"hi\"")
  nil)

(defun read-all-tokens (js-string)
  "Return a list of cons cells representing the tokens
   of JS-STRING.  The CAR of each cell is the type of
   token, and the CDR is the source text."
  (loop with l = (make-javascript-lexer js-string)
        for x = (multiple-value-list (funcall l))
        while (not (eq (car x) eoi))
        collect x))

(deftest lexer/1 :notes lexer
  (read-all-tokens
   "/* test string */
    function (f)
    {
      // Ignore this stuff
      var m = 010;
      doStuff('stuff', \"nonsense\", 0xff, 45.0, f(m));
    }")
  ((:function "function")
   (:left-paren "(")
   (:identifier "f")
   (:right-paren ")")
   (:left-curly "{")
   (:var "var")
   (:identifier "m")
   (:equals "=")
   (:number 8)
   (:semicolon ";")
   (:identifier "doStuff")
   (:left-paren "(")
   (:string-literal "stuff")
   (:comma ",")
   (:string-literal "nonsense")
   (:comma ",")
   (:number 255)
   (:comma ",")
   (:number 45.0)
   (:comma ",")
   (:identifier "f")
   (:left-paren "(")
   (:identifier "m")
   (:right-paren ")")
   (:right-paren ")")
   (:semicolon ";")
   (:right-curly "}")))
    
(deftest lexer/2 :notes lexer
  (read-all-tokens
   "var re1 = /hello/g;
    var re2 = /hello\\/goodbye/ig;")
  ((:var "var")
   (:identifier "re1")
   (:equals "=")
   (:re-literal ("hello" . "g"))
   (:semicolon ";")
   (:var "var")
   (:identifier "re2")
   (:equals "=")
   (:re-literal ("hello/goodbye" . "ig"))
   (:semicolon ";")))

(deftest lexer/3 :notes lexer
  (read-all-tokens
   "x >= 10, y<=20, <=>=<>")
  ((:identifier "x")
   (:greater-than-equals ">=")
   (:number 10)
   (:comma ",")
   (:identifier "y")
   (:less-than-equals "<=")
   (:number 20)
   (:comma ",")
   (:less-than-equals "<=")
   (:greater-than-equals ">=")
   (:less-than "<")
   (:greater-than ">")))