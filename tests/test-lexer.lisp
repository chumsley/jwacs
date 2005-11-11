;;;; test-lexer.lisp
;;;
;;; Unit tests for the Javascript lexer.
(in-package :jwacs-tests)

(defun non-null (x)
  "Return T if X is non-null.  This is a convenience function that
   frees us from the necessity of having to know exactly which non-null
   value a test should expect."
  (not (null x)))

(deftest regexp-re/1 :note (lexer regexp)
  (non-null (scan jwacs::regexp-re "/hello/"))
  t)

(deftest regexp-re/2 :note (lexer regexp)
  (non-null (scan jwacs::regexp-re "/.\\n/"))
  t)

(deftest regexp-re/3 :note (lexer regexp)
  (non-null (scan jwacs::regexp-re "/(this)/g"))
  t)

(deftest regexp-re/4 :note (lexer regexp)
  (non-null (scan jwacs::regexp-re "/(this)/gi"))
  t)

(deftest regexp-re/5 :note (lexer regexp)
  (scan jwacs::regexp-re "\"hi\"")
  nil)

(deftest regexp-re/6 :note (lexer regexp)
  (scan jwacs::regexp-re "/\"hi\"")
  nil)


(deftest lexer/1 :note lexer
  (let ((js-string-1 "/* test string */
                      function (f)
                      {
                        // Ignore this stuff
                        var m = 010;
                        doStuff('stuff', \"nonsense\", 0xff, 45.0, f(m));
                      }"))
    (loop with l = (jwacs::make-javascript-lexer js-string-1)
          for x = (multiple-value-list (funcall l))
          while (not (eq (car x) :eoi))
          collect x))
  ((:FUNCTION "function")
   (:LEFT-PAREN "(")
   (:IDENTIFIER "f")
   (:RIGHT-PAREN ")")
   (:LEFT-CURLY "{")
   (:VAR "var")
   (:IDENTIFIER "m")
   (:EQUALS "=")
   (:NUMBER 8)
   (:SEMICOLON ";")
   (:IDENTIFIER "doStuff")
   (:LEFT-PAREN "(")
   (:STRING-LITERAL "stuff")
   (:COMMA ",")
   (:STRING-LITERAL "nonsense")
   (:COMMA ",")
   (:NUMBER 255)
   (:COMMA ",")
   (:NUMBER 45.0)
   (:COMMA ",")
   (:IDENTIFIER "f")
   (:LEFT-PAREN "(")
   (:IDENTIFIER "m")
   (:RIGHT-PAREN ")")
   (:RIGHT-PAREN ")")
   (:SEMICOLON ";")
   (:RIGHT-CURLY "}")))
    
(deftest lexer/2 :note lexer
  (let ((js-string-2 "var re1 = /hello/g;
                     var re2 = /hello\\/goodbye/ig;"))
    (loop with l = (jwacs::make-javascript-lexer js-string-2)
          for x = (multiple-value-list (funcall l))
          while (not (eq (car x) :eoi))
          collect x))
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
