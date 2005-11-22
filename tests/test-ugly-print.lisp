;;;; test-ugly-print.lisp
;;;
;;; Tests for the ugly printer

(in-package :jwacs-tests)

;;;; Helper functions
(defun ugly-string (elm)
  "Uglyprint LM to a tring value instead of a stream"
  (with-output-to-string (s)
    (ugly-print elm s)))

(defmacro with-fresh-genvar (&body body)
  "Make sure that GENVAR variable names will start from 0 and that
   continuation arguments will have a known value"
  `(let* ((*genvar-counter* 0))
    ,@body))

;;;; Test category
(defnote ugly-print "tests for the ugly printer")

(deftest ugly-print/var-decl/1 :notes ugly-print
  (with-fresh-genvar
      (ugly-string (parse "var x = 3;")))
    "var JW$0=3;")

(deftest ugly-print/function-decl/1 :notes ugly-print
  (with-fresh-genvar
    (ugly-string (parse "function FOO(){;}")))
  "function JW$0(){;};")

(deftest ugly-print/function-decl/2 :notes ugly-print
  (with-fresh-genvar
    (ugly-string (parse "function FOO(x){;}")))
  "function JW$0(JW$1){;};")

(deftest ugly-print/function-decl/3 :notes ugly-print
  (with-fresh-genvar
    (ugly-string (parse "function FOO(x){ var y = x; }")))
  "function JW$0(JW$1){var JW$2=JW$1;};")

(deftest ugly-print/function-decl/4 :notes ugly-print
  (with-fresh-genvar
    (ugly-string (parse "function FOO(){ FOO(); }")))
  "function JW$0(){JW$0();};")

(deftest ugly-print/function-decl-arg-shadow/1 :notes ugly-print
  (with-fresh-genvar
    (ugly-string (parse "function FOO(x){ var x = 3; }")))
  "function JW$0(JW$1){var JW$2=3;};")
    

(deftest ugly-print/function-decl-arg-shadow/2 :notes ugly-print
  (with-fresh-genvar
    (ugly-string (parse "function FOO(x){ var x = 3; FOO(x);}")))
  "function JW$0(JW$1){var JW$2=3;JW$0(JW$2);};")

(deftest ugly-print/function-in-function/1 :notes ugly-print
  (with-fresh-genvar
    (ugly-string (parse "function FOO(x) {
                          function BAR(z) {
                             return z + y;
                          }
                          var y = 3;
                          bar(3); 
                         }")))
  "function JW$0(JW$1){function JW$2(JW$3){return JW$3+JW4;};var JW$4=3;JW$2(3);};")

(deftest ugly-print/function-in-function/2 :notes ugly-print
  (with-fresh-genvar
    (ugly-string (parse "function FOO(x) {
                          var y = 3;
                          function BAR(z) {
                             return z + y;
                          }
                          bar(3); 
                         }")))
  "function JW$0(JW$2){var JW$3=3;function JW$1(JW$4){return JW$4+JW$3;};JW$1(3);};")


;;; TESTS:
;;; ensure vardecls in blocks shadow function vars
;;;
;;;    function foo(x) <-- this x could be JW$0
;;;    {
;;;        var x = 3;  <-- this x should be JW$1 not 0
;;;        bar(x);     <-- this x should be JW$1
;;;    }
;;;