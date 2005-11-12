;;;; test-pretty-print.lisp
;;;
;;; Tests for the pretty-printer.

(in-package :jwacs-tests)
(import '(jwacs::make-source-element
          jwacs::make-special-value
          jwacs::make-identifier
          jwacs::make-numeric-literal
          jwacs::make-string-literal
          jwacs::make-array-literal
          jwacs::make-object-literal
          jwacs::make-new-expr
          jwacs::make-fn-call
          jwacs::make-property-access
          jwacs::make-unary-operator
          jwacs::make-binary-operator
          jwacs::make-conditional
          jwacs::make-comma-expr
          jwacs::make-var-decl-stmt
          jwacs::make-var-decl
          jwacs::make-block
          jwacs::make-if
          jwacs::make-do
          jwacs::make-while
          jwacs::make-for
          jwacs::make-for-in
          jwacs::make-continue
          jwacs::make-break
          jwacs::make-return
          jwacs::make-with
          jwacs::make-switch
          jwacs::make-case
          jwacs::make-default
          jwacs::make-label
          jwacs::make-throw
          jwacs::make-try
          jwacs::make-catch-clause
          jwacs::make-finally-clause
          jwacs::make-function-decl
          jwacs::make-function-expression

          jwacs::pretty-print
          jwacs::with-indent
          jwacs::*indent*))

;;;; Helper functions
(defun pretty-string (elm)
  "Pretty-print ELM to a string value instead of a stream."
  (with-output-to-string (s)
    (pretty-print elm s)))

;;;; Test categories
(defnote pretty-print "tests for the pretty-printer")

;;; Helpful constants
(defconstant foo-id (make-identifier :name "foo"))
(defconstant bar-id (make-identifier :name "bar"))
(defconstant baz-id (make-identifier :name "baz"))

;;;; Tests
(deftest pretty-print-special-value/1 :notes pretty-print
  (pretty-string (make-special-value :symbol :this))
  "this")

(deftest pretty-print-special-value/2 :notes pretty-print
  (pretty-string (make-special-value :symbol :true))
  "true")

(deftest pretty-print-special-value/3 :notes pretty-print
       (pretty-string (make-special-value :symbol :false))
       "false")

(deftest pretty-print-special-value/4 :notes pretty-print
  (pretty-string (make-special-value :symbol :null))
  "null")

(deftest pretty-print-special-value/5 :notes pretty-print
  (pretty-string (make-special-value :symbol :undefined))
  "undefined")

(deftest pretty-print-unary-operator/1 :notes pretty-print
  (pretty-string (make-unary-operator :op-symbol :void :arg foo-id))
  "void foo")

(deftest pretty-print-unary-operator/2 :notes pretty-print
  (pretty-string (make-unary-operator :op-symbol :delete :arg foo-id))
  "delete foo")

(deftest pretty-print-unary-operator/3 :notes pretty-print
  (pretty-string (make-unary-operator :op-symbol :post-incr :arg foo-id))
  "foo++")

(deftest pretty-print-unary-operator/4 :notes pretty-print
  (pretty-string (make-unary-operator :op-symbol :post-decr :arg foo-id))
  "foo--")

(deftest pretty-print-unary-operator/5 :notes pretty-print
  (pretty-string (make-unary-operator :op-symbol :pre-incr :arg foo-id))
  "++foo")

(deftest pretty-print-unary-operator/6 :notes pretty-print
  (pretty-string (make-unary-operator :op-symbol :pre-decr :arg foo-id))
  "--foo")

(deftest pretty-print-unary-operator/7 :notes pretty-print
  (pretty-string (make-unary-operator :op-symbol :unary-plus :arg foo-id))
  "+foo")

(deftest pretty-print-unary-operator/8 :notes pretty-print
  (pretty-string (make-unary-operator :op-symbol :unary-minus :arg foo-id))
  "-foo")

(deftest pretty-print-binary-operator/1 :notes pretty-print
  (pretty-string (make-binary-operator :op-symbol :add :left-arg foo-id :right-arg bar-id))
  "foo + bar")

(deftest pretty-print-binary-operator/2 :notes pretty-print
  (pretty-string (make-binary-operator :op-symbol :lshift :left-arg foo-id :right-arg bar-id))
  "foo << bar")

(deftest pretty-print-binary-operator/3 :notes pretty-print
  (pretty-string (make-binary-operator :op-symbol :instanceof :left-arg foo-id :right-arg bar-id))
  "foo instanceof bar")

(deftest pretty-print-conditional/1 :notes pretty-print
  (pretty-string
   (make-conditional :condition foo-id
                     :true-arg bar-id
                     :false-arg (make-special-value :symbol :null)))
  "(foo) ? (bar) : (null)")

(deftest pretty-print-numeric-literal/1 :notes pretty-print
  (pretty-string (make-numeric-literal :value 50))
  "50")

(deftest pretty-print-var-decl-stmt/1 :notes pretty-print
  (pretty-string
   (make-var-decl-stmt :var-decls 
                       (list (make-var-decl :name "x")
                             (make-var-decl :name "y" :initializer (make-numeric-literal :value 50)))))
  "var x, y = 50")

(deftest pretty-print-block/1 :notes pretty-print
  (pretty-string
   (make-block :statements
               (list (make-var-decl-stmt :var-decls (list (make-var-decl :name "x" :initializer (make-numeric-literal :value 55))))
                     (make-binary-operator :op-symbol :times-equals :left-arg (make-identifier :name "x") :right-arg foo-id))))
  "{
  var x = 55;
  x *= foo;
}")

(deftest pretty-print-block/2 :notes pretty-print
  (with-indent
    (pretty-string
     (make-block :statements
                 (list (make-var-decl-stmt :var-decls (list (make-var-decl :name "x" :initializer (make-numeric-literal :value 55))))
                       (make-binary-operator :op-symbol :times-equals :left-arg (make-identifier :name "x") :right-arg foo-id)))))
  "  {
    var x = 55;
    x *= foo;
  }")

(deftest pretty-print-if/1 :notes pretty-print
  (pretty-string (make-if :condition foo-id :then-statement bar-id))
  "if(foo)
  bar")

(deftest pretty-print-if/2 :notes pretty-print
  (pretty-string (make-if :condition foo-id :then-statement bar-id :else-statement baz-id))
  "if(foo)
  bar;
else
  baz")

(deftest pretty-print-if/3 :notes pretty-print
  (pretty-string (make-if :condition foo-id :then-statement (make-block :statements (list bar-id))))
  "if(foo)
{
  bar;
}")

(deftest pretty-print-if/4 :notes pretty-print
  (pretty-string
   (make-if :condition foo-id :then-statement (make-block :statements (list bar-id)) :else-statement baz-id))
  "if(foo)
{
  bar;
}
else
  baz")

(deftest pretty-print-if/5 :notes pretty-print
  (pretty-string
   (make-if :condition foo-id :then-statement bar-id
            :else-statement (make-block :statements (list baz-id))))
   "if(foo)
  bar;
else
{
  baz;
}")


(deftest pretty-print-do/1 :notes pretty-print
  (pretty-string (make-do :condition (make-binary-operator :op-symbol :greater-than
                                                        :left-arg foo-id
                                                        :right-arg (make-numeric-literal :value 55.0))
                       :body (make-block :statements
                                         (list (make-unary-operator :op-symbol :post-incr :arg foo-id)))))
  "do
{
  foo++;
}
while(foo > 55.0)")

(deftest pretty-print-while/1 :notes pretty-print
  (pretty-string (make-while :condition (make-unary-operator :op-symbol :typeof :arg foo-id)
                             :body (make-unary-operator :op-symbol :delete :arg foo-id)))
  "while(typeof foo)
  delete foo")

(deftest pretty-print-while/2 :notes pretty-print
  (pretty-string (make-while :condition (make-unary-operator :op-symbol :typeof :arg foo-id)
                             :body (make-block :statements (list (make-unary-operator :op-symbol :delete :arg foo-id)))))
  "while(typeof foo)
{
  delete foo;
}")

(deftest pretty-print-for/1 :notes pretty-print
  (pretty-string (make-for :body (make-block)))
  "for(; ; )
{
}")

(deftest pretty-print-for/2 :notes pretty-print
  (pretty-string (make-for :initializer (make-var-decl-stmt :var-decls (list (make-var-decl :name "foo" :initializer bar-id)))
                        :condition (make-binary-operator :op-symbol :not-equals :left-arg foo-id :right-arg baz-id)
                        :step (make-unary-operator :op-symbol :pre-incr :arg foo-id)
                        :body (make-block :statements (list (make-unary-operator :op-symbol :post-decr :arg foo-id)))))
  "for(var foo = bar; foo != baz; ++foo)
{
  foo--;
}")

(deftest pretty-print-for-in/1 :notes pretty-print
  (pretty-string (make-for-in :binding (make-var-decl-stmt :var-decls (list (make-var-decl :name "foo")))
                           :collection bar-id
                           :body (make-fn-call :fn baz-id :args (list foo-id bar-id))))
  "for(var foo in bar)
  baz(foo, bar)")

(deftest pretty-print-for-in/2 :notes pretty-print
  (pretty-string (make-for-in :binding foo-id
                              :collection bar-id
                              :body (make-fn-call :fn baz-id :args (list foo-id bar-id))))
  "for(foo in bar)
  baz(foo, bar)")

(deftest pretty-print-switch/1 :notes pretty-print
  (pretty-string (make-switch :value foo-id :clauses
                 (list
                  (make-case :label (make-numeric-literal :value 10)
                             :body (list
                                    (make-fn-call :fn bar-id :args (list (make-numeric-literal :value 1)))
                                    (make-fn-call :fn baz-id :args (list foo-id))
                                    (make-break)))
                  (make-case :label (make-numeric-literal :value 20))
                  (make-case :label (make-numeric-literal :value 30)
                             :body (list
                                    (make-fn-call :fn bar-id :args (list (make-numeric-literal :value 3)))))
                  (make-default :body (list
                                       (make-return :arg foo-id))))))
  "switch(foo)
{
case 10:
  bar(1);
  baz(foo);
  break;
case 20:
case 30:
  bar(3);
default:
  return foo;
}")

(deftest pretty-print-with/1 :notes pretty-print
  (pretty-string (make-with :scope-object foo-id :body (make-block :statements (list bar-id))))
  "with(foo)
{
  bar;
}")

(deftest pretty-print-label/1 :notes pretty-print
  (pretty-string (make-label :name "fhwqgads" :statement (make-fn-call :fn foo-id :args (list bar-id baz-id))))
  "fhwqgads:
foo(bar, baz)")

(deftest pretty-print-try/1 :notes pretty-print
  (pretty-string (make-try :body (make-block :statements (list (make-fn-call :fn foo-id :args (list bar-id))))
                        :catch-clause (make-catch-clause :binding "e" :body (make-block :statements (list (make-fn-call :fn foo-id :args (list (make-identifier :name "e"))))))
                        :finally-clause (make-finally-clause :body (make-block :statements (list (make-fn-call :fn baz-id) (make-unary-operator :op-symbol :delete :arg foo-id))))))
  "try
{
  foo(bar);
}
catch(e)
{
  foo(e);
}
finally
{
  baz();
  delete foo;
}")

(deftest pretty-print-function-decl/1 :notes pretty-print
  (pretty-string
   (make-function-decl :name "yarb" :parameters '("x" "y") :body (list (make-fn-call :fn foo-id) (make-fn-call :fn bar-id) (make-fn-call :fn baz-id))))
  "function yarb(x, y)
{
  foo();
  bar();
  baz();
}")

;;  TODO Add test case(s) for function expressions (at least one for single-line and one for multi-line)
