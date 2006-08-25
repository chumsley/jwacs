;;;; test-parser.lisp
;;;
;;; Unit tests for the Javascript parser
;;;
;;; Copyright (c) 2005 James Wright
;;; See LICENSE for full licensing details.
;;;

;;TODO Round-trip test (eg, parse -> pretty-print -> parse)

(in-package :jwacs-tests)
          
;;;; Test categories
(defnote parser "tests for the parser")

;;;; Tests
(deftest parser/object-literal/1 :notes parser
  (parse "foo = {a:10};")
  (#S(binary-operator :op-symbol :assign 
		      :left-arg #S(identifier :name "foo")
		      :right-arg #S(object-literal :properties ((#S(string-literal :value "a") . #S(numeric-literal :value 10)))))))

(deftest parser/object-literal/2 :notes parser
 (parse "foo = {a: 10, b: \"Ten\"};")
 (#s(binary-operator
    :op-symbol :assign
    :left-arg #s(identifier :name "foo")
    :right-arg #s(object-literal
                  :properties ((#s(string-literal :value "a") . #s(numeric-literal :value 10))
                               (#s(string-literal :value "b") . #s(string-literal :value "Ten")))))))

(deftest parser/object-literal/3 :notes parser
  (parse "foo = {a: 10, b: {c: 10, d: 10}};")
  (#s(binary-operator
    :op-symbol :assign
    :left-arg #s(identifier :name "foo")
    :right-arg #s(object-literal
                  :properties ((#s(string-literal :value "a") . #s(numeric-literal :value 10))
                               (#s(string-literal :value "b") . #s(object-literal
                                                                   :properties ((#s(string-literal :value "c") . #s(numeric-literal :value 10))
                                                                                (#s(string-literal :value "d") . #s(numeric-literal :value 10))))))))))

(deftest parser/re-literal/1 :notes parser
  (parse "/hello/;")
  (#S(re-literal :pattern "hello" :options "")))

(deftest parser/re-literal/2 :notes parser
  (parse "/hello/ig;")
  (#S(re-literal :pattern "hello" :options "ig")))


(deftest parser/re-literal/3 :notes parser
  (parse "x = 10 / 20;
               /hello/ig;")
  (#S(binary-operator :op-symbol :assign
                      :left-arg #S(identifier :name "x")
                      :right-arg #S(binary-operator :op-symbol :divide
                                                    :left-arg #S(numeric-literal :value 10)
                                                    :right-arg #S(numeric-literal :value 20)))
   #S(re-literal :pattern "hello" :options "ig")))
              
(deftest parser/re-literal/4 :notes parser
  (parse "x = 10 / 20; /hello/ig;")
  (#S(binary-operator :op-symbol :assign
                      :left-arg #S(identifier :name "x")
                      :right-arg #S(binary-operator :op-symbol :divide
                                                    :left-arg #S(numeric-literal :value 10)
                                                    :right-arg #S(numeric-literal :value 20)))
   #S(re-literal :pattern "hello" :options "ig")))

;; I acknowledge that the parser (actually the lexer) isn't treating this case properly, 
;; but frankly I don't care.  To get it to work properly it is necessary to switch lexers
;; depending upon the current location in the syntax, and it's just not worth the effort
;; right now.  The problem is easily worked around by the programmer (just stick a newline
;; between your statements).  I'll fix this around the same time as I add semicolon insertion.
(flag-expected-failure 'parser/re-literal/4)
 
(deftest parser/property-access/1 :notes parser
  (parse "var x = y[44];")
  (#S(var-decl-statement :var-decls
                        (#S(var-decl :name "x"
                                    :initializer #S(property-access :target #S(identifier :name "y")
                                                                   :field #S(numeric-literal :value 44)))))))

(deftest parser/property-access/2 :notes parser
  (parse "var x = y.z;")
  (#S(var-decl-statement :var-decls
                        (#S(var-decl :name "x"
                                    :initializer #S(property-access :target #S(identifier :name "y")
                                                                   :field #S(string-literal :value "z")))))))
(deftest parser/new-expr/1 :notes parser
  (parse "var x = new ObjectName(10, 20);")
  (#S(var-decl-statement :var-decls
                        (#S(var-decl :name "x"
                                     :initializer #S(new-expr :constructor #S(identifier :name "ObjectName")
                                                            :args (#S(numeric-literal :value 10)
                                                                   #S(numeric-literal :value 20))))))))
(deftest parser/new-expr/2 :notes parser
  (parse "new fcn;")
  (#S(new-expr :constructor #S(identifier :name "fcn")
              :args nil)))

(deftest parser/new-expr/3 :notes parser
  (parse "new fcn (ahoy1, ahoy2);")
  (#S(new-expr :constructor #S(identifier :name "fcn")
              :args (#S(identifier :name "ahoy1")
                     #S(identifier :name "ahoy2")))))

(deftest parser/new-expr/4 :notes parser
  (parse "new (foo())(baz);")
  (#S(new-expr :constructor #S(fn-call :fn #s(identifier :name "foo"))
               :args (#s(identifier :name "baz")))))

(deftest parser/new-expr/5 :notes parser
  (parse "new foo.bar[baz]();")
  (#S(new-expr :constructor #s(property-access
                               :target #s(property-access :target #s(identifier :name "foo")
                                                          :field #s(string-literal :value "bar"))
                               :field #s(identifier :name "baz")))))

(deftest parser/new-expr/6 :notes parser
  (parse "new (foo.bar[baz]());")
  (#S(new-expr :constructor #S(fn-call :fn #s(property-access
                                              :target #s(property-access :target #s(identifier :name "foo")
                                                                         :field #s(string-literal :value "bar"))
                                              :field #s(identifier :name "baz"))))))

(deftest parser/new-expr-and-nested-property-access/1 :notes parser
  (parse "var x = (new Foo).field[20];")
  (#S(var-decl-statement :var-decls
                        (#S(var-decl :name "x" 
                                    :initializer
                                    #S(property-access 
                                     :target #S(property-access 
                                              :target #S(new-expr
                                                       :constructor #S(identifier :name "Foo")
                                                       :args nil)
                                              :field #S(string-literal :value "field"))
                                     :field #S(numeric-literal :value 20)))))))

(deftest parser/fn-call/1 :notes parser
  (parse "var/*x*/varName=func(0x8);")
  (#S(var-decl-statement :var-decls
                        (#S(var-decl :name "varName"
                                    :initializer #S(fn-call :fn #S(identifier :name "func")
                                                          :args (#S(numeric-literal :value 8))))))))
(deftest parser/fn-call/2 :notes parser
  (parse "fcn ( arg );")
  (#S(fn-call :fn #S(identifier :name "fcn")
             :args (#S(identifier :name "arg")))))

(deftest parser/fn-call-and-nested-property-access/1 :notes parser
  (parse "var varName=func(0x8, 'str')['sam'].a;")
  (#S(var-decl-statement :var-decls
                        (#S(var-decl :name "varName"
                                    :initializer
                                    #S(property-access
                                     :target
                                     #S(property-access 
                                      :target
                                      #S(fn-call :fn #S(identifier :name "func")
                                                :args (#S(numeric-literal :value 8) #S(string-literal :value "str")))
                                      :field #S(string-literal :value "sam"))
                                     :field #S(string-literal :value "a")))))))

(deftest parser/binary-operator/1 :notes parser
  (parse "var x = 10 * 10 / 20;")
  (#S(var-decl-statement :var-decls
                        (#S(var-decl :name "x"
                                    :initializer
                                    #S(binary-operator :op-symbol :divide
                                                      :left-arg #S(binary-operator :op-symbol :multiply
                                                                                  :left-arg #S(numeric-literal :value 10)
                                                                                  :right-arg #S(numeric-literal :value 10))
                                                      :right-arg #S(numeric-literal :value 20)))))))
(deftest parser/binary-operator/2 :notes parser
     (parse "var x = 10 * 2 + 3;")
     (#S(var-decl-statement :var-decls
                           (#S(var-decl :name "x"
                                       :initializer
                                       #S(binary-operator :op-symbol :add
                                                         :left-arg #S(binary-operator :op-symbol :multiply
                                                                                     :left-arg #S(numeric-literal :value 10)
                                                                                     :right-arg #S(numeric-literal :value 2))
                                                         :right-arg #S(numeric-literal :value 3)))))))
(deftest parser/binary-operator/3 :notes parser
  (parse "var x = 3+10 * 2 ;")
  (#S(var-decl-statement :var-decls
                        (#S(var-decl :name "x"
                                    :initializer
                                    #S(binary-operator :op-symbol :add
                                                      :left-arg #S(numeric-literal :value 3)
                                                      :right-arg #S(binary-operator :op-symbol :multiply
                                                                                   :left-arg #S(numeric-literal :value 10)
                                                                                   :right-arg #S(numeric-literal :value 2))))))))

(deftest parser/binary-operator/4 :notes parser
  (parse "var x = 10 << (99 - 50) * 10;")
  (#S(var-decl-statement :var-decls
                        (#S(var-decl :name "x"
                                    :initializer
                                    #S(binary-operator :op-symbol :lshift
                                                      :left-arg #S(numeric-literal :value 10)
                                                      :right-arg #S(binary-operator :op-symbol :multiply
                                                                                   :left-arg #S(binary-operator :op-symbol :subtract
                                                                                                               :left-arg #S(numeric-literal :value 99)
                                                                                                               :right-arg #S(numeric-literal :value 50))
                                                                                   :right-arg #S(numeric-literal :value 10))))))))
(deftest parser/binary-operator/5 :notes parser
  (parse "var x = a & b | c ^ d;")
  (#S(var-decl-statement :var-decls
                        (#S(var-decl :name "x"
                                    :initializer
                                    #S(binary-operator :op-symbol :bitwise-or
                                                      :left-arg #S(binary-operator :op-symbol :bitwise-and
                                                                                  :left-arg #S(identifier :name "a")
                                                                                  :right-arg #S(identifier :name "b"))
                                                      :right-arg #S(binary-operator :op-symbol :bitwise-xor
                                                                                   :left-arg #S(identifier :name "c")
                                                                                   :right-arg #S(identifier :name "d"))))))))

(deftest parser/binary-operator/6 :notes parser
  (parse "var x = a && b || c && d;")
  (#S(var-decl-statement :var-decls
                        (#S(var-decl :name "x"
                                    :initializer #S(binary-operator :op-symbol :logical-or
                                                                   :left-arg #S(binary-operator :op-symbol :logical-and
                                                                                               :left-arg #S(identifier :name "a")
                                                                                               :right-arg #S(identifier :name "b"))
                                                                   :right-arg #S(binary-operator :op-symbol :logical-and
                                                                                                :left-arg #S(identifier :name "c")
                                                                                                :right-arg #S(identifier :name "d"))))))))
    
(deftest parser/binary-operator/7 :notes parser
  (parse "x == y;")
  (#S(binary-operator :op-symbol :equals
                     :left-arg #S(identifier :name "x")
                     :right-arg #S(identifier :name "y"))))

(deftest parser/binary-operator/8 :notes parser
  (parse "x = y;")
  (#S(binary-operator :op-symbol :assign
                     :left-arg #S(identifier :name "x")
                     :right-arg #S(identifier :name "y"))))


(deftest parser/binary-operator/9 :notes parser
  (parse "x += 50;")
  (#S(binary-operator :op-symbol :plus-equals
                     :left-arg #S(identifier :name "x")
                     :right-arg #S(numeric-literal :value 50))))

(deftest parser/unary-operator/1 :notes parser
  (parse "delete x++;")
  (#S(unary-operator :op-symbol :delete
                    :arg #S(unary-operator :op-symbol :post-incr
                                          :arg #S(identifier :name "x")))))

(deftest parser/return-statement/1 :notes parser
  (parse "return;")
  (#S(return-statement :arg nil)))
     
(deftest parser/return-statement/2 :notes parser
  (parse "return 8 >> 2;")
  (#S(return-statement :arg #S(binary-operator :op-symbol :rshift
                                             :left-arg #S(numeric-literal :value 8)
                                             :right-arg #S(numeric-literal :value 2)))))

(deftest parser/continue-statement/1 :notes parser
  (parse "continue;")
  (#S(continue-statement :target-label nil)))

(deftest parser/continue-statement/2 :notes parser
  (parse "continue blockName;")
  (#S(continue-statement :target-label "blockName")))

(deftest parser/break-statement/1 :notes parser
  (parse "break blockName;")
  (#S(break-statement :target-label "blockName")))

(deftest parser/with/1 :notes parser
  (parse "with(x.y) { z -= 10; }")
  (#S(with :scope-object #S(property-access :target #S(identifier :name "x")
                                          :field #S(string-literal :value "y"))
          :body #S(statement-block :statements
                                  (#S(binary-operator :op-symbol :minus-equals
                                                     :left-arg #S(identifier :name "z")
                                                     :right-arg #S(numeric-literal :value 10)))))))

(deftest parser/switch/1 :notes parser
  (parse "switch(x[y]) { case 10:case 20:return x << 1;default:case 88:break; }")
  (#S(switch :value #S(property-access :target #S(identifier :name "x")
                                       :field #S(identifier :name "y"))
             :clauses
             (#S(case-clause :value #S(numeric-literal :value 10)
                             :body nil)
                #S(case-clause :value #S(numeric-literal :value 20)
                               :body (#S(return-statement :arg #S(binary-operator :op-symbol :lshift
                                                                                  :left-arg #S(identifier :name "x")
                                                                                  :right-arg #S(numeric-literal :value 1)))))
                #S(default-clause :body nil)
                #S(case-clause :value #S(numeric-literal :value 88)
                               :body (#S(break-statement :target-label nil)))))))

(deftest parser/label/1 :notes parser
  (parse "jaerb: while(true) { echo('good jorb'); }")
  (#S(while :label "jaerb"
            :condition #S(special-value :symbol :true)
            :body #S(statement-block :statements (#S(fn-call :fn #S(identifier :name "echo")
                                                             :args (#s(string-literal :value "good jorb"))))))))
(deftest parser/label/2 :notes parser
  (parse "{hello: x += 20;x*=10;}")
  (#S(statement-block :statements (#S(binary-operator :label "hello"
                                                      :op-symbol :plus-equals
                                                      :left-arg #S(identifier :name "x")
                                                      :right-arg #S(numeric-literal :value 20))
                                   #S(binary-operator :op-symbol :times-equals
                                                      :left-arg #S(identifier :name "x")
                                                      :right-arg #S(numeric-literal :value 10))))))

(deftest parser/throw-statement/1 :notes parser
  (parse "throw -1;")
  (#S(throw-statement :value #S(unary-operator :op-symbol :unary-minus
                                               :arg #S(numeric-literal :value 1)))))

(deftest parser/throw-statement/2 :notes parser
  (parse "throw 1 -> k;")
  (#S(throw-statement :value #S(numeric-literal :value 1)
                      :target #S(identifier :name "k"))))

(deftest parser/try/1 :notes parser
  (parse "try { throw x++; } catch(y) {return y;}")
  (#S(try
    :body (#S(throw-statement :value #S(unary-operator :op-symbol :post-incr
                                                       :arg #S(identifier :name "x"))))
    :catch-clause #S(catch-clause :binding "y"
                                  :body (#S(return-statement :arg #S(identifier :name "y"))))
    :finally-clause nil)))

(deftest parser/try/2 :notes parser
  (parse "try {throw 10;} finally {delete x;delete y;}")
  (#S(try
    :body (#S(throw-statement :value #S(numeric-literal :value 10)))
    :catch-clause nil
    :finally-clause #S(finally-clause :body (#S(unary-operator :op-symbol :delete
                                                               :arg #S(identifier :name "x"))
                                               #S(unary-operator :op-symbol :delete
                                                                 :arg #S(identifier :name "y")))))))

(deftest parser/try/3 :notes parser
  (parse "try {func(x);} catch(e) {} finally {delete x;}")
  (#S(try
    :body (#S(fn-call :fn #S(identifier :name "func")
                      :args (#S(identifier :name "x"))))
    :catch-clause #S(catch-clause :binding "e" :body nil)
    :finally-clause #S(finally-clause :body (#S(unary-operator :op-symbol :delete
                                                               :arg #S(identifier :name "x")))))))
  
  
(deftest parser/if/1 :notes parser
  (parse "if(x == 10) {y=100;} else {y = null;}")
  (#S(if-statement :condition #S(binary-operator :op-symbol :equals
                                               :left-arg #S(identifier :name "x")
                                               :right-arg #S(numeric-literal :value 10))
                  :then-statement #S(statement-block :statements
                                                    (#S(binary-operator :op-symbol :assign
                                                                       :left-arg #S(identifier :name "y")
                                                                       :right-arg #S(numeric-literal :value 100))))
                  :else-statement #S(statement-block :statements
                                                    (#S(binary-operator :op-symbol :assign
                                                                       :left-arg #S(identifier :name "y")
                                                                       :right-arg #S(special-value :symbol :null)))))))

(deftest parser/function-decl/1 :notes parser
  (parse "function foo(x) { if(x == 20) return 10; return 55;}")
  (#S(function-decl :name "foo"
                   :parameters ("x")
                   :body  (#S(if-statement :condition
                                          #S(binary-operator :op-symbol :equals
                                                            :left-arg #S(identifier :name "x")
                                                            :right-arg #S(numeric-literal :value 20))
                                          :then-statement
                                          #S(return-statement :arg #S(numeric-literal :value 10))
                                          :else-statement
                                          nil)
                           #S(return-statement :arg #S(numeric-literal :value 55))))))

(deftest parser/function-decl/2 :notes parser
  (parse "function foo(x) { var x = 10; function bar() { return x; } }")
  (#S(function-decl :name "foo"
                    :parameters ("x")
                    :body (#S(var-decl-statement :var-decls (#S(var-decl :name "x" :initializer #S(numeric-literal :value 10))))
                           #S(function-decl :name "bar"
                                            :parameters ()
                                            :body (#S(return-statement :arg #S(identifier :name "x"))))))))

(deftest parser/function-decl/3 :notes parser
  (parse "function foo() { }")
  (#S(function-decl :name "foo"
                    :parameters nil
                    :body nil)))

(deftest parser/function-decl-and-toplevel-call/1 :notes parser
  (parse "function make_adder(n) { return function(x) { return x + n;};} make_adder(20);")
  (#S(function-decl :name "make_adder"
                   :parameters ("n")
                   :body (#S(return-statement :arg #S(function-expression
                                                   :name nil
                                                   :parameters ("x")
                                                   :body (#S(return-statement :arg #S(binary-operator :op-symbol :add
                                                                                                    :left-arg #S(identifier :name "x")
                                                                                                    :right-arg #S(identifier :name "n"))))))))
   #S(fn-call :fn #S(identifier :name "make_adder")
             :args (#S(numeric-literal :value 20)))))

(deftest parser/function-expression/1 :notes parser
  (parse "var x = function f(n) { if(n > 0) return n*f(n-1); else return 1;};")
  (#S(var-decl-statement :var-decls
                        (#S(var-decl :name "x"
                                    :initializer #S(function-expression
                                                  :name "f"
                                                  :parameters ("n")
                                                  :body (#S(if-statement
                                                          :condition
                                                          #S(binary-operator :op-symbol :greater-than
                                                                            :left-arg #S(identifier :name "n")
                                                                            :right-arg #S(numeric-literal :value 0))
                                                          :then-statement
                                                          #S(return-statement :arg #S(binary-operator :op-symbol :multiply
                                                                                                    :left-arg #S(identifier :name "n")
                                                                                                    :right-arg #S(fn-call :fn #S(identifier :name "f")
                                                                                                                         :args (#S(binary-operator :op-symbol :subtract
                                                                                                                                                  :left-arg #S(identifier :name "n")
                                                                                                                                                  :right-arg #S(numeric-literal :value 1))))))
                                                          :else-statement
                                                          #S(return-statement :arg #S(numeric-literal :value 1))))))))))

(deftest parser/suspend-statement/1 :notes parser
  (parse "suspend;")
  (#S(suspend-statement)))

(deftest parser/resume-statement/1 :notes parser
  (parse "resume getContinuation();")
  (#S(resume-statement :target #S(fn-call :fn #S(identifier :name "getContinuation")))))

(deftest parser/resume-statement/2 :notes parser
  (parse "resume foo.bar <- baz;")
  (#S(resume-statement :target #S(property-access :target #S(identifier :name "foo")
                                                  :field #S(string-literal :value "bar"))
                       :arg #S(identifier :name "baz"))))

(deftest parser/resume-statement/3 :notes parser
  (parse "resume k <- 100;")
  (#S(resume-statement :target #S(identifier :name "k")
                       :arg #S(numeric-literal :value 100))))

(deftest parser/function_continuation/1 :notes parser
  (parse "x = function_continuation;")
  (#S(binary-operator :op-symbol :assign
                      :left-arg #S(identifier :name "x")
                      :right-arg #S(special-value :symbol :function_continuation))))

(deftest parser/for/1 :notes parser
  (parse "for(;;) suspend;")
  (#S(for :body #s(suspend-statement))))

(deftest parser/for/2 :notes parser
  (parse "for(x=0;;) suspend;")
  (#S(for :initializer #s(binary-operator :op-symbol :assign :left-arg #s(identifier :name "x") :right-arg #s(numeric-literal :value 0))
          :body #s(suspend-statement))))

(deftest parser/for/3 :notes parser
  (parse "for(;true;) { suspend; }")
  (#S(for :condition #S(special-value :symbol :true)
          :body #S(statement-block :statements (#S(suspend-statement))))))

(deftest parser/for/4 :notes parser
  (parse "for(;;x++) suspend;")
  (#S(for :step #s(unary-operator :op-symbol :post-incr :arg #s(identifier :name "x"))
          :body #s(suspend-statement))))

(deftest parser/for/5 :notes parser
  (parse "for(x=0;true;) suspend;")
  (#S(for :initializer #s(binary-operator :op-symbol :assign :left-arg #s(identifier :name "x") :right-arg #s(numeric-literal :value 0))
          :condition #S(special-value :symbol :true)
          :body #s(suspend-statement))))
  
(deftest parser/for/6 :notes parser
  (parse "for(;true;x++) suspend;")
  (#S(for :condition #S(special-value :symbol :true)
          :step #s(unary-operator :op-symbol :post-incr :arg #s(identifier :name "x"))
          :body #s(suspend-statement))))

(deftest parser/for/7 :notes parser
  (parse "for(x=0;;x++) suspend;")
  (#S(for :initializer #s(binary-operator :op-symbol :assign :left-arg #s(identifier :name "x") :right-arg #s(numeric-literal :value 0))
          :step #s(unary-operator :op-symbol :post-incr :arg #s(identifier :name "x"))
          :body #s(suspend-statement))))

(deftest parser/for/8 :notes parser
  (parse "for(x=0;true;x++) suspend;")
  (#S(for :initializer #s(binary-operator :op-symbol :assign :left-arg #s(identifier :name "x") :right-arg #s(numeric-literal :value 0))
          :condition #S(special-value :symbol :true)
          :step #s(unary-operator :op-symbol :post-incr :arg #s(identifier :name "x"))
          :body #s(suspend-statement))))

(deftest parser/for/9 :notes parser
  (parse "for(var x = 0;;) suspend;")
  (#s(for :initializer #s(var-decl-statement :var-decls (#s(var-decl :name "x" :initializer #s(numeric-literal :value 0))))
          :body #s(suspend-statement))))

(deftest parser/for/10 :notes parser
  (parse "for(var x = 0;true;) suspend;")
  (#s(for :initializer #s(var-decl-statement :var-decls (#s(var-decl :name "x" :initializer #s(numeric-literal :value 0))))
          :condition #S(special-value :symbol :true)
          :body #s(suspend-statement))))

(deftest parser/for/11 :notes parser
  (parse "for(var x = 0;;x++) suspend;")
  (#s(for :initializer #s(var-decl-statement :var-decls (#s(var-decl :name "x" :initializer #s(numeric-literal :value 0))))
          :step #s(unary-operator :op-symbol :post-incr :arg #s(identifier :name "x"))
          :body #s(suspend-statement))))

(deftest parser/for/12 :notes parser
  (parse "for(var x = 0;true;x++) suspend;")
  (#s(for :initializer #s(var-decl-statement :var-decls (#s(var-decl :name "x" :initializer #s(numeric-literal :value 0))))
          :condition #S(special-value :symbol :true)
          :step #s(unary-operator :op-symbol :post-incr :arg #s(identifier :name "x"))
          :body #s(suspend-statement))))

(deftest parser/array-literal/1 :notes parser
  (parse "x = [];")
  (#s(binary-operator :op-symbol :assign :left-arg #s(identifier :name "x")
                      :right-arg #s(array-literal :elements nil))))

(deftest parser/array-literal/2 :notes parser
  (parse "x=[1];")
  (#s(binary-operator :op-symbol :assign :left-arg #s(identifier :name "x")
                      :right-arg #s(array-literal :elements (#s(numeric-literal :value 1))))))

(deftest parser/array-literal/3 :notes parser
  (parse "x=[1,2];")
  (#s(binary-operator :op-symbol :assign :left-arg #s(identifier :name "x")
                      :right-arg #s(array-literal :elements (#s(numeric-literal :value 1)
                                                             #s(numeric-literal :value 2))))))

(deftest parser/array-literal/4 :notes parser
  (parse "x=[1,,2];")
  (#s(binary-operator :op-symbol :assign :left-arg #s(identifier :name "x")
                      :right-arg #s(array-literal :elements (#s(numeric-literal :value 1)
                                                             #s(identifier :name "undefined")
                                                             #s(numeric-literal :value 2))))))

(deftest parser/array-literal/5 :notes parser
  (parse "x=[1,];")
  (#s(binary-operator :op-symbol :assign :left-arg #s(identifier :name "x")
                      :right-arg #s(array-literal :elements (#s(numeric-literal :value 1)
                                                             #s(identifier :name "undefined"))))))
(deftest parser/array-literal/6 :notes parser
  (parse "x=[,,1];")
  (#s(binary-operator :op-symbol :assign :left-arg #s(identifier :name "x")
                      :right-arg #s(array-literal :elements (#s(identifier :name "undefined")
                                                             #s(identifier :name "undefined")
                                                             #s(numeric-literal :value 1))))))

(deftest parser/array-literal/7 :notes parser
  (parse "x=[,1,];")
  (#s(binary-operator :op-symbol :assign :left-arg #s(identifier :name "x")
                      :right-arg #s(array-literal :elements (#s(identifier :name "undefined")
                                                             #s(numeric-literal :value 1)
                                                             #s(identifier :name "undefined"))))))

(deftest parser/array-literal/8 :notes parser
  (parse "x=[,,,];")
  (#s(binary-operator :op-symbol :assign :left-arg #s(identifier :name "x")
                      :right-arg #s(array-literal :elements (#s(identifier :name "undefined")
                                                             #s(identifier :name "undefined")
                                                             #s(identifier :name "undefined")
                                                             #s(identifier :name "undefined"))))))


(deftest parser/semicolon-insertion/1 :notes parser
  (parse "foo()")
  #.(parse "foo();"))

(deftest parser/semicolon-insertion/2 :notes parser
  (expect-error (parse "foo() bar()")
                syntax-error)
  t)

(deftest parser/semicolon-insertion/3 :notes parser
  (parse "foo()
          bar()")
  #.(parse "foo();
            bar();"))

(deftest parser/semicolon-insertion/4 :notes parser
  (parse "foo() /* multi-line
                   comments count
                   as containing line-terminators
       */ bar()")
  #.(parse "foo();
            bar();"))

(deftest parser/semicolon-insertion/5 :notes parser
  (expect-error (parse "if(x) foo() bar()")
                syntax-error)
  t)

(deftest parser/semicolon-insertion/6 :notes parser
  (parse "if(x) { foo() } bar()")
  #.(parse "if(x) { foo(); } bar();"))

;; Some further semicolon-insertion unit-tests from the spec
(deftest parser/semicolon-insertion/7 :notes parser
  (expect-error (parse "{ 1 2 } 3")
                syntax-error)
  t)

(deftest parser/semicolon-insertion/8 :notes parser
  (parse "{ 1
            2 } 3")
  #.(parse "{ 1
            ;2 ;} 3;"))

(deftest parser/semicolon-insertion/9 :notes parser
  (expect-error (parse "for(a; b
                        )")
                syntax-error)
  t)

(deftest parser/semicolon-insertion/10 :notes parser
  (parse "a = b + c
          (d + e).print()")
  #.(parse "a = b + (c(d + e)).print()"))

(deftest parser/semicolon-insertion/11 :notes parser
  (expect-error (parse "if (a > b)
                        else c = d")
                syntax-error)
  t)

(deftest parser/semicolon-insertion/12 :notes parser
  (parse "return
          a + b")
  #.(parse "return;
            a + b;"))

(deftest parser/semicolon-insertion/13 :notes parser
  (parse "a = b
          ++c")
  #.(parse "a = b; ++c;"))
