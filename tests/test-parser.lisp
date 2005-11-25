;;;; test-parser.lisp
;;;
;;; Unit tests for the Javascript parser
;;;
;;; NOTE: Because (equal s1 s2) doesn't work for structures, we instead use
;;; parse-to-plist, which returns a list-based representation of the source-element
;;; structures.

;;TODO Round-trip test (eg, parse -> pretty-print -> parse)

(in-package :jwacs-tests)
          
;;;; Helper functions
(defun parse-only (string)
  "Call PARSE on STRING and return only the first return value.
   This is to deal with the fact that the Lispworks parser returns a second value
   indicating whether any errors occurred, whereas cl-yacc does not."
  (multiple-value-bind (val1)
      (parse string)
    val1))

;;;; Test categories
(defnote parser "tests for the parser")

;;;; Tests
(deftest parser/object-literal/1 :notes parser
  (parse-only "{a:10};")
  (#S(object-literal :properties ((#S(identifier :name "a") . #S(numeric-literal :value 10))))))

(deftest parser/re-literal/1 :notes parser
  (parse-only "/hello/;")
  (#S(re-literal :pattern "hello" :options "")))

(deftest parser/re-literal/2 :notes parser
  (parse-only "/hello/ig;")
  (#S(re-literal :pattern "hello" :options "ig")))

(deftest parser/property-access/1 :notes parser
  (parse-only "var x = y[44];" )
  (#S(var-decl-statement :var-decls
                        (#S(var-decl :name "x"
                                    :initializer #S(property-access :target #S(identifier :name "y")
                                                                   :field #S(numeric-literal :value 44)))))))

(deftest parser/property-access/2 :notes parser
  (parse-only "var x = y.z;")
  (#S(var-decl-statement :var-decls
                        (#S(var-decl :name "x"
                                    :initializer #S(property-access :target #S(identifier :name "y")
                                                                   :field #S(string-literal :value "z")))))))
(deftest parser/new-expr/1 :notes parser
  (parse-only "var x = new ObjectName(10, 20);")
  (#S(var-decl-statement :var-decls
                        (#S(var-decl :name "x"
                                    :initializer #S(new-expr :object-name #S(identifier :name "ObjectName")
                                                            :args (#S(numeric-literal :value 10)
                                                                   #S(numeric-literal :value 20))))))))
(deftest parser/new-expr/2 :notes parser
  (parse-only "var x = new 'strtype';")
  (#S(var-decl-statement :var-decls
                        (#S(var-decl :name "x"
                                    :initializer #S(new-expr
                                                  :object-name #S(string-literal :value "strtype")
                                                  :args nil))))))

(deftest parser/new-expr/3 :notes parser
  (parse-only "new fcn;")
  (#S(new-expr :object-name #S(identifier :name "fcn")
              :args nil)))

(deftest parser/new-expr/4 :notes parser
  (parse-only "new fcn (ahoy1, ahoy2);")
  (#S(new-expr :object-name #S(identifier :name "fcn")
              :args (#S(identifier :name "ahoy1")
                     #S(identifier :name "ahoy2")))))


(deftest parser/new-expr-and-nested-property-access/1 :notes parser
  (parse-only "var x = (new 'strtype').field[20];")
  (#S(var-decl-statement :var-decls
                        (#S(var-decl :name "x" 
                                    :initializer
                                    #S(property-access 
                                     :target #S(property-access 
                                              :target #S(new-expr
                                                       :object-name #S(string-literal :value "strtype")
                                                       :args nil)
                                              :field #S(string-literal :value "field"))
                                     :field #S(numeric-literal :value 20)))))))

(deftest parser/fn-call/1 :notes parser
  (parse-only "var/*x*/varName=func(0x8);")
  (#S(var-decl-statement :var-decls
                        (#S(var-decl :name "varName"
                                    :initializer #S(fn-call :fn #S(identifier :name "func")
                                                          :args (#S(numeric-literal :value 8))))))))
(deftest parser/fn-call/2 :notes parser
  (parse-only "fcn ( arg );")
  (#S(fn-call :fn #S(identifier :name "fcn")
             :args (#S(identifier :name "arg")))))

(deftest parser/fn-call-and-nested-property-access/1 :notes parser
  (parse-only "var varName=func(0x8, 'str')['sam'].a;")
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
  (parse-only "var x = 10 * 10 / 20;")
  (#S(var-decl-statement :var-decls
                        (#S(var-decl :name "x"
                                    :initializer
                                    #S(binary-operator :op-symbol :divide
                                                      :left-arg #S(binary-operator :op-symbol :multiply
                                                                                  :left-arg #S(numeric-literal :value 10)
                                                                                  :right-arg #S(numeric-literal :value 10))
                                                      :right-arg #S(numeric-literal :value 20)))))))
(deftest parser/binary-operator/2 :notes parser
     (parse-only "var x = 10 * 2 + 3;")
     (#S(var-decl-statement :var-decls
                           (#S(var-decl :name "x"
                                       :initializer
                                       #S(binary-operator :op-symbol :add
                                                         :left-arg #S(binary-operator :op-symbol :multiply
                                                                                     :left-arg #S(numeric-literal :value 10)
                                                                                     :right-arg #S(numeric-literal :value 2))
                                                         :right-arg #S(numeric-literal :value 3)))))))
(deftest parser/binary-operator/3 :notes parser
  (parse-only "var x = 3+10 * 2 ;")
  (#S(var-decl-statement :var-decls
                        (#S(var-decl :name "x"
                                    :initializer
                                    #S(binary-operator :op-symbol :add
                                                      :left-arg #S(numeric-literal :value 3)
                                                      :right-arg #S(binary-operator :op-symbol :multiply
                                                                                   :left-arg #S(numeric-literal :value 10)
                                                                                   :right-arg #S(numeric-literal :value 2))))))))

(deftest parser/binary-operator/4 :notes parser
  (parse-only "var x = 10 << (99 - 50) * 10;")
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
  (parse-only "var x = a & b | c ^ d;")
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
  (parse-only "var x = a && b || c && d;")
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
  (parse-only "x == y;")
  (#S(binary-operator :op-symbol :equals
                     :left-arg #S(identifier :name "x")
                     :right-arg #S(identifier :name "y"))))

(deftest parser/binary-operator/8 :notes parser
  (parse-only "x = y;")
  (#S(binary-operator :op-symbol :assign
                     :left-arg #S(identifier :name "x")
                     :right-arg #S(identifier :name "y"))))


(deftest parser/binary-operator/9 :notes parser
  (parse-only "x += 50;")
  (#S(binary-operator :op-symbol :plus-equals
                     :left-arg #S(identifier :name "x")
                     :right-arg #S(numeric-literal :value 50))))

(deftest parser/unary-operator/1 :notes parser
  (parse-only "delete x++;")
  (#S(unary-operator :op-symbol :delete
                    :arg #S(unary-operator :op-symbol :post-incr
                                          :arg #S(identifier :name "x")))))

(deftest parser/return-statement/1 :notes parser
  (parse-only "return;")
  (#S(return-statement :arg nil)))
     
(deftest parser/return-statement/2 :notes parser
  (parse-only "return 8 >> 2;")
  (#S(return-statement :arg #S(binary-operator :op-symbol :rshift
                                             :left-arg #S(numeric-literal :value 8)
                                             :right-arg #S(numeric-literal :value 2)))))

(deftest parser/continue-statement/1 :notes parser
  (parse-only "continue;")
  (#S(continue-statement :label nil)))

(deftest parser/continue-statement/2 :notes parser
  (parse-only "continue blockName;")
  (#S(continue-statement :label "blockName")))

(deftest parser/break-statement/1 :notes parser
  (parse-only "break blockName;")
  (#S(break-statement :label "blockName")))

(deftest parser/with/1 :notes parser
  (parse-only "with(x.y) { z -= 10; }")
  (#S(with :scope-object #S(property-access :target #S(identifier :name "x")
                                          :field #S(string-literal :value "y"))
          :body #S(statement-block :statements
                                  (#S(binary-operator :op-symbol :minus-equals
                                                     :left-arg #S(identifier :name "z")
                                                     :right-arg #S(numeric-literal :value 10)))))))

(deftest parser/switch/1 :notes parser
  (parse-only "switch(x[y]) { case 10:case 20:return x << 1;default:case 88:break; }")
  (#S(switch :value #S(property-access :target #S(identifier :name "x")
                                     :field #S(identifier :name "y"))
            :clauses
            (#S(case-clause :label #S(numeric-literal :value 10)
                           :body nil)
             #S(case-clause :label #S(numeric-literal :value 20)
                           :body (#S(return-statement :arg #S(binary-operator :op-symbol :lshift
                                                                            :left-arg #S(identifier :name "x")
                                                                            :right-arg #S(numeric-literal :value 1)))))
             #S(default-clause :body nil)
             #S(case-clause :label #S(numeric-literal :value 88)
                           :body (#S(break-statement :label nil)))))))

(deftest parser/statement-block/1 :notes parser
  (parse-only "{hello: x += 20;x*=10;}")
  (#S(statement-block :statements (#S(label :name "hello" :statement #S(binary-operator :op-symbol :plus-equals
                                                                               :left-arg #S(identifier :name "x")
                                                                               :right-arg #S(numeric-literal :value 20)))
                                  #S(binary-operator :op-symbol :times-equals
                                                    :left-arg #S(identifier :name "x")
                                                    :right-arg #S(numeric-literal :value 10))))))

(deftest parser/throw-statement/1 :notes parser
  (parse-only "throw -1;")
  (#S(throw-statement :value #S(unary-operator :op-symbol :unary-minus
                                             :arg #S(numeric-literal :value 1)))))

(deftest parser/try/1 :notes parser
  (parse-only "try { throw x++; } catch(y) {return y;}")
  (#S(try
    :body #S(statement-block :statements
                            (#S(throw-statement :value #S(unary-operator :op-symbol :post-incr
                                                                      :arg #S(identifier :name "x")))))
    :catch-clause #S(catch-clause :binding "y"
                                 :body #S(statement-block :statements
                                                        (#S(return-statement :arg #S(identifier :name "y")))))
    :finally-clause nil)))

(deftest parser/try/2 :notes parser
  (parse-only "try {throw 10;} finally {delete x;delete y;}")
  (#S(try
    :body #S(statement-block :statements
                            (#S(throw-statement :value #S(numeric-literal :value 10))))
    :catch-clause nil
    :finally-clause #S(finally-clause :body #S(statement-block :statements
                                                             (#S(unary-operator :op-symbol :delete
                                                                               :arg #S(identifier :name "x"))
                                                              #S(unary-operator :op-symbol :delete
                                                                               :arg #S(identifier :name "y"))))))))

(deftest parser/try/3 :notes parser
  (parse-only "try {func(x);} catch(e) {} finally {delete x;}")
  (#S(try
    :body #S(statement-block :statements
                            (#S(fn-call :fn #S(identifier :name "func")
                                       :args (#S(identifier :name "x")))))
    :catch-clause #S(catch-clause :binding "e" :body #S(statement-block :statements nil))
    :finally-clause #S(finally-clause :body #S(statement-block :statements
                                                            (#S(unary-operator :op-symbol :delete
                                                                              :arg #S(identifier :name "x"))))))))
  
  
(deftest parser/if/1 :notes parser
  (parse-only "if(x == 10) {y=100;} else {y = null;}")
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
  (parse-only "function foo(x) { if(x == 20) return 10; return 55;}")
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
  (parse-only "function foo(x) { var x = 10; function bar() { return x; } }")
  (#S(function-decl :name "foo"
                    :parameters ("x")
                    :body (#S(var-decl-statement :var-decls (#S(var-decl :name "x" :initializer #S(numeric-literal :value 10))))
                           #S(function-decl :name "bar"
                                            :parameters ()
                                            :body (#S(return-statement :arg #S(identifier :name "x"))))))))

(deftest parser/function-decl-and-toplevel-call/1 :notes parser
  (parse-only "function make_adder(n) { return function(x) { return x + n;};} make_adder(20);")
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
  (parse-only "var x = function f(n) { if(n > 0) return n*f(n-1); else return 1;};")
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

(deftest parser/function-expression/2 :notes parser
  (parse-only "function(n) { if(n > 0) return n*f(n-1); else return 1;};")
  (#S(function-expression :name nil
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
                                 #S(return-statement :arg #S(numeric-literal :value 1)))))))

(deftest parser/function-expression/3 :notes parser
  (parse-only "
    function foo(x)
    {
      var y = 10;
      if(y)
      {
        function bar()
        { return x; }
      }
    }")
  (#S(function-decl :name "foo"
                    :parameters ("x")
                    :body (#S(var-decl-statement :var-decls (#S(var-decl :name "y" :initializer #S(numeric-literal :value 10))))
                           #S(if-statement :condition #s(identifier :name "y")
                                           :then-statement
                                           #s(statement-block :statements
                                                              (#S(function-expression :name "bar"
                                                                                      :parameters ()
                                                                                      :body (#S(return-statement :arg #S(identifier :name "x")))))))))))