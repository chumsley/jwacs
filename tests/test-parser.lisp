;;;; test-parser.lisp
;;;
;;; Unit tests for the Javascript parser
;;;
;;; NOTE: Because (equal s1 s2) doesn't work for structures, we instead use
;;; parse-to-plist, which returns a list-based representation of the source-element
;;; structures.

(in-package :jwacs-tests)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(jwacs::structure-slots
            jwacs::make-label
            jwacs::make-identifier)))
          
          
;;;; Helper functions
(defgeneric structure-to-plist (maybe-structure)
  (:documentation
   "Convert structures to lists of the form (struct-name slot-name slot-value ...).
    The conversion is recursive; structure slot values and lists are traversed."))

(defmethod structure-to-plist ((maybe-structure structure-object))
  (flet ((keyword-of (symbol)
           (intern (symbol-name symbol) :keyword)))
    (cons (keyword-of (type-of maybe-structure))
          (loop for slot in (structure-slots maybe-structure)
                for slot-name = (keyword-of slot)
                nconc (list slot-name
                            (structure-to-plist (slot-value maybe-structure slot)))))))

(defmethod structure-to-plist ((maybe-structure list))
  (if (listp (cdr maybe-structure))
    (mapcar 'structure-to-plist maybe-structure)
    (cons (structure-to-plist (car maybe-structure))
          (structure-to-plist (cdr maybe-structure)))))

(defmethod structure-to-plist (maybe-structure)
  maybe-structure)

(defun parse-to-plist (string)
  "Parse STRING and return the resulting source-elements converted to
   plists by STRUCTURE-TO-PLIST."
  (structure-to-plist (parse string)))

;;;; Test categories
(defnote parser "tests for the parser")
(defnote test-helpers "tests for test-helper functions")

;;;; Tests
(deftest structure-to-plist/1 :notes (parser test-helpers)
  (structure-to-plist (list
                       (make-identifier :name "foo")
                       (make-label :name "bar" :statement (make-identifier :name "baz"))))
  ((:identifier :name "foo")
   (:label :name "bar" :statement (:identifier :name "baz"))))

(deftest structure-to-plist/2 :notes (parser test-helpers)
  (structure-to-plist (cons (make-identifier :name "foo")
                            (make-identifier :name "bar")))
  ((:identifier :name "foo") . (:identifier :name "bar")))

(deftest parse-to-plist/1 :notes (parser test-helpers)
  (parse-to-plist "this;")
  ((:special-value :symbol :this)))

(deftest parse-object-literal/1 :notes parser
  (parse-to-plist "{a:10};")
  ((:object-literal :properties (((:identifier :name "a") . (:numeric-literal :value 10))))))

(deftest parse-property-access/1 :notes parser
  (parse-to-plist "var x = y[44];" )
  ((:var-decl-statement :var-decls
                        ((:var-decl :name "x"
                                    :initializer (:property-access :target (:identifier :name "y")
                                                                   :field (:numeric-literal :value 44)))))))

(deftest parse-property-access/2 :notes parser
  (parse-to-plist "var x = y.z;")
  ((:var-decl-statement :var-decls
                        ((:var-decl :name "x"
                                    :initializer (:property-access :target (:identifier :name "y")
                                                                   :field (:string-literal :value "z")))))))
(deftest parse-new-expr/1 :notes parser
  (parse-to-plist "var x = new ObjectName(10, 20);")
  ((:var-decl-statement :var-decls
                        ((:var-decl :name "x"
                                    :initializer (:new-expr :object-name (:identifier :name "ObjectName")
                                                            :args ((:numeric-literal :value 10)
                                                                   (:numeric-literal :value 20))))))))
(deftest parse-new-expr/2 :notes parser
  (parse-to-plist "var x = new 'strtype';")
  ((:var-decl-statement :var-decls
                        ((:var-decl :name "x"
                                    :initializer (:new-expr
                                                  :object-name (:string-literal :value "strtype")
                                                  :args nil))))))

(deftest parse-new-expr/3 :notes parser
  (parse-to-plist "new fcn;")
  ((:new-expr :object-name (:identifier :name "fcn")
              :args nil)))

(deftest parse-new-expr/4 :notes parser
  (parse-to-plist "new fcn (ahoy1, ahoy2);")
  ((:new-expr :object-name (:identifier :name "fcn")
              :args ((:identifier :name "ahoy1")
                     (:identifier :name "ahoy2")))))


(deftest parse-new-expr-and-nested-property-access/1 :notes parser
  (parse-to-plist "var x = (new 'strtype').field[20];")
  ((:var-decl-statement :var-decls
                        ((:var-decl :name "x" 
                                    :initializer
                                    (:property-access 
                                     :target (:property-access 
                                              :target (:new-expr
                                                       :object-name (:string-literal :value "strtype")
                                                       :args nil)
                                              :field (:string-literal :value "field"))
                                     :field (:numeric-literal :value 20)))))))

(deftest parse-fn-call/1 :notes parser
  (parse-to-plist "var/*x*/varName=func(0x8);")
  ((:var-decl-statement :var-decls
                        ((:var-decl :name "varName"
                                    :initializer (:fn-call :fn (:identifier :name "func")
                                                          :args ((:numeric-literal :value 8))))))))
(deftest parse-fn-call/2 :notes parser
  (parse-to-plist "fcn ( arg );")
  ((:fn-call :fn (:identifier :name "fcn")
             :args ((:identifier :name "arg")))))

(deftest parse-fn-call-and-nested-property-access/1 :notes parser
  (parse-to-plist "var varName=func(0x8, 'str')['sam'].a;")
  ((:var-decl-statement :var-decls
                        ((:var-decl :name "varName"
                                    :initializer
                                    (:property-access
                                     :target
                                     (:property-access 
                                      :target
                                      (:fn-call :fn (:identifier :name "func")
                                                :args ((:numeric-literal :value 8) (:string-literal :value "str")))
                                      :field (:string-literal :value "sam"))
                                     :field (:string-literal :value "a")))))))

(deftest parse-binary-operator/1 :notes parser
  (parse-to-plist "var x = 10 * 10 / 20;")
  ((:var-decl-statement :var-decls
                        ((:var-decl :name "x"
                                    :initializer
                                    (:binary-operator :op-symbol :divide
                                                      :left-arg (:binary-operator :op-symbol :multiply
                                                                                  :left-arg (:numeric-literal :value 10)
                                                                                  :right-arg (:numeric-literal :value 10))
                                                      :right-arg (:numeric-literal :value 20)))))))
(deftest parse-binary-operator/2 :notes parser
     (parse-to-plist "var x = 10 * 2 + 3;")
     ((:var-decl-statement :var-decls
                           ((:var-decl :name "x"
                                       :initializer
                                       (:binary-operator :op-symbol :add
                                                         :left-arg (:binary-operator :op-symbol :multiply
                                                                                     :left-arg (:numeric-literal :value 10)
                                                                                     :right-arg (:numeric-literal :value 2))
                                                         :right-arg (:numeric-literal :value 3)))))))
(deftest parse-binary-operator/3 :notes parser
  (parse-to-plist "var x = 3+10 * 2 ;")
  ((:var-decl-statement :var-decls
                        ((:var-decl :name "x"
                                    :initializer
                                    (:binary-operator :op-symbol :add
                                                      :left-arg (:numeric-literal :value 3)
                                                      :right-arg (:binary-operator :op-symbol :multiply
                                                                                   :left-arg (:numeric-literal :value 10)
                                                                                   :right-arg (:numeric-literal :value 2))))))))

(deftest parse-binary-operator/4 :notes parser
  (parse-to-plist "var x = 10 << (99 - 50) * 10;")
  ((:var-decl-statement :var-decls
                        ((:var-decl :name "x"
                                    :initializer
                                    (:binary-operator :op-symbol :lshift
                                                      :left-arg (:numeric-literal :value 10)
                                                      :right-arg (:binary-operator :op-symbol :multiply
                                                                                   :left-arg (:binary-operator :op-symbol :subtract
                                                                                                               :left-arg (:numeric-literal :value 99)
                                                                                                               :right-arg (:numeric-literal :value 50))
                                                                                   :right-arg (:numeric-literal :value 10))))))))
(deftest parse-binary-operator/5 :notes parser
  (parse-to-plist "var x = a & b | c ^ d;")
  ((:var-decl-statement :var-decls
                        ((:var-decl :name "x"
                                    :initializer
                                    (:binary-operator :op-symbol :bitwise-or
                                                      :left-arg (:binary-operator :op-symbol :bitwise-and
                                                                                  :left-arg (:identifier :name "a")
                                                                                  :right-arg (:identifier :name "b"))
                                                      :right-arg (:binary-operator :op-symbol :bitwise-xor
                                                                                   :left-arg (:identifier :name "c")
                                                                                   :right-arg(:identifier :name "d"))))))))

(deftest parse-binary-operator/6 :notes parser
  (parse-to-plist "var x = a && b || c && d;")
  ((:var-decl-statement :var-decls
                        ((:var-decl :name "x"
                                    :initializer (:binary-operator :op-symbol :logical-or
                                                                   :left-arg (:binary-operator :op-symbol :logical-and
                                                                                               :left-arg (:identifier :name "a")
                                                                                               :right-arg (:identifier :name "b"))
                                                                   :right-arg (:binary-operator :op-symbol :logical-and
                                                                                                :left-arg (:identifier :name "c")
                                                                                                :right-arg (:identifier :name "d"))))))))
    
(deftest parse-binary-operator/7 :notes parser
  (parse-to-plist "x == y;")
  ((:binary-operator :op-symbol :equals
                     :left-arg (:identifier :name "x")
                     :right-arg (:identifier :name "y"))))

(deftest parse-binary-operator/8 :notes parser
  (parse-to-plist "x = y;")
  ((:binary-operator :op-symbol :assign
                     :left-arg (:identifier :name "x")
                     :right-arg(:identifier :name "y"))))


(deftest parse-binary-operator/9 :notes parser
  (parse-to-plist "x += 50;")
  ((:binary-operator :op-symbol :plus-equals
                     :left-arg (:identifier :name "x")
                     :right-arg (:numeric-literal :value 50))))

(deftest parse-unary-operator/1 :notes parser
  (parse-to-plist "delete x++;")
  ((:unary-operator :op-symbol :delete
                    :arg (:unary-operator :op-symbol :post-incr
                                          :arg (:identifier :name "x")))))

(deftest parse-return-statement/1 :notes parser
  (parse-to-plist "return;")
  ((:return-statement :arg nil)))
     
(deftest parse-return-statement/2 :notes parser
  (parse-to-plist "return 8 >> 2;")
  ((:return-statement :arg (:binary-operator :op-symbol :rshift
                                             :left-arg (:numeric-literal :value 8)
                                             :right-arg (:numeric-literal :value 2)))))

(deftest parse-continue-statement/1 :notes parser
  (parse-to-plist "continue;")
  ((:continue-statement :label nil)))

(deftest parse-continue-statement/2 :notes parser
  (parse-to-plist "continue blockName;")
  ((:continue-statement :label "blockName")))

(deftest parse-break-statement/1 :notes parser
  (parse-to-plist "break blockName;")
  ((:break-statement :label "blockName")))

(deftest parse-with/1 :notes parser
  (parse-to-plist "with(x.y) { z -= 10; }")
  ((:with :scope-object (:property-access :target (:identifier :name "x")
                                          :field (:string-literal :value "y"))
          :body (:statement-block :statements
                                  ((:binary-operator :op-symbol :minus-equals
                                                     :left-arg (:identifier :name "z")
                                                     :right-arg (:numeric-literal :value 10)))))))

(deftest parse-switch/1 :notes parser
  (parse-to-plist "switch(x[y]) { case 10:case 20:return x << 1;default:case 88:break; }")
  ((:switch :value (:property-access :target (:identifier :name "x")
                                     :field (:identifier :name "y"))
            :clauses
            ((:case-clause :label (:numeric-literal :value 10)
                           :body nil)
             (:case-clause :label (:numeric-literal :value 20)
                           :body ((:return-statement :arg (:binary-operator :op-symbol :lshift
                                                                            :left-arg (:identifier :name "x")
                                                                            :right-arg (:numeric-literal :value 1)))))
             (:default-clause :body nil)
             (:case-clause :label (:numeric-literal :value 88)
                           :body ((:break-statement :label nil)))))))

(deftest parse-statement-block/1 :notes parser
  (parse-to-plist "{hello: x += 20;x*=10;}")
  ((:statement-block :statements ((:label "hello" :statement (:binary-operator :op-symbol :plus-equals
                                                                               :left-arg (:identifier :name "x")
                                                                               :right-arg (:numeric-literal :value 20)))
                                  (:binary-operator :op-symbol :times-equals
                                                    :left-arg (:identifier :name "x")
                                                    :right-arg (:numeric-literal :value 10))))))
(pushnew 'parse-statement-block/1 rtest::*expected-failures*)

(deftest parse-throw-statement/1 :notes parser
  (parse-to-plist "throw -1;")
  ((:throw-statement :value (:unary-operator :op-symbol :unary-minus
                                             :arg (:numeric-literal :value 1)))))

(deftest parse-try/1 :notes parser
  (parse-to-plist "try { throw x++; } catch(y) {return y;}")
  ((:try
    :body (:statement-block :statements
                            ((:throw-statement :value (:unary-operator :op-symbol :post-incr
                                                                      :arg (:identifier :name "x")))))
    :catch-clause (:catch-clause :binding "y"
                                 :body (:statement-block :statements
                                                        ((:return-statement :arg (:identifier :name "y")))))
    :finally-clause nil)))

(deftest parse-try/2 :notes parser
  (parse-to-plist "try {throw 10;} finally {delete x;delete y;}")
  ((:try
    :body (:statement-block :statements
                            ((:throw-statement :value (:numeric-literal :value 10))))
    :catch-clause nil
    :finally-clause (:finally-clause :body (:statement-block :statements
                                                             ((:unary-operator :op-symbol :delete
                                                                               :arg (:identifier :name "x"))
                                                              (:unary-operator :op-symbol :delete
                                                                               :arg (:identifier :name "y"))))))))

(deftest parse-try/3 :notes parser
  (parse-to-plist "try {func(x);} catch(e) {} finally {delete x;}")
  ((:try
    :body (:statement-block :statements
                            ((:fn-call :fn (:identifier :name "func")
                                       :args ((:identifier :name "x")))))
    :catch-clause (:catch-clause :binding "e" :body (:statement-block :statements nil))
    :finally-clause (:finally-clause :body (:statement-block :statements
                                                            ((:unary-operator :op-symbol :delete
                                                                              :arg (:identifier :name "x"))))))))
  
  
(deftest parse-if/1 :notes parser
  (parse-to-plist "if(x == 10) {y=100;} else {y = null;}")
  ((:if-statement :condition (:binary-operator :op-symbol :equals
                                               :left-arg (:identifier :name "x")
                                               :right-arg (:numeric-literal :value 10))
                  :then-statement (:statement-block :statements
                                                    ((:binary-operator :op-symbol :assign
                                                                       :left-arg (:identifier :name "y")
                                                                       :right-arg (:numeric-literal :value 100))))
                  :else-statement (:statement-block :statements
                                                    ((:binary-operator :op-symbol :assign
                                                                       :left-arg (:identifier :name "y")
                                                                       :right-arg (:special-value :symbol :null)))))))

(deftest parse-function-decl/1 :notes parser
  (parse-to-plist "function foo(x) { if(x == 20) return 10; return 55;}")
  ((:function-decl :name "foo"
                   :parameters ("x")
                   :body  ((:if-statement :condition
                                          (:binary-operator :op-symbol :equals
                                                            :left-arg (:identifier :name "x")
                                                            :right-arg (:numeric-literal :value 20))
                                          :then-statement
                                          (:return-statement :arg (:numeric-literal :value 10))
                                          :else-statement
                                          nil)
                           (:return-statement :arg (:numeric-literal :value 55))))))

(deftest parse-function-decl-and-toplevel-call/1 :notes parser
  (parse-to-plist "function make_adder(n) { return function(x) { return x + n;};} make_adder(20);")
  ((:function-decl :name "make_adder"
                   :parameters ("n")
                   :body ((:return-statement :arg (:function-expression
                                                   :name nil
                                                   :parameters ("x")
                                                   :body ((:return-statement :arg (:binary-operator :op-symbol :add
                                                                                                    :left-arg (:identifier :name "x")
                                                                                                    :right-arg (:identifier :name "n"))))))))
   (:fn-call :fn (:identifier :name "make_adder")
             :args ((:numeric-literal :value 20)))))

(deftest parse-function-expression/1 :notes parser
  (parse-to-plist "var x = function f(n) { if(n > 0) return n*f(n-1); else return 1;};")
  ((:var-decl-statement :var-decls
                        ((:var-decl :name "x"
                                    :initializer (:function-expression
                                                  :name "f"
                                                  :parameters ("n")
                                                  :body ((:if-statement
                                                          :condition
                                                          (:binary-operator :op-symbol :greater-than
                                                                            :left-arg (:identifier :name "n")
                                                                            :right-arg (:numeric-literal :value 0))
                                                          :then-statement
                                                          (:return-statement :arg (:binary-operator :op-symbol :multiply
                                                                                                    :left-arg (:identifier :name "n")
                                                                                                    :right-arg (:fn-call :fn (:identifier :name "f")
                                                                                                                         :args ((:binary-operator :op-symbol :subtract
                                                                                                                                                  :left-arg (:identifier :name "n")
                                                                                                                                                  :right-arg (:numeric-literal :value 1))))))
                                                          :else-statement
                                                          (:return-statement :arg (:numeric-literal :value 1))))))))))

(deftest parse-function-expression/2 :notes parser
  (parse-to-plist "function(n) { if(n > 0) return n*f(n-1); else return 1;};")
  ((:function-expression :name nil
                         :parameters ("n")
                         :body ((:if-statement
                                 :condition
                                 (:binary-operator :op-symbol :greater-than
                                                   :left-arg (:identifier :name "n")
                                                   :right-arg (:numeric-literal :value 0))
                                 :then-statement
                                 (:return-statement :arg (:binary-operator :op-symbol :multiply
                                                                           :left-arg (:identifier :name "n")
                                                                           :right-arg (:fn-call :fn (:identifier :name "f")
                                                                                                :args ((:binary-operator :op-symbol :subtract
                                                                                                                         :left-arg (:identifier :name "n")
                                                                                                                         :right-arg (:numeric-literal :value 1))))))
                                 :else-statement
                                 (:return-statement :arg (:numeric-literal :value 1)))))))

