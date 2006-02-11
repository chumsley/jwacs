;;;; test-type-analysis.lisp
;;;
;;; Unit tests for type analysis functionality
(in-package :jw-tests)

(defnote type-analysis "tests for the type-analysis functionality")

(deftest type-analysis/simple-assignment/1 :notes type-analysis
  (compute-types #s(identifier :name "x")
                 (type-analyze (parse "x = 5 / '2.5'; y = 'str'; y = x;")))
  (#s(type-node :name "Number")))

(deftest type-analysis/simple-assignment/2 :notes type-analysis
  (compute-types #s(identifier :name "y")
                 (type-analyze (parse "x = 5 / '2.5'; y = 'str'; y = x;")))
  (#s(type-node :name "Number") #s(type-node :name "String")))

(deftest type-analysis/var-decl/1 :notes type-analysis
  (compute-types #s(identifier :name "y")
                 (type-analyze (parse "var x; x = 5 / '2.5'; y = 'str'; y = x;")))
  (#s(type-node :name "Number") #s(type-node :name "undefined") #s(type-node :name "String")))

(deftest type-analysis/var-decl/2 :notes type-analysis
  (compute-types #s(identifier :name "y")
                 (type-analyze (parse "var x = 5 / '2.5', y = 'str'; y = x;")))
  (#s(type-node :name "Number") #s(type-node :name "String")))

(deftest type-analysis/function-parameters/1 :notes type-analysis
  (compute-types
   #s(identifier :name "a")
   (type-analyze (parse "
     function foo()
     {
       var x = 20;
       var y = 'str';
       return bar(x, y);
     }

     function bar(a, b)
     {
       return a + b;
     }")))
  (#s(type-node :name "Number")))

(deftest type-analysis/function-parameters/2 :notes type-analysis
  (compute-types
   #s(identifier :name "b")
   (type-analyze (parse "
     function foo()
     {
       var x = 20;
       var y = 'str';
       bar(x);
       return bar(x, y);
     }

     function bar(a, b)
     {
       return a + b;
     }")))
  (#s(type-node :name "String") #s(type-node :name "undefined")))

(deftest type-analysis/function-return/1 :notes type-analysis
  (compute-types
   #s(identifier :name "x")
   (type-analyze (parse "
     function foo()
     {
       var x = bar(10);
     }

     function bar(a)
     {
       return a;
     }")))
  (#s(type-node :name "Number")))

(deftest type-analysis/function-return/2 :notes type-analysis
  (compute-types
   #s(identifier :name "x")
   (type-analyze (parse "
     function foo()
     {
       var x = bar(10);
       var y = bar('str');
       bar();
     }

     function bar(a)
     {
       return a;
     }")))
  (#s(type-node :name "String") #s(type-node :name "Number") #s(type-node :name "undefined")))

(deftest type-analysis/property-access/1 :notes type-analysis
  (compute-types
   #s(property-access :target #s(identifier :name "x") :field #s(string-literal :value "foo"))
   (type-analyze (parse "
     x.foo = 20;")))
  (#s(type-node :name "Number")))

(deftest type-analysis/property-access/2 :notes type-analysis
  (compute-types
   #s(property-access :target #s(identifier :name "x") :field #s(string-literal :value "foo"))
   (type-analyze (parse "
     var x = new Object;
     x.foo = 20;
     var y = x;
     y.foo = 'str';")))
  (#s(type-node :name "String") #s(type-node :name "Number")))

(deftest type-analysis/property-access/object-literals/1 :notes type-analysis
  (compute-types
   #s(property-access :target #s(property-access :target #s(identifier :name "x")
                                                 :field #s(string-literal :value "foo"))
                      :field #s(string-literal :value "a"))
   (type-analyze (parse "
     x.foo = {a: null, b: 20};")))
  (#s(type-node :name "null")))

(deftest type-analysis/property-access/object-literals/2 :notes type-analysis
  (compute-types
   #s(property-access :target #s(property-access :target #s(identifier :name "x")
                                                 :field #s(string-literal :value "foo"))
                      :field #s(string-literal :value "b"))
   (type-analyze (parse "
     x.foo = {a: null, b: 20};")))
  (#s(type-node :name "Number")))

(deftest type-analysis/property-access/function-calls/1 :notes type-analysis
  (compute-types
   #s(property-access :target #s(identifier :name "y")
                      :field #s(string-literal :value "a"))
   (type-analyze (parse "
     x.foo = function() { return {a: null, b: 20}; };
     y = x.foo();")))
  (#s(type-node :name "null")))

(deftest type-analysis/property-access/function-calls/2 :notes type-analysis
  (compute-types
   #s(property-access :target #s(identifier :name "y")
                      :field #s(string-literal :value "a"))
   (type-analyze (parse "
     x.foo = function() { return {a: null, b: 20}; };
     y = x.foo();")))
  (#s(type-node :name "null")))

;; JRW These two will work once I've added find-node support for function-expressions,
;; which I won't do until after the refactoring.
(flag-expected-failure 'type-analysis/property-access/function-expressions/1)
(flag-expected-failure 'type-analysis/property-access/function-expressions/2)

(deftest type-analysis/property-access/function-expressions/1 :notes type-analysis
  (compute-types
   #s(property-access :target  #s(fn-call :fn #s(property-access :target #s(identifier :name "x")
                                                                 :field #s(string-literal :value "foo"))
                                          :args nil)
                      :field #s(string-literal :value "a"))
   (type-analyze (parse "
     x.foo = function() { return {a: null, b: 20}; };")))
  (#s(type-node :name "null")))

(deftest type-analysis/property-access/function-expressions/2 :notes type-analysis
  (compute-types
   #s(property-access :target  #s(fn-call :fn #s(property-access :target #s(identifier :name "x")
                                                                 :field #s(string-literal :value "foo"))
                                          :args nil)
                      :field #s(string-literal :value "non-existo"))
   (type-analyze (parse "
     x.foo = function() { return {a: null, b: 20}; };")))
  (#s(type-node :name "undefined")))


  