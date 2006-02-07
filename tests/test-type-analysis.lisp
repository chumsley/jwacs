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
