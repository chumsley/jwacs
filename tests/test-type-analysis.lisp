;;;; test-type-analysis.lisp
;;;
;;; Unit tests and benchmarks for type analysis functionality
(in-package :jw-tests)

(defnote type-analysis "tests for the type-analysis functionality")

(defun type-names (type-node-list)
  (sort (copy-list 
         (mapcar 'jw::type-node-name type-node-list))
        'string<))

(deftest type-analysis/simple-assignment/1 :notes type-analysis
  (type-names
   (compute-types #s(identifier :name "x")
                  (type-analyze (parse "x = 5 / '2.5'; y = 'str'; y = x;"))))
  ("Number"))

(deftest type-analysis/simple-assignment/2 :notes type-analysis
  (type-names
   (compute-types #s(identifier :name "y")
                  (type-analyze (parse "x = 5 / '2.5'; y = 'str'; y = x;"))))
   ("Number" "String"))

(deftest type-analysis/var-decl/1 :notes type-analysis
    (type-names
     (compute-types #s(identifier :name "y")
                    (type-analyze (parse "var x; x = 5 / '2.5'; y = 'str'; y = x;"))))
    ("Number" "String" "undefined"))

(deftest type-analysis/var-decl/2 :notes type-analysis
  (type-names
   (compute-types #s(identifier :name "y")
                  (type-analyze (parse "var x = 5 / '2.5', y = 'str'; y = x;"))))
  ("Number" "String"))

(deftest type-analysis/function-parameters/1 :notes type-analysis
  (type-names
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
     }"))))
  ("Number"))

(deftest type-analysis/function-parameters/2 :notes type-analysis
  (type-names
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
     }"))))
  ("String" "undefined"))

(deftest type-analysis/function-return/1 :notes type-analysis
  (type-names
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
     }"))))
  ("Number"))

(deftest type-analysis/function-return/2 :notes type-analysis
  (type-names
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
     }"))))
  ("Number" "String" "undefined"))

(deftest type-analysis/property-access/1 :notes type-analysis
  (type-names
   (compute-types
    #s(property-access :target #s(identifier :name "x") :field #s(string-literal :value "foo"))
    (type-analyze (parse "
     var x = new Object;
     x.foo = 20;"))))
  ("Number"))

(deftest type-analysis/property-access/2 :notes type-analysis
  (type-names
   (compute-types
    #s(property-access :target #s(identifier :name "x") :field #s(string-literal :value "foo"))
    (type-analyze (parse "
     var x = new Object;
     x.foo = 20;
     var y = x;
     y.foo = 'str';"))))
  ("Number" "String"))

(deftest type-analysis/property-access/object-literals/1 :notes type-analysis
  (type-names
   (compute-types
    #s(property-access :target #s(property-access :target #s(identifier :name "x")
                                                  :field #s(string-literal :value "foo"))
                       :field #s(string-literal :value "a"))
    (type-analyze (parse "
     var x = new Object;
     x.foo = {a: null, b: 20};"))))
  ("null"))

(deftest type-analysis/property-access/object-literals/2 :notes type-analysis
  (type-names
   (compute-types
    #s(property-access :target #s(property-access :target #s(identifier :name "x")
                                                  :field #s(string-literal :value "foo"))
                       :field #s(string-literal :value "b"))
    (type-analyze (parse "
     var x = new Object;
     x.foo = {a: null, b: 20};"))))
  ("Number"))

(deftest type-analysis/property-access/function-calls/1 :notes type-analysis
  (type-names
   (compute-types
    #s(property-access :target #s(identifier :name "y")
                       :field #s(string-literal :value "a"))
    (type-analyze (parse "
     x.foo = function() { return {a: null, b: 20}; };
     y = x.foo();"))))
  ("null"))

(deftest type-analysis/property-access/function-calls/2 :notes type-analysis
  (type-names
   (compute-types
    #s(property-access :target #s(identifier :name "y")
                       :field #s(string-literal :value "a"))
    (type-analyze (parse "
     x.foo = function() { return {a: null, b: 20}; };
     y = x.foo();"))))
  ("null"))

;; JRW These two will work once I've added find-node support for function-expressions,
;; which I won't do until after the refactoring.
(flag-expected-failure 'type-analysis/property-access/function-expressions/1)
(flag-expected-failure 'type-analysis/property-access/function-expressions/2)

(deftest type-analysis/property-access/function-expressions/1 :notes type-analysis
  (type-names
   (compute-types
    #s(property-access :target  #s(fn-call :fn #s(property-access :target #s(identifier :name "x")
                                                                  :field #s(string-literal :value "foo"))
                                           :args nil)
                       :field #s(string-literal :value "a"))
    (type-analyze (parse "
     x.foo = function() { return {a: null, b: 20}; };"))))
  ("null"))

(deftest type-analysis/property-access/function-expressions/2 :notes type-analysis
  (type-names
   (compute-types
    #s(property-access :target  #s(fn-call :fn #s(property-access :target #s(identifier :name "x")
                                                                  :field #s(string-literal :value "foo"))
                                           :args nil)
                       :field #s(string-literal :value "non-existo"))
    (type-analyze (parse "
     x.foo = function() { return {a: null, b: 20}; };"))))
  ("undefined"))

(deftest type-analysis/cycle/1 :notes type-analysis
  (type-names
   (compute-types
    #s(identifier :name "w")
    (type-analyze (parse "
     x = 50;
     x = y;
     y = 'str';
     y = z;
     z = new Array;
     z = w;
     w = new Object;
     w = x;"))))
  ("Array" "Number" "Object" "String"))

(deftest type-analysis/cycle/2 :notes type-analysis
  (length
   (jw::value-node-assignments
    (gethash "x" (type-analyze (parse "
                     x = 50;
                     x = y;
                     y = 'str';
                     y = z;
                     z = new Array;
                     z = w;
                     w = new Object;
                     w = x;")))))
  4)

(defun benchmark/multi-property-cycle (fn &optional (n 1000) (repetitions 10))
  "Constructs a pathological case for collapsing property-accesses:
   a large cycle of value nodes, each of which has a property 'foo'
   that points to the next node.

   Times the application of FN to a cycle of N nodes REPETITIONS times."
  (let ((ast (append (parse "x0.foo = x1; x0 = x1;")
                      (loop for idx from 1 upto (1- n)
                            append (parse (format nil "x~D.foo = x~D; x~D = x~d;" idx (1+ idx) idx (1+ idx))))
                      (parse (format nil "x~D = x0; x~D.foo = x0;" n n))))
        (data (make-array repetitions)))
    (flet ((make-graph (idx)
             (let ((jw::*type-graph* (make-hash-table :test 'equal)))
               (jw::internal-analyze ast)
               (setf (aref data idx)
                     jw::*type-graph*)))
           (process-data ()
             (loop for idx from 0 to (1- repetitions)
                   do (funcall fn (aref data idx)))))
      (loop for idx from 0 to (1- repetitions)
            do (make-graph idx))
      (time (process-data)))))
    
  
                   
     
