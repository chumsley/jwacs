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
