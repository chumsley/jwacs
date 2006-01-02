;;;; test-source-transformation.lisp
;;;
;;; Tests for the source transformations.

(in-package :jwacs-tests)

;;;; Test categories 
(defnote source-transformations "tests for the source-transformations")
    
;;;; Tests 

;;;; General traversal behaviour 
(defmethod transform ((xform (eql 'hello)) (elm string))
    "hello there!")

(deftest source-transformations/general-behaviour/1 :notes source-transformations
  (transform 'hello #S(continue-statement :label "go away!"))
  #S(continue-statement :label "hello there!"))

(deftest source-transformations/general-behaviour/2 :notes source-transformations
  (transform 'hello '("string 1" symbol ("string 2")))
  ("hello there!" symbol ("hello there!")))
