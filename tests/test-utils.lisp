;;;; test-utils.lisp
;;;
;;; Some helper functions for testing in general (i.e., that are
;;; not specific to testing a certain file).
;;;
;;; Copyright (c) 2005 James Wright
;;; See LICENSE for full licensing details.
;;;
(in-package :jw-tests)

(defun flag-expected-failure (test-name)
  "Add a test to the list of expected failures"
  (pushnew test-name rtest::*expected-failures*))

;;; The REMOVE-ADMINISTRATIVES transformation translates administrative source-elements
;;; (such as CONTINUATION-FUNCTIONs) to their non-administrative equivalents (eg FUNCTION-EXPRESSION).
;;; This transformation is used by the TEST-TRANSFORM function to ensure that the results of a
;;; transformation are the same as what would be parsed from their pretty-printed representation
;;; (so that we can write unit tests by providing JWACS code instead of ASTs)
(defmethod transform ((xform (eql 'remove-administratives)) (elm thunk-function))
  (make-function-expression :name (jw::function-expression-name elm)
                            :parameters (jw::function-expression-parameters elm)
                            :body (transform xform (jw::function-expression-body elm))))

(defmethod transform ((xform (eql 'remove-administratives)) (elm continuation-function))
  (make-function-expression :name (jw::function-expression-name elm)
                            :parameters (jw::function-expression-parameters elm)
                            :body (transform xform (jw::function-expression-body elm))))

(defmethod transform ((xform (eql 'remove-administratives)) (elm continuation-call))
  (make-fn-call :fn (transform xform (jw::fn-call-fn elm))
                :args (transform xform (jw::fn-call-args elm))))

(defmethod transform ((xform (eql 'remove-administratives)) (elm special-value))
  (if (eq :arguments (jw::special-value-symbol elm))
    (make-identifier :name jw::*arguments-name*)
    (call-next-method)))

(defmethod transform ((xform (eql 'remove-administratives)) (elm add-handler))
  (make-fn-call :fn (make-identifier :name "$addHandler")
                :args (list (transform xform (jw::add-handler-handler elm))
                            (transform xform (jw::make-function-expression
                                              :body (jw::add-handler-thunk-body elm))))))

(defmethod transform ((xform (eql 'remove-administratives)) (elm remove-handler))
  (make-fn-call :fn (make-identifier :name "$removeHandler")
                :args (list (transform xform (jw::remove-handler-handler elm))
                            (transform xform (jw::make-function-expression
                                              :body (jw::remove-handler-thunk-body elm))))))

(defun test-transform (xform elm)
  "Return the results of applying XFORM to ELM with any administrative source-elements
   converted to their non-administrative equivalents"
  (transform 'remove-administratives
             (transform xform elm)))

(defun compile-lang-tests ()
  "Compile the language tests"
  (let* ((module (asdf:find-component (asdf:find-system :jwacs-tests) "tests"))
         (component (asdf:find-component module "lang-tests")))
    (jw::build-app (asdf:component-pathname component))))
    
(defun compile-examples ()
  "Compiles all the examples"
  (let* ((sys-pathname (truename (asdf:system-definition-pathname (asdf:find-system :jwacs))))
         (lib-pathname (merge-pathnames (make-pathname :directory '(:relative "lib")
                                                       :name :unspecific :type :unspecific :version :unspecific)
                                        sys-pathname))
         (examples-pathname (merge-pathnames (make-pathname :directory '(:relative "examples")
                                                            :name :unspecific :type :unspecific :version :unspecific)
                                        sys-pathname)))
    (flet ((build-ex (name)
             (jw:build-app (merge-pathnames name examples-pathname) :prefix-lookup `(("/lib/" . ,lib-pathname)))))

      (build-ex "CalendarMark2.jw")
      (build-ex "Counter.jw")
      (build-ex "TrivialHttpRequest.jw"))))

;;TODO Automated benchmarks?
;;TODO Randomized testing?  (a la Quickcheck)
;;TODO Coverage checks?
