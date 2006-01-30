;;;; test-utils.lisp
;;;
;;; Some helper functions for testing in general (i.e., that are
;;; not specific to testing a certain file).

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
  (make-fn-call :fn (jw::fn-call-fn elm)
                :args (jw::fn-call-args elm)))

(defun test-transform (xform elm)
  "Return the results of applying XFORM to ELM with any administrative source-elements
   converted to their non-administrative equivalents"
  (transform 'remove-administratives
             (transform xform elm)))


;;TODO Automated benchmarks?
;;TODO Randomized testing?  (a la Quickcheck)
;;TODO Coverage checks?
