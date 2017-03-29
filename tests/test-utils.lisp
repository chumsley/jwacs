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

(defmacro expect-error (form &optional (condition-type 'error))
  "If evaluating FORM results in an error being raised, returns non-NIL.
   If the CONDITION-TYPE argument is provided, non-NIL is raised only if
   an error of that specific type is raised."
  (let ((gret (gensym))
        (gerr (gensym)))
    `(multiple-value-bind (,gret ,gerr)
        (ignore-errors ,form)
      (declare (ignore ,gret))
      (or (typep ,gerr ',condition-type)
          ,gerr))))

;;; The REMOVE-POSITIONS transformation strips source positions from a source
;;; tree.  TEST-PARSE and TEST-TRANSFORM both use it so that we can check
;;; generated Javascript code in unit tests without having to ensure that we
;;; provide the correct source positions (which is in many cases impossible,
;;; since transformation often moves source elements to different parts of the
;;; code while preserving their original position information).
(defmethod transform ((xform (eql 'remove-positions)) (elm source-element))
  (apply
   (get-constructor elm)
   (loop for slot in (structure-slots elm)
         collect (make-keyword slot)
         collect (if (or (eq slot 'jw::start) (eq slot 'jw::end))
                     nil
                     (transform xform (slot-value elm slot))))))

(defmethod transform ((xform (eql 'remove-positions)) (elm object-literal))
  (make-object-literal
   :properties
   (loop for (prop-name . prop-value) in (jw::object-literal-properties elm)
         collect (cons
                  (transform xform prop-name)
                  (transform xform prop-value)))))

(defun test-parse (str)
  "Parse STR into a source model representation that does not include source positions"
  (let ((elm (parse str)))
    (transform 'remove-positions elm)))

;;; The REMOVE-ADMINISTRATIVES transformation translates administrative
;;; source-elements (such as CONTINUATION-FUNCTIONs) to their non-administrative
;;; equivalents (eg FUNCTION-EXPRESSION).  This transformation is used by the
;;; TEST-TRANSFORM function to ensure that the results of a transformation are
;;; the same as what would be parsed from their pretty-printed representation
;;; (so that we can write unit tests by providing JWACS code instead of ASTs).
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
   converted to their non-administrative equivalents and with source positions removed."
  (transform 'remove-administratives
             (transform 'remove-positions
                        (transform xform elm))))

;;; compilation helpers

(defun compile-lang-tests (&key debug-mode)
  "Compile the language tests"
  (let* ((jw::*debug-mode* debug-mode)
         (module (asdf:find-component (asdf:find-system :jwacs-tests) "tests"))
         (component (asdf:find-component module "lang-tests")))
    (jw::build-app (asdf:component-pathname component))))

(defun compile-examples (&key (compress-mode t) (combine-mode t))
  "Compiles all the examples"
  (let ((lib-pathname (asdf:system-relative-pathname :jwacs "lib/"))
        (examples-pathname (asdf:system-relative-pathname :jwacs "examples/")))
    (flet ((build-ex (name)
             (jw:build-app (uiop:subpathname examples-pathname name)
                           :prefix-lookup `(("/lib/" . ,lib-pathname))
                           :compress-mode compress-mode
                           :combine-mode combine-mode)))
      (list
       (build-ex "CalendarMark2.jw")
       (build-ex "Counter.jw")
       (build-ex "TrivialHttpRequest.jw")))))

;;TODO Automated benchmarks?
;;TODO Randomized testing?  (a la Quickcheck)
;;TODO Coverage checks?
