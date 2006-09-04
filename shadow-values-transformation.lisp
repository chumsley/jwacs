;;;; shadow-values-transformation.lisp
;;;
;;; Defines the shadow-values transformation, which replaces references to
;;; `this` and `arguments` (which don't behave as expected when you're at
;;; the bottom of three levels of continuation) with references to "shadow
;;; values", which are variable that have been set to point to the /correct/
;;; versions of `this` and arguments at the beginning of a function.
;;;
;;; Copyright (c) 2006 James Wright
;;; See LICENSE for full licensing details.
;;;
(in-package :jw)

(defparameter *shadowed-this-name* nil
  "Name of the variable that currently shadows `this`")

(defparameter *shadowed-arguments-name* nil
  "Name of the variable that currently shadows `arguments`")

(defparameter *arguments-name* "arguments"
  "Name of the `arguments` identifier")

(defmethod transform ((xform (eql 'shadow-values)) (elm identifier))
  (if (and *shadowed-arguments-name*
           (equal (identifier-name elm) *arguments-name*))
    (make-identifier :name *shadowed-arguments-name*
                     :start (source-element-start elm)
                     :end (source-element-end elm))
    (call-next-method)))

(defmethod transform ((xform (eql 'shadow-values)) (elm special-value))
  (if (and *shadowed-this-name*
           (eq :this (special-value-symbol elm)))
    (make-identifier :name *shadowed-this-name*
                     :start (source-element-start elm)
                     :end (source-element-end elm))
    (call-next-method)))

(defun tx-shadow-values-body (body)
  "Transforms the body of a function expression or declaration and returns
   the new body"
  (let* ((declares-arguments-p (find *arguments-name*
                                     (collect-in-scope body 'var-decl)
                                     :key 'var-decl-name :test 'equal))
         (references-arguments-p (find *arguments-name*
                                       (collect-in-scope body 'identifier)
                                       :key 'identifier-name :test 'equal))
         (references-this-p (find :this
                                  (collect-in-scope body 'special-value)
                                  :key 'special-value-symbol :test 'eq))
         (*shadowed-arguments-name* (if (and references-arguments-p (not declares-arguments-p))
                                      (genvar "arguments")
                                      nil))
         (*shadowed-this-name* (if references-this-p
                                 (genvar "this")
                                 nil)))
    (cond

      ((and references-arguments-p (not declares-arguments-p)
            references-this-p)
       (append
        (list (make-var-init *shadowed-arguments-name* (make-special-value :symbol :arguments))
              (make-var-init *shadowed-this-name* (make-special-value :symbol :this)))
        (transform 'shadow-values body)))

      ((and references-arguments-p (not declares-arguments-p))
       (cons (make-var-init *shadowed-arguments-name* (make-special-value :symbol :arguments))
             (transform 'shadow-values body)))

      (references-this-p
       (cons (make-var-init *shadowed-this-name* (make-special-value :symbol :this))
             (transform 'shadow-values body)))

      (t
       (transform 'shadow-values body)))))
  

(defmethod transform ((xform (eql 'shadow-values)) (elm function-decl))
  (make-function-decl :name (function-decl-name elm)
                      :parameters (function-decl-parameters elm)
                      :body (tx-shadow-values-body (function-decl-body elm))
                      :start (source-element-start elm)
                      :end (source-element-end elm)))

(defmethod transform ((xform (eql 'shadow-values)) (elm function-expression))
  (make-function-expression :name (function-expression-name elm)
                            :parameters (function-expression-parameters elm)
                            :body (tx-shadow-values-body (function-expression-body elm))
                            :start (source-element-start elm)
                            :end (source-element-end elm)))