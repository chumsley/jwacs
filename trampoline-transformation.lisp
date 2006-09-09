;;;; trampoline-transformation.lisp
;;;
;;; Define the transformation that converts cps-form Javascript source
;;; into trampolined cps-form Javascript.
;;;
;;; Copyright (c) 2006 James Wright
;;; See LICENSE for full licensing details.
;;;
(in-package :jwacs)

;;;; ======= Trampoline transformation =============================================================
;;;
;;; 1. Every return statement whose argument is a function call
;;;    to a trampoline-style function is transformed into a trampolining
;;;    return statement whose `done` field is `false` and whose `thunk`
;;     field is a function that contains the original return statement.
;;; 2. Every return statement whose argument is /not/ a function call
;;;    is transformed into a trampolining return statement whose `done`
;;;    field is `true` and whose `result` field contains the original
;;;
;;; The return continuation for a function to convert to trampoline form
;;; is assumed to already be in trampoline form.
;;;
;;; It is assumed that all calls to trampoline-style functions are
;;; tail calls; the above two rules are obviously not sufficient when
;;; this condition doesn't hold.

;;;; ======= Helpers ===============================================================================
(defparameter *thunk-prop* (make-string-literal :value "thunk")
  "property name for the thunk field of a boxed result object")

(defparameter *done-prop* (make-string-literal :value "done")
  "property name for the done field of a boxed result object")

(defparameter *result-prop* (make-string-literal :value "result")
  "property name for the result field of a boxed result object")

(defparameter *add-handler-prop* (make-string-literal :value "addHandler")
  "property name for the 'add exception handler to stack' operation field of a boxed result object")

(defparameter *remove-handler-prop* (make-string-literal :value "removeHandler")
  "property name for the 'remove exception handler from stack' operation field of a boxed result object")

(defparameter *replace-handler-stack-prop* (make-string-literal :value "replaceHandlers")
  "property name for the 'replace exception handler stack' operation field of a boxed result object")

(defparameter *start-pos-prop* (make-string-literal :value "startPos")
  "property name for the start-position debug property on boxed thunks")

(defparameter *end-pos-prop* (make-string-literal :value "endPos")
  "property name for the end-position debug property on boxed thunks")

(defparameter *handler-stack-k-prop* (make-string-literal :value "$exHandlers")
  "Name of the property on continuations that contains the handler stack to use")

(defparameter *handler-stack-var-name* "$e"
  "standard variable name for storing the current handler stack")

(defvar *debug-mode* nil
  "When true, we pack more information into each boxed thunk")

(defparameter *debug-eval-var-name* "$localEvalArg"
  "Name of the parameter containing an expression to be evaluated locally for debug-mode thunks")

(defparameter *debug-eval-boilerplate*
  (make-if-statement :condition (make-identifier :name *debug-eval-var-name*)
                     :then-statement (make-return-statement
                                      :arg (make-continuation-call :fn (make-identifier :name "$id")
                                                                   :args (list (make-continuation-call :fn (make-identifier :name "eval")
                                                                                                       :args (list (make-identifier :name *debug-eval-var-name*)))))))
  "Boilerplate code for debug-mode local evaluation")

(defun make-boxed-thunk (body-elm &optional stack-op stack-op-arg)
  "Returns an object literal whose `done` field is `false` and whose
   `thunk` field contains a thunk whose body is BODY-ELM.  When STACK-OP
   is non-NIL, a handler stack operation property will also be added with
   a value of STACK-OP-ARG.  When *DEBUG-MODE* is non-NIL, an `$evalArg`
   parameter is also provided, along with boilerplate code that evaluates
   the argument and then returns when it's present."
  (cond
    ((and *debug-mode* stack-op)
     (make-object-literal :properties (list
                                       (cons stack-op stack-op-arg)
                                       (cons *done-prop* (make-special-value :symbol :false))
                                       (cons *start-pos-prop*
                                             (aif (element-start body-elm)
                                               (make-numeric-literal :value it)
                                               undefined-id))
                                       (cons *end-pos-prop*
                                             (aif (element-end body-elm)
                                               (make-numeric-literal :value it)
                                               undefined-id))
                                       (cons *thunk-prop*
                                             (make-thunk-function :parameters (list *handler-stack-var-name*
                                                                                    *debug-eval-var-name*)
                                                                  :body (combine-statements
                                                                         *debug-eval-boilerplate*
                                                                         body-elm))))))
    (stack-op
     (make-object-literal :properties (list
                                       (cons stack-op stack-op-arg)
                                       (cons *done-prop* (make-special-value :symbol :false))
                                       (cons *thunk-prop*
                                             (make-thunk-function :parameters (list *handler-stack-var-name*)
                                                                  :body (combine-statements body-elm))))))
    (*debug-mode*
     (make-object-literal :properties (list
                                       (cons *start-pos-prop*
                                             (aif (source-element-start body-elm)
                                               (make-numeric-literal :value it)
                                               undefined-id))
                                       (cons *end-pos-prop*
                                             (aif (source-element-end body-elm)
                                               (make-numeric-literal :value it)
                                               undefined-id))
                                       (cons *done-prop* (make-special-value :symbol :false))
                                       (cons *thunk-prop*
                                             (make-thunk-function :parameters (list *handler-stack-var-name*
                                                                                    *debug-eval-var-name*)
                                                                  :body (combine-statements
                                                                         *debug-eval-boilerplate*
                                                                         body-elm))))))
    (t
     (make-object-literal :properties (list
                                       (cons *done-prop* (make-special-value :symbol :false))
                                       (cons *thunk-prop*
                                             (make-thunk-function :parameters (list *handler-stack-var-name*)
                                                                  :body (combine-statements body-elm))))))))
(defun make-boxed-result (elm)
  "Returns an object literal whose `done` field is `true` and whose
   `result` field contains ELM.  If ELM is NIL, the result field will
   be left undefined."
  (if (null elm)
    (make-object-literal :properties
                         (list (cons *done-prop* (make-special-value :symbol :true))))
    (make-object-literal :properties
                         (list
                          (cons *done-prop* (make-special-value :symbol :true))
                          (cons *result-prop* elm)))))

;;;; ======= TRANSFORM methods =====================================================================

(defmethod transform ((xform (eql 'trampoline)) (elm return-statement))
  (with-slots (arg) elm
    (if (or (fn-call-p arg)
            (new-expr-p arg)) ; new expressions are "effective function calls", because the runtime transform will turn them into calls to `$new`.
      (make-return-statement :arg (make-boxed-thunk (make-return-statement :arg (transform 'trampoline arg)
                                                                           :start (source-element-start elm)
                                                                           :end (source-element-end elm))))
      (make-return-statement :arg (make-boxed-result (transform 'trampoline arg))
                             :start (source-element-start elm)
                             :end (source-element-end elm)))))

;;;; ------- handler stack operations --------------------------------------------------------------

(defmethod transform ((xform (eql 'trampoline)) (elm add-handler))
  (make-return-statement
   :arg (make-boxed-thunk (transform 'trampoline (add-handler-thunk-body elm))
                          *add-handler-prop*
                          (add-handler-handler elm))))

(defmethod transform ((xform (eql 'trampoline)) (elm remove-handler))
  (make-return-statement
   :arg (make-boxed-thunk (transform 'trampoline (remove-handler-thunk-body elm))
                          *remove-handler-prop*
                          (remove-handler-handler elm))))

;;;; ------- `suspend` and `resume` transformation -------------------------------------------------

(defmethod transform ((xform (eql 'trampoline)) (elm suspend-statement))
  ;; We don't bother with a replace-handler operation here since we will be exiting
  ;; the $trampoline function right away.
  (make-return-statement :arg (make-boxed-result nil)
                         :start (source-element-start elm)
                         :end (source-element-end elm)))

(defmethod transform ((xform (eql 'trampoline)) (elm resume-statement))
  (with-slots (target arg) elm
    (let ((new-call (make-continuation-call :fn (transform 'trampoline target)
                                            :args (when arg
                                                    (list arg))
                                            :start (source-element-start elm)
                                            :end (source-element-end elm))))
      (make-return-statement
       :arg (make-boxed-thunk
             (make-return-statement :arg new-call
                                    :start (source-element-start elm)
                                    :end (source-element-end elm))
             *replace-handler-stack-prop*
             (make-property-access :target (transform 'trampoline target)
                                   :field *handler-stack-k-prop*))))))

;;;; ------- throwing into a continuation ----------------------------------------------------------

(defmethod transform ((xform (eql 'trampoline)) (elm throw-statement))
  (with-slots (value target) elm
    (cond
      (target
       (make-return-statement
       :arg (make-boxed-thunk
             (make-throw-statement :value (transform 'trampoline value)
                                   :start (source-element-start elm)
                                   :end (source-element-end elm))
             *replace-handler-stack-prop*
             (make-property-access :target (transform 'trampoline target)
                                   :field *handler-stack-k-prop*))))
      (*debug-mode*
       ;; In debug mode, it's a lot less confusing if we generate an otherwise-superfluous
       ;; thunk for each throw statement.
       (make-return-statement
       :arg (make-boxed-thunk
             (make-throw-statement :value (transform 'trampoline value)
                                   :start (source-element-start elm)
                                   :end (source-element-end elm)))))
      (t
       (make-throw-statement :value (transform 'trampoline value)
                             :start (source-element-start elm)
                             :end (source-element-end elm))))))