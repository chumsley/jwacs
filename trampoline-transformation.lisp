;;;; trampoline-transformation.lisp
;;;
;;; Define the transformation that converts cps-form Javascript source
;;; into trampolined cps-form Javascript.
(in-package :jwacs)

;;;;= Trampoline transformation =
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
;;; is assumed to be in trampoline form.
;;;
;;; It is assumed that all calls to trampoline-style functions are
;;; tail calls; the above two rules are obviously not sufficient when
;;; this condition doesn't hold.

(defparameter *thunk-id* (make-identifier :name "thunk")
  "identifier for the thunk field of a return object")

(defparameter *done-id* (make-identifier :name "done")
  "identifier for the done field of a return object")

(defparameter *result-id* (make-identifier :name "result")
  "identifier for the result field of a return object")

;;TODO First cut, assume all function calls are to trampolined functions
(defun needs-thunk-p (ret-elm)
  "Returns non-NIL for return statements that should be wrapped in a thunk"
  (assert (return-statement-p ret-elm))
  (with-slots (arg) ret-elm
    (fn-call-p arg)))

(defun make-thunk (elm)
  "Returns an object literal whose `done` field is `false` and whose
   `thunk` field contains ELM (which should be a return statement)"
  (assert (return-statement-p elm))
  (make-object-literal :properties
                       (list
                        (cons *done-id* (make-special-value :symbol :false))
                        (cons *thunk-id*
                              (make-function-expression :body (list elm))))))

(defun make-result (elm)
  "Returns an object literal whose `done` field is `true` and whose
   `result` field contains ELM.  If ELM is nil, the result field will
   be left undefined."
  (if (null elm)
    (make-object-literal :properties
                         (list (cons *done-id* (make-special-value :symbol :true))))
    (make-object-literal :properties
                         (list
                          (cons *done-id* (make-special-value :symbol :true))
                          (cons *result-id* elm)))))     
                         
(defmethod transform ((xform (eql 'trampoline)) (elm return-statement))
  (if (needs-thunk-p elm)
    (make-return-statement :arg (make-thunk elm))
    (make-return-statement :arg (make-result (return-statement-arg elm)))))
    
    