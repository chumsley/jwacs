;;;; trampoline-transformation.lisp
;;;
;;; Define the transformation that converts cps-form Javascript source
;;; into trampolined cps-form Javascript.
(in-package :jwacs)

;;;; Trampoline transformation 
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
  "identifier for the thunk field of a boxed result object")

(defparameter *done-id* (make-identifier :name "done")
  "identifier for the done field of a boxed result object")

(defparameter *result-id* (make-identifier :name "result")
  "identifier for the result field of a boxed result object")

(defun make-thunk (ret-elm)
  "Returns an object literal whose `done` field is `false` and whose
   `thunk` field contains a thunk whose only line is RET-ELM (which
   should be a return statement)"
  (assert (return-statement-p ret-elm))
  (make-object-literal :properties
                       (list
                        (cons *done-id* (make-special-value :symbol :false))
                        (cons *thunk-id*
                              (make-thunk-function :body (list ret-elm))))))

(defun make-result (elm)
  "Returns an object literal whose `done` field is `true` and whose
   `result` field contains ELM.  If ELM is NIL, the result field will
   be left undefined."
  (if (null elm)
    (make-object-literal :properties
                         (list (cons *done-id* (make-special-value :symbol :true))))
    (make-object-literal :properties
                         (list
                          (cons *done-id* (make-special-value :symbol :true))
                          (cons *result-id* elm)))))     

(defmethod transform ((xform (eql 'trampoline)) (elm return-statement))
  (with-slots (arg) elm
    (if (fn-call-p (return-statement-arg elm))
      (make-return-statement :arg (make-thunk (make-return-statement :arg (transform 'trampoline arg))))
      (make-return-statement :arg (make-result (transform 'trampoline arg))))))

;;;; `suspend` and `resume` transformation

(defmethod transform ((xform (eql 'trampoline)) (elm suspend-statement))
  (make-return-statement :arg (make-result nil)))

(defmethod transform ((xform (eql 'trampoline)) (elm resume-statement))
  (with-slots (target arg) elm
    (make-return-statement
     :arg
     (make-thunk (make-return-statement
                  :arg
                  (make-continuation-call :fn (transform 'trampoline target)
                                          :args (when arg
                                                  (list arg))))))))
