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
  "identifier for the thunk field of a return object")

(defparameter *done-id* (make-identifier :name "done")
  "identifier for the done field of a return object")

(defparameter *result-id* (make-identifier :name "result")
  "identifier for the result field of a return object")

(defparameter *trampolineResult-id* (make-identifier :name "$trampolineResult")
  "identifier for the runtime function $trampolineResult")

;;; Trampoline results fall into three categories:
;;;
;;; 1. *inlined thunk*
;;;    If the function call is to an identifier representing an in-scope function,
;;;    then it must be thunked.
;;; 2. *inlined result*
;;;    If the return statement's argument is not a function call, then it definitely
;;;    need not be thunked and can be returned in the result field.
;;; 3. *indirected tail-call*
;;;    If the function call is to an idenfitier that is not an in-scope function,
;;;    or to a property or variable, then there is no (current) way to statically
;;;    determine whether it is a call to a trampolined or direct function.  So instead
;;;    we indirect through the runtime function `$trampolineResult`, which checks a
;;;    special runtime property of the function to determine whether this function will
;;;    return a "trampoline-boxed" result or a simple result and wraps the call
;;;    appropriately.
(defun trampoline-result-category (ret-elm)
  "Returns the result category for RET-ELM (which should be a return statement):
   1. :INLINED-THUNK indicates that the function call which is RET-ELM's argument
      should be wrapped in a thunk.
   2. :INLINED-RESULT indicates that RET-ELM's argument (may or may not be a function
      call) should be treated as a final result.
   3. :INDIRECTED-TAIL-CALL indicates that it is not possible to statically determine
      wheter the function call which is RET-ELM's argument is to a trampolined function
      or a direct function."
  (assert (return-statement-p ret-elm))
  (with-slots (arg) ret-elm
    (cond
      ((and (fn-call-p arg)
            (identifier-p (fn-call-fn arg))
            (function-in-scope-p (identifier-name (fn-call-fn arg))))
       :inlined-thunk)
      ((fn-call-p arg)
       :indirected-tail-call)
      (t
       :inlined-result))))

(defun make-thunk (ret-elm)
  "Returns an object literal whose `done` field is `false` and whose
   `thunk` field contains RET-ELM (which should be a return statement)"
  (assert (return-statement-p ret-elm))
  (make-object-literal :properties
                       (list
                        (cons *done-id* (make-special-value :symbol :false))
                        (cons *thunk-id*
                              (make-function-expression :body (list ret-elm))))))

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

(defun make-indirected-call (fn-call-elm)
  "Wraps FN-CALL-ELM (which should be a function call) in a call to the runtime
   function `$trampolineResult`, which will determine at runtime whether to generate
   a thunk that makes the call or to make the call directly and return a result."
  (assert (fn-call-p fn-call-elm))
  (make-fn-call :fn *trampolineResult-id*
                :args (list (fn-call-fn fn-call-elm)
                            (make-special-value :symbol :this)
                            (make-array-literal :elements (fn-call-args fn-call-elm)))))
                
(defmethod transform :around ((xform (eql 'trampoline)) (elm-list list))
  (let ((*function-decls-in-scope* (append (mapcar 'function-decl-name
                                                     (collect-in-scope elm-list 'function-decl))
                                             *function-decls-in-scope*)))
    (call-next-method)))

(defmethod transform ((xform (eql 'trampoline)) (elm return-statement))
  (ecase (trampoline-result-category elm)
    (:inlined-thunk
     (make-return-statement :arg (make-thunk elm)))
    (:inlined-result
     (make-return-statement :arg (make-result (return-statement-arg elm))))
    (:indirected-tail-call
     (make-return-statement :arg (make-indirected-call (return-statement-arg elm))))))
    
;;;; `suspend` and `resume` transformation

(defmethod transform ((xform (eql 'trampoline)) (elm suspend-statement))
  (make-return-statement :arg (make-result nil)))

(defmethod transform ((xform (eql 'trampoline)) (elm resume-statement))
  (with-slots (target arg) elm
    (make-return-statement :arg (make-fn-call :fn (transform 'cps target)
                                              :args (when arg
                                                      (list arg))))))
