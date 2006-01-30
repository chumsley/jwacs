;;;; runtime-transformation.lisp
;;;
;;; Define the transformation that adds in support for and calls into
;;; the dynamic runtime.
(in-package :jwacs)

;;; We're going to "bubble up" some source elements as part of the RUNTIME
;;; transformation, so we need to provide a method for the list type.
(defmethod transform ((xform (eql 'runtime)) (elm-list list))
  (unless (null elm-list)
    (let ((head (transform xform (car elm-list))))
      (if (listp head)
        (append head (transform xform (cdr elm-list)))
        (cons head (transform xform (cdr elm-list)))))))

;;;; Runtime flags
;;;
;;; We add runtime flags to each function indicating its type (continuation,
;;; transformed jwacs function, etc.)

(defparameter *transformed-property* (make-string-literal :value "$jw")
  "The name of the property that we set on each transformed function")

(defparameter *continuation-property* (make-string-literal :value "$isK")
  "The name of the property that we set on each continuation function")

(defparameter *makeK-fn* (make-identifier :name "$makeK")
  "Runtime function called to flag a function expression as a continuation")

(defparameter *makeLambda-fn* (make-identifier :name "$lambda")
  "Runtime function called to flag a function expression as a transformed jwacs function expression")

(defparameter *callFromDirect-fn* (make-identifier :name "$callFromDirect")
  "Runtime function which will call a transformed function with a trampoline loop and identity continuation")

(defparameter *call-fn* (make-identifier :name "$call")
  "Runtime function that we use to make indirect calls to targets that may not be transformed")

;;;; Call-style guards
;;;
;;; We are only transforming our own code; we're not transforming anyone else's.
;;; So there will be lots of cases where untransformed code attempts to call a
;;; transformed function in direct style.  We deal with these cases by checking
;;; that the first argument is a continuation; if it isn't, we'll start a trampoline
;;; with a thunk that calls the current function with an identity continuation.

(defun make-call-style-guard (fn-name)
  "Builds a if statement that checks whether the incoming continuation argument
   is actually a continuation.  If it isn't, then (on the assumption that this
   is an incoming direct-style call), re-calls the current function (whose name
   is FN-NAME) with a default continuation parameter (`$id`) followed by the
   original incoming parameters"
  (assert (not (null fn-name)))
  (make-if-statement
   :condition (make-unary-operator :op-symbol :logical-not
                                   :arg (make-property-access :target *cont-id*
                                                              :field *continuation-property*))
   :then-statement (make-return-statement
                    :arg
                    (make-fn-call :fn *callFromDirect-fn*
                                  :args (list (make-identifier :name fn-name)
                                              (make-special-value :symbol :this)
                                              (make-identifier :name "arguments"))))))

;;;; Scope tracking
;;;
;;; We track which function-decls are in scope, on the assumption that anything
;;; in scope has been transformed.
;;; TODO This will have to get quite a bit more sophisticated once we add
;;; "context optimization" (where we only transform functions that are in a call
;;; path down to a function_continuation capture).

(defparameter *function-decls-in-scope* nil
  "A list of names of currently-visible function-decls.  We use this to
   determine which calls need to be indirected through $call, and which can
   be 'inlined' as unchecked CPS calls.  We don't do any handling for
   duplicate identifiers, since we currently assume that variable names will
   all have been uglified by this point.")

(defun function-in-scope-p (target-name)
  "Return non-NIL if a function-decl with name NAME is currently visible in scope"
  (member target-name *function-decls-in-scope* :test 'equal))

(defun inlineable-call-target (fn-elm)
  "Return non-NIL if it is definitely safe to use FN-ELM as the target of an
   inlined call (rather than one that has been indirected through $call)"
  (and (identifier-p fn-elm)
       (or (function-in-scope-p (identifier-name fn-elm)) ; Calling a transformed function
           (equalp *cont-id* fn-elm)))) ; Calling the function's continuation

(defun make-indirected-call (fn-call-elm)
  "Wraps FN-CALL-ELM (which should be a function call) in a call to the runtime
   function `$trampolineResult`, which will determine at runtime whether to generate
   a thunk that makes the call or to make the call directly and return a result."
  (assert (fn-call-p fn-call-elm))
  (make-fn-call :fn *callFromDirect-fn*
                :args (list (fn-call-fn fn-call-elm)
                            (make-special-value :symbol :this)
                            (make-array-literal :elements (fn-call-args fn-call-elm)))))

(defmethod transform :around ((xform (eql 'runtime)) (elm-list list))
  (let ((*function-decls-in-scope* (append (mapcar 'function-decl-name
                                                   (collect-in-scope elm-list 'function-decl))
                                           *function-decls-in-scope*)))
    (call-next-method)))

;;;; The runtime transformation
;;;
;;; 1. Add indirection for calls to functions that we can't be sure are transformed
;;; 2. Add call-style guards to allow functions to be called in direct style without
;;;    causing confusing errors.
;;; 3. Add code to flag each function decl and expression with its type

(defmethod transform ((xform (eql 'runtime)) (elm function-decl))
  (list
   (make-function-decl :name (function-decl-name elm)
                       :parameters (function-decl-parameters elm)
                       :body (cons (make-call-style-guard (function-decl-name elm))
                                   (transform 'runtime (function-decl-body elm))))
   (make-binary-operator :op-symbol :assign
                         :left-arg
                         (make-property-access :target (make-identifier :name (function-decl-name elm))
                                               :field *transformed-property*)
                                               
                         :right-arg (make-special-value :symbol :true))))

(defmethod transform ((xform (eql 'runtime)) (elm function-expression))
  (let ((fn-name (if (null (function-expression-name elm))
                    (genvar "lambda")
                    (function-expression-name elm))))
    (make-fn-call :fn *makeLambda-fn*
                  :args (list
                         (make-function-expression
                          :name fn-name
                          :parameters (function-expression-parameters elm)
                          :body (cons (make-call-style-guard fn-name)
                                      (transform 'runtime
                                                 (function-expression-body elm))))))))

(defmethod transform ((xform (eql 'runtime)) (elm continuation-function))
  (make-fn-call :fn *makeK-fn*
                :args (list
                       (make-function-expression :name (function-expression-name elm)
                                                 :parameters (function-expression-parameters elm)
                                                 :body (transform 'runtime (function-expression-body elm))))))

(defmethod transform ((xform (eql 'runtime)) (elm thunk-function))
  (make-function-expression :name (function-expression-name elm)
                            :parameters (function-expression-parameters elm)
                            :body (transform 'runtime (function-expression-body elm))))

(defmethod transform ((xform (eql 'runtime)) (elm fn-call))
  (flet ((runtime-transform (elm)
           (transform 'runtime elm)))
    (assert (or (continuation-function-p (first (fn-call-args elm))) ; First argument is a new continuation
                (equalp *cont-id* (first (fn-call-args elm))) ; First argument is the function's continuation
                (equalp *cont-id* (fn-call-fn elm)))) ; Call's target is the function's continuation
    
    (if (inlineable-call-target (fn-call-fn elm))
      (make-fn-call :fn (runtime-transform (fn-call-fn elm))
                    :args (mapcar #'runtime-transform (fn-call-args elm)))
      (make-fn-call :fn *call-fn*
                    :args (list
                           (runtime-transform (fn-call-fn elm))
                           (runtime-transform (first (fn-call-args elm)))
                           (make-special-value :symbol :this)
                           (make-array-literal :elements
                                               (mapcar #'runtime-transform
                                                       (cdr (fn-call-args elm)))))))))

;; We can be sure that we don't need to indirect through $call for a continuation-call, because those
;; are only produced by resume statements.
(defmethod transform ((xform (eql 'runtime)) (elm continuation-call))
  (make-fn-call :fn (transform 'runtime (fn-call-fn elm))
                :args (mapcar (lambda (arg) (transform 'runtime arg)) (fn-call-args elm))))