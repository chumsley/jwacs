;;;; runtime-transformation.lisp
;;;
;;; Define the transformation that adds in support for and calls into
;;; the dynamic runtime.
;;;
;;; Copyright (c) 2006 James Wright
;;; See LICENSE for full licensing details.
;;;
(in-package :jwacs)

;;; XXX Note that the runtime transformation does not preserve source-position
;;; information.  The only reason for that is that the trampoline transformation is where
;;; we grab that data and make it available for debuggers, which happens before the
;;; runtime transformation is applied.

;;;; ======= Constants =============================================================================

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

(defparameter *call0-fn* (make-identifier :name "$call0")
  "Runtime function that we use to make indirect calls with a small number of arguments
   to targets that may not be transformed")

(defparameter *max-call0-args* 8
  "Maximum number of arguments that can be passed to a function using $call0")
  
(defparameter *call-fn* (make-identifier :name "$call")
  "Runtime function that we use to make indirect calls with large numbers of arguments
   to targets that may not be transformed")

(defparameter *new0-fn* (make-identifier :name "$new0")
  "Runtime function that we use to make indirect constructions with a small number of arguments
   to constructors that may have been transformed")

(defparameter *max-new0-args* 8
  "Maximum number of arguments that can be passed to a constructor using $new0")

(defparameter *new-fn* (make-identifier :name "$new")
  "Runtime function that we use to make indirect constructions with a large number of arguments
   to constructors that may have been transformed")

(defparameter *makeArguments-fn* (make-identifier :name "$makeArguments")
  "Runtime function that constructs a shadowing `arguments` object that omits the continuation
   argument from the numbered arguments (but which includes it as the 'continuation' field)")
  
(defparameter *pogo-function* (make-identifier :name "$trampoline")
  "Runtime function that drives trampoline-style programs; We have to add explicit calls to this
   function for toplevel calls to trampolined functions.")

(defparameter *addHandler-fn* (make-identifier :name "$addHandler")
  "Runtime function that adds a new exception handler to the global handler stack")

(defparameter *removeHandler-fn* (make-identifier :name "$removeHandler")
  "Runtime function that removes the top exception handler from the global handler stack")

;;;; ======= Environment tracking ==================================================================

(defparameter *toplevel-handler-stack-reference* (make-special-value :symbol :null)
  "At the toplevel and outside of a thunk, there is no handler stack yet.")

(defparameter *in-function-handler-stack-reference* (make-property-access :target *cont-id*
                                                                          :field *handler-stack-k-prop*)
  "Inside a function but outside of a thunk, the current handler stack is the handler stack property
   of the function's continuation.")

(defparameter *in-thunk-handler-stack-reference* (make-identifier :name *handler-stack-var-name*)
  "Inside a thunk, the current handler stack is passed in as a parameter named `$e`.")

(defparameter *current-handler-stack-reference* *toplevel-handler-stack-reference*
  "Element that refers to the current handler stack.  This should be one of
   *TOPLEVEL-HANDLER-STACK-REFERENCE*, *IN-FUNCTION-HANDLER-STACK-REFERENCE*, or
   *IN-THUNK-HANDLER-STACK-REFERENCE*.")

;;;; ======= Call-style guards =====================================================================
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
   :condition (make-binary-operator :op-symbol :logical-or
                                    :left-arg (make-unary-operator :op-symbol :logical-not
                                                                   :arg *cont-id*)
                                    :right-arg (make-unary-operator :op-symbol :logical-not
                                                                   :arg (make-property-access :target *cont-id*
                                                                                              :field *continuation-property*)))
   :then-statement (make-return-statement
                    :arg
                    (make-fn-call :fn *callFromDirect-fn*
                                  :args (list (make-identifier :name fn-name)
                                              (make-special-value :symbol :this)
                                              (make-identifier :name "arguments"))))))

;;;; ======= Scope tracking ========================================================================
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

(defmethod transform :around ((xform (eql 'runtime)) (elm-list list))
  (let ((new-decls (mapcar 'function-decl-name
                           (collect-in-scope elm-list 'function-decl))))
    (if (subsetp new-decls *function-decls-in-scope*)
      (call-next-method)
      (let ((*function-decls-in-scope* (union new-decls *function-decls-in-scope*)))
        (call-next-method)))))

;;;; ======= The runtime transformation ============================================================
;;;
;;; 1. Add indirection for calls to functions that we can't be sure are transformed
;;; 2. Add call-style guards to allow functions to be called in direct style without
;;;    causing confusing errors.
;;; 3. Add code to flag each function decl and expression with its type
;;; 4. Replace references to `arguments` with a call to `$makeArguments` applied to `arguments`.
;;; TODO Make this description more complete

;; These elements should have been removed by TRAMPOLINE
(forbid-transformation-elements runtime (resume-statement suspend-statement add-handler remove-handler))

(defmethod transform ((xform (eql 'runtime)) (elm function-decl))
  (list
   (make-function-decl :name (function-decl-name elm)
                       :parameters (function-decl-parameters elm)
                       :body (cons (make-call-style-guard (function-decl-name elm))
                                   (let ((*current-handler-stack-reference* *in-function-handler-stack-reference*))
                                     (in-local-scope
                                       (transform 'runtime (function-decl-body elm))))))
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
                                      (let ((*current-handler-stack-reference* *in-function-handler-stack-reference*))
                                        (in-local-scope
                                          (transform 'runtime
                                                     (function-expression-body elm))))))))))

(defmethod transform ((xform (eql 'runtime)) (elm continuation-function))
  (make-fn-call :fn *makeK-fn*
                :args (list
                       (make-function-expression :name (function-expression-name elm)
                                                 :parameters (function-expression-parameters elm)
                                                 :body (in-local-scope
                                                         (transform 'runtime (function-expression-body elm))))
                       *current-handler-stack-reference*)))

(defmethod transform ((xform (eql 'runtime)) (elm thunk-function))
  (make-function-expression :name (function-expression-name elm)
                            :parameters (function-expression-parameters elm)
                            :body (let ((*current-handler-stack-reference* *in-thunk-handler-stack-reference*))
                                    (in-local-scope
                                      (transform 'runtime (function-expression-body elm))))))

(defmethod transform ((xform (eql 'runtime)) (elm fn-call))
  (let ((new-call
         (flet ((runtime-transform (elm)
                  (transform 'runtime elm)))
           (let ((this-obj (if (property-access-p (fn-call-fn elm))
                             (runtime-transform (property-access-target (fn-call-fn elm)))
                             (make-special-value :symbol :null)))
                 (method-name (when (property-access-p (fn-call-fn elm))
                                (property-access-field (fn-call-fn elm)))))
             (assert (or (continuation-function-p (first (fn-call-args elm))) ; First argument is a new continuation
                         (equalp *cont-id* (first (fn-call-args elm))) ; First argument is the function's continuation
                         (equalp *cont-id* (fn-call-fn elm)))) ; Call's target is the function's continuation
    
             (cond
               ((inlineable-call-target (fn-call-fn elm))
                (make-fn-call :fn (runtime-transform (fn-call-fn elm))
                              :args (mapcar #'runtime-transform (fn-call-args elm))))
               ((<= (length (fn-call-args elm)) *max-call0-args*)
                (make-fn-call :fn *call0-fn*
                              :args (append
                                     (list
                                      (if method-name
                                        (runtime-transform method-name)
                                        (runtime-transform (fn-call-fn elm)))
                                      (runtime-transform (first (fn-call-args elm)))
                                      this-obj)
                                     (mapcar #'runtime-transform
                                             (cdr (fn-call-args elm))))))
               (t
                (make-fn-call :fn *call-fn*
                              :args (list
                                     (runtime-transform (fn-call-fn elm))
                                     (runtime-transform (first (fn-call-args elm)))
                                     this-obj
                                     (make-array-literal :elements
                                                         (mapcar #'runtime-transform
                                                                 (cdr (fn-call-args elm))))))))))))
    new-call))

(defmethod transform ((xform (eql 'runtime)) (elm new-expr))
  (let ((new-call
         (flet ((runtime-transform (elm)
                  (transform 'runtime elm)))
           (with-slots (constructor args) elm
             (if (<= (length args) *max-new0-args*)
               (make-fn-call :fn *new0-fn*
                             :args (cons (runtime-transform constructor)
                                         (runtime-transform args))) ; We assume that the continuation is already the first arg
               (make-fn-call :fn *new-fn*
                             :args (list (runtime-transform constructor)
                                         (runtime-transform (car args))
                                         (make-array-literal :elements (mapcar #'runtime-transform
                                                                               (cdr args))))))))))
    new-call))
  
(defmethod transform ((xform (eql 'runtime)) (elm special-value))
  (if (eq :arguments (special-value-symbol elm))
    (make-fn-call :fn *makeArguments-fn*
                  :args (list (make-identifier :name *arguments-name*)))
    (call-next-method)))

;; We can be sure that we don't need to indirect through $call for a continuation-call, because those
;; are only produced by resume statements.
(defmethod transform ((xform (eql 'runtime)) (elm continuation-call))
  (let ((new-call (make-fn-call :fn (transform 'runtime (fn-call-fn elm))
                  :args (mapcar (lambda (arg)
                                  (transform 'runtime arg))
                                (fn-call-args elm)))))
    new-call))

;; If we encounter a return statement at the toplevel, then wrap it in a thunk and pass it into
;; `$trampoline`.  (Due to the nature of the cps transformation, each toplevel control path will
;; have at most one of these)
(defmethod transform ((xform (eql 'runtime)) (elm return-statement))
  (with-slots (arg) elm
    (if *in-local-scope*
      (make-return-statement :arg (transform 'runtime arg))
      (make-fn-call :fn *pogo-function*
                    :args (list (make-thunk-function
                                 :parameters (list *handler-stack-var-name*)
                                 :body (list (make-return-statement
                                              :arg (in-local-scope
                                                     (transform 'runtime arg))))))))))
