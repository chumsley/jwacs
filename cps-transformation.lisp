;;;; cps-transformation.lisp
;;;
;;; Define the cps transformation and supporting functionality.
;;;
;;; Copyright (c) 2005-2006 James Wright and Greg Smolyn
;;; See LICENSE for full licensing details.
;;;
(in-package :jwacs)

;;;; CPS transformation
;;; Initial, naive version does the following:
;;; - All function calls transformed to fn-calls in continuation-passing style
;;; - All new expressions transformed to new expressions that pass a continuation as the first argument
;;; - All assignments to function call or `new` results transformed to new continuations
;;; - All returns transformed to returns of the arg passed to the current continuation
;;; - TODO describe loop conversions
;;; - TODO describe try-catch conversions
;;;
;;;; Preconditions
;;; The CPS transform assumes the following:
;;; 1. Scope analysis transformation has been performed (and therefore all identifiers are unique)
;;; 2. Explicitization transformation has been performed (and therefore all result values have an
;;;    explicit and unique name).
;;; 3. The loop transformation has been performed, so all loops have been transformed to canonical
;;;    while form.

;; These elements should have been removed by LOOP-TO-FUNCTION
(forbid-transformation-elements cps (do-statement for for-in))

(defun assignment-operator-p (elm)
  "Returns non-NIL if ELM represents an operation with an assignment side-effect"
  (and
   (binary-operator-p elm)
   (member (binary-operator-op-symbol elm)
           '(:assign :plus-equals :minus-equals :times-equals :divide-equals
             :lshift-equals :rshift-equals :urshift-equals
             :and-equals :or-equals :xor-equals))))

(defparameter *cont-name* "$k"
  "The name of the standard continuation parameter")

(defparameter *cont-id* (make-identifier :name *cont-name*)
  "An identifier whose name is *cont-name*")

(defparameter *escaping-references* nil
  "List of variable names that are referenced in a statement tail that is going to be
   reordered into a labelled continuation.  We use this to determine when it is safe
   to convert a variable declaration directly into a continuation parameter, vs when
   we need to retain the variable declaration and assign it at the beginning of a
   continuation.")

(defparameter *lexically-active-handlers* nil
  "List of handlers that are active in the current lexical scope.  These are the handlers
   that need to be removed just before a value return, or at the beginning of the continuation
   of a tail call.")

;;;; Statement tails 
;;;
;;; A "statement tail" is a list of the statements that follow the current
;;; statement in the current scope.  Note that this can include statements
;;; from an enclosing scope as well.  For example, in the following code:
;;;
;;;     function foo(opt)
;;;     {
;;;       if(opt)
;;;       {
;;;         foo(x);
;;;         bar(x);
;;;         baz(opt);
;;;       }
;;;       else
;;;       {
;;;         bar(0);
;;;       } 
;;;
;;;       quux();
;;;       quuux();
;;;     }
;;;
;;;     doThis();
;;;     doThat();
;;;
;;; the statement-tail of the `foo(x)` call is (`bar(x)`, `baz(opt)`, `quux()`, `quuux()`).
;;; Note that `quux()` and `quuux()` are in the statement-tail, but `dothis()` and `dothat()`
;;; are not.  The statement-tail will never extend past the end of the innermost enclosing
;;; function declaration.
;;;
;;; Basically, the statement-tail contains all of the statements that should go into a continuation,
;;; should we need to generate a continuation at this point.
;;;

;;;; ======= TRANSFORM method ======================================================================
;;;
;;; As with the explicitization transformation, the cps transformation has a fairly
;;; complicated protocol, so the TRANSFORM method is just a thin wrapper over the
;;; TX-CPS generic function.

(defmethod transform ((xform (eql 'cps)) elm)
  (bind-with-backchannels (new-elm :bare-var-decl-required bare-var-decls)
      (tx-cps elm nil)
    (cond
      ((listp new-elm)
       (append bare-var-decls new-elm))
      (bare-var-decls
       (postpend bare-var-decls new-elm))
      (t
       new-elm))))

(defmethod transform ((xform (eql 'cps)) (elm-list list))
  (bind-with-backchannels (new-list :bare-var-decl-required bare-var-decls)
      (tx-cps elm-list nil)
    (append bare-var-decls new-list)))
    
;;;; ======= TX-CPS ================================================================================
;;;
;;; Previously we bound the *STATEMENT-TAIL* special variable in order to provide
;;; it to "downstream" functions, but now we pass it as a parameter.
;;;
;;; The first return value of the TX-CPS function is the transformed source element.
;;; The second return value indicates whether the statement-tail has been consumed
;;; or not.
;;;
;;; Backchannel return values are sent on channel :BARE-VAR-DECL-REQUIRED by the
;;; VAR-DECL-STATEMENT method.  It sends VAR-DECL-STATEMENTS that need to be placed
;;; at the beginning of the neared enclosing scope.  These backchannel messages are
;;; collected by the FUNCTION-DECL and FUNCTION-EXPRESSION methods, as well as by
;;; the generic TRANSFORM methods.

(defgeneric tx-cps (elm statement-tail)
  (:documentation
   "Converts ELM (which should be explicitized) into CPS form and returns the
    new version as its first return value.  The second return value is T if
    STATEMENT-TAIL was consumed or NIL otherwise."))

(defmethod tx-cps ((elm function-decl) statement-tail)
  (declare (ignore statement-tail))
  ;; The body has an empty return statement appended if not every control path
  ;; has an explicit return.
  (let ((new-parameters (cons *cont-name* (function-decl-parameters elm)))
        (body (if (explicit-return-p (function-decl-body elm))
                (function-decl-body elm)
                (postpend (function-decl-body elm) #s(return-statement)))))
    (bind-with-backchannels (tx-body :bare-var-decl-required bare-var-decls)
        (let ((*lexically-active-handlers* nil))
          (in-local-scope
            (tx-cps body nil))) ; Each function starts with a fresh statement-tail and empty lexical handler environment
      (values
       (make-function-decl :name (function-decl-name elm)
                           :parameters new-parameters
                           :body (append bare-var-decls
                                         tx-body)
                           :start (source-element-start elm)
                           :end (source-element-end elm))
       nil))))

(defmethod tx-cps ((elm function-expression) statement-tail)
  (declare (ignore statement-tail))
  ;; The body has an empty return statement appended if not every control path
  ;; has an explicit return.
  (let ((new-parameters (cons *cont-name* (function-expression-parameters elm)))
        (body (if (explicit-return-p (function-expression-body elm))
                (function-expression-body elm)
                (postpend (function-expression-body elm) #s(return-statement)))))
    (bind-with-backchannels (tx-body :bare-var-decl-required bare-var-decls)
        (let ((*lexically-active-handlers* nil))
          (in-local-scope
            (tx-cps body nil))) ; Each function starts with a fresh statement-tail and empty lexical handler environment
    (values
     (make-function-expression :name (function-expression-name elm)
                                :parameters new-parameters
                                :body (append bare-var-decls
                                              tx-body)
                                :start (source-element-start elm)
                                :end (source-element-end elm))
     nil))))

(defun make-tail-call-continuation (cont-id lexically-active-handlers)
  "If LEXICALLY-ACTIVE-HANDLERS is non-NIL, returns a continuation function that removes all of the active handlers
   and then calls the continuation represented by CONT-ID; otherwise returns CONT-ID."
  (if (null lexically-active-handlers)
    cont-id
    (let* ((param-name (genvar))
           (cont-call (make-return-statement :arg
                                             (make-fn-call :fn cont-id
                                                           :args (list (make-identifier :name param-name)))))
           (remove-handlers (reduce (lambda (outer-handler inner-elm)
                                      (make-remove-handler :handler outer-handler
                                                           :thunk-body (list inner-elm)))
                                    lexically-active-handlers
                                    :initial-value cont-call
                                    :from-end t)))
      (make-continuation-function :parameters (list param-name)
                                  :body (list remove-handlers)))))

(defmethod tx-cps ((elm return-statement) statement-tail)
  (with-slots (arg) elm
    (let ((new-ret
           (cond
             ;; Tail call
             ((fn-call-p arg)
              (make-return-statement
               :arg (make-fn-call :fn (tx-cps (fn-call-fn arg) nil)
                                  :args (cons (make-tail-call-continuation *cont-id*
                                                                           *lexically-active-handlers*)
                                              (mapcar (lambda (item)
                                                        (tx-cps item nil))
                                                      (fn-call-args arg))))
               :start (source-element-start elm)
               :end (source-element-end elm)))

             ;; Tail call to $new0
             ((new-expr-p arg)
              (make-return-statement
               :arg (make-new-expr :constructor (tx-cps (new-expr-constructor arg) nil)
                                   :args (cons (make-tail-call-continuation *cont-id*
                                                                            *lexically-active-handlers*)
                                               (mapcar (lambda (item)
                                                         (tx-cps item nil))
                                                       (new-expr-args arg))))
               :start (source-element-start elm)
               :end (source-element-end elm)))
              
             
             
             ;; Simple return from a try block
             (*lexically-active-handlers*
              (reduce (lambda (outer-handler inner-elm)
                        (make-remove-handler :handler outer-handler
                                             :thunk-body (list inner-elm)))
                      *lexically-active-handlers*
                      :initial-value (make-return-statement
                                      :arg (make-fn-call :fn *cont-id*
                                                         :args (unless (null arg)
                                                                 (list (tx-cps arg nil))))
                                      :start (source-element-start elm)
                                      :end (source-element-end elm))
                      :from-end t))

             ;; Simple return
             (t
              (make-return-statement
               :arg (make-fn-call :fn *cont-id*
                                  :args (unless (null arg)
                                          (list (tx-cps arg nil))))
               :start (source-element-start elm)
               :end (source-element-end elm))))))
      (values new-ret nil))))
      

(defun make-labelled-continuation (name statement-tail)
  "Constructs a continuation from STATEMENT-TAIL and returns a var-decl-statement
   that initializes a variable named NAME to the new continuation.  Note that labelled
   continuations accept no arguments."
  (make-var-init name
                 (make-continuation-function
                  :parameters nil
                  :body (in-local-scope
                          (tx-cps statement-tail nil)))))

(defun make-void-continuation (current-cont)
  "Returns a function expression that calls CURRENT-CONT with no arguments.
   This allows us to preserve the behaviour of functions that return no value."
  (make-continuation-function
   :parameters nil
   :body (list (make-return-statement :arg
                                      (make-fn-call :fn current-cont
                                                    :args nil)))))

;;; This method only handles function calls where the return value is ignored.
;;;
;;; Function calls that are the initializers for variable declarations are handled
;;; in (METHOD TX-CPS (VAR-DECL-STATEMENT T)).  Function calls that are
;;; assigned to existing variables or used as intermediate values in calculations are
;;; transformed to variable-decl initializers in the EXPLICITIZE transformation.
;;; Tail calls (ie, those that are the argument to a return statement) are handled
;;; in (METHOD TX-CPS (RETURN-STATEMENT T)).
(defmethod tx-cps ((elm fn-call) statement-tail)
  (let ((new-fn-call
         (if (null statement-tail)
           ;; Tailless call
           (make-fn-call
            :fn (tx-cps (fn-call-fn elm) nil)
            :args (cons (make-void-continuation *cont-id*)
                        (mapcar (lambda (item)
                                  (tx-cps item nil))
                                (fn-call-args elm)))
            :start (source-element-start elm)
            :end (source-element-end elm))

           ;; Call w/statement-tail
           (make-fn-call
            :fn (tx-cps (fn-call-fn elm) nil)
            :args (cons (make-continuation-function :parameters nil
                                                    :body (in-local-scope
                                                            (tx-cps statement-tail nil)))
                          (mapcar (lambda (item)
                                    (tx-cps item nil))
                                  (fn-call-args elm)))
            :start (source-element-start elm)
            :end (source-element-end elm)))))
    (values (make-return-statement :arg new-fn-call
                                   :start (source-element-start new-fn-call)
                                   :end (source-element-end new-fn-call))
            t)))

(defmethod tx-cps ((elm new-expr) statement-tail)
  (let ((tx-expr
         (if (null statement-tail)
           ;; Tailless construction
           (make-new-expr
            :constructor (tx-cps (new-expr-constructor elm) nil)
            :args (cons (make-void-continuation *cont-id*)
                        (mapcar (lambda (item)
                                  (tx-cps item nil))
                                (new-expr-args elm))))
           ;; Call w/statement-tail
           (make-new-expr
            :constructor (tx-cps (new-expr-constructor elm) nil)
            :args (cons (make-continuation-function :parameters nil
                                                    :body (in-local-scope
                                                            (tx-cps statement-tail nil)))
                        (mapcar (lambda (item)
                                  (tx-cps item nil))
                                (new-expr-args elm)))
            :start (source-element-start elm)
            :end (source-element-end elm)))))
    (values (make-return-statement :arg tx-expr) t)))

;; TODO if we really cared, we could make a predicate to tell us whether every control
;; path _that contains an effective function call_ is terminated.  The current scheme can
;; result in some spurious additional termination (eg if path A contains a function call
;; is terminated but path B contains no function call and is unterminated).
(defun maybe-terminate-toplevel (elm-list)
  "When *IN-LOCAL-SCOPE* is NIL, returns a list of source elements that is guaranteed to be correctly
   terminated for the toplevel.  When *IN-LOCAL-SCOPE* is non-NIL, ELM-LIST is returned unchanged.
 
   'Correctly terminated for the toplevel' means that if there are any effective function calls,
   then all control paths are terminated by either a suspend, resume, or throw statement."
  (let ((termination-needed-p (and (not *in-local-scope*)
                                   (introduces-fn-call-p elm-list)
                                   (not (explicitly-terminated-p elm-list '(:suspend :resume :throw))))))
    (if termination-needed-p
      (postpend elm-list (make-suspend-statement))
      elm-list)))

(defmethod tx-cps ((elm-list list) statement-tail)
  (unless (null elm-list)
    (loop for cell on (maybe-terminate-toplevel elm-list) ; Note: CELL not ELM; iterating maplist-style, not mapcar-style
            for (head consumed) = (multiple-value-list
                                      (tx-cps (car cell)
                                              (append (cdr cell) statement-tail)))
            ;; Collect results
            if (listp head)
            append head into result
            else
            collect head into result

            ;; If the statement tail was consumed, then the incoming STATEMENT-TAIL was consumed,
            ;; so return T as our second return value.  The CDR of CELL (ie, the rest of ELM-LIST)
            ;; was also consued, so don't keep iterating.
            when consumed
            return (values result t)

            ;; If we get to the end of ELM-LIST then we haven't consumed the tail
            finally
            (return (values result nil)))))

(defmethod tx-cps ((elm var-decl-statement) statement-tail)
  ;; Assuming one decl per statment because that is one of the results of explicitization
  (with-slots (var-decls) elm
    (assert (<= (length var-decls) 1))
    (let* ((name (var-decl-name (car var-decls)))
           (initializer (var-decl-initializer (car var-decls)))
           (escaping-reference (find name *escaping-references* :test #'equal))
           (k-param-name (if escaping-reference
                           (genvar)
                           name))
           (assignment-stmt (make-binary-operator :op-symbol :assign
                                                  :left-arg (make-identifier :name name)
                                                  :right-arg (make-identifier :name k-param-name)))
           (augmented-statement-tail (if escaping-reference
                                       (cons assignment-stmt statement-tail)
                                       statement-tail)))

      ;; If this var-decl is for an escaping reference, then we will replace it with a statement
      ;; that _assigns_ the variable rather than _declaring_ it, so signal up the call chain that
      ;; a bare declaration will be required.
      (when escaping-reference
        (backchannel-signal :bare-var-decl-required (make-var-init escaping-reference nil)))
        
      (cond
        ;; eg: var x = fn();
        ((fn-call-p initializer)
         (let ((new-call (make-fn-call :fn (tx-cps (fn-call-fn initializer) nil)
                                       :args (cons
                                              (make-continuation-function :parameters (list k-param-name)
                                                                          :body (in-local-scope
                                                                                  (tx-cps augmented-statement-tail nil)))
                                              (fn-call-args initializer))
                                       :start (source-element-start initializer)
                                       :end (source-element-end initializer))))
           (values (make-return-statement :arg new-call
                                          :start (element-start new-call)
                                          :end (element-end new-call))
                   t)))

        ;; eg: var x = new Foo;
        ((new-expr-p initializer)
         (let ((new-construction (make-new-expr :constructor (tx-cps (new-expr-constructor initializer) nil)
                                                :args (cons
                                                       (make-continuation-function :parameters (list k-param-name)
                                                                                   :body (in-local-scope
                                                                                           (tx-cps augmented-statement-tail nil)))
                                                       (new-expr-args initializer))
                                                :start (source-element-start initializer)
                                                :end (source-element-end initializer))))
           (values (make-return-statement :arg new-construction
                                          :start (element-start new-construction)
                                          :end (element-end new-construction))
                   t)))

        ;; eg: var x = 20;
        (initializer
         (if escaping-reference
           (tx-cps (make-binary-operator :op-symbol :assign
                                         :left-arg (make-identifier :name name)
                                         :right-arg initializer)
                   statement-tail) 
           (multiple-value-bind (new-decl consumed)
               (tx-cps (car var-decls) statement-tail)
             (values (make-var-decl-statement :var-decls (list new-decl)
                                              :start (source-element-start elm)
                                              :end (source-element-end elm))
                     consumed))))

        ;; eg: var x;
        (t
         (if escaping-reference
           (values nil nil)
           (values elm nil)))))))

;;;; ======= function_continuation transformation ==================================================

(defmethod tx-cps ((elm special-value) statement-tail)
  (if (eq :function_continuation
          (special-value-symbol elm))
    (values (make-identifier :name (identifier-name *cont-id*)
                             :start (source-element-start elm)
                             :end (source-element-end elm))
            nil)
    (call-next-method)))

;;;; ======= loop transformation ===================================================================

;; while loops only
;; break needs to call some break continuation (which is essentially the statement-tail)
;; continue needs to call some continue continuation

;; these can be labelled! So an inner loop can call a break or continue continuation created earlier in the chain

;; from example
;; function fun1(x)
;; {
;;   while(f(g(x)))
;;   {
;;     var fooResult = foo();
;;     bar();
;;   }
;;   baz();
;;   return quux(fooResult);
;; }

;; ===> (canonicalization, explicitization, cps)


;; function fun1(x, $k)
;; {
;;   var fooResult;
;;   function continue_k()
;;   {
;;     return g(x, function(JW0) {
;;       return f(JW0, function(JW1) {
;;         if(!JW1)
;;           return break_k();
;;         return foo(function(fooResult$2) {
;;           fooResult = fooResult$2;  // footnote 1
;;           return bar(function() {
;;             return continue_k();
;;           });
;;         });
;;       });
;;     });
;;   }
;;   function break_k()
;;   {
;;     return baz(function() {
;;       return quux(function() {
;;         return $k(fooResult);
;;       });
;;     });
;;   }
;;   return continue_k();
;; }

(defparameter *nearest-continue* nil
  "Name of the continue continuation function for the nearest enclosing loop")
(defparameter *nearest-break* nil
  "Name of the break continuation function for the nearest enclosing loop")

(defmethod tx-cps ((elm while) statement-tail)
  (with-added-environment 
    ;; The break continuation must be transformed outside the scope of the break binding,
    ;; because break statements from within the break continuation should refer to an
    ;; outermore break continuation.  Eg, in:
    ;;
    ;;  var break$n = function()
    ;;  {
    ;;    break; // <----
    ;;  }
    ;;  var break$0 = function()
    ;;  {
    ;;    ...
    ;;  }
    ;;
    ;; The "arrowed" break statement in `break$n` should be transformed to `resume break$0;`,
    ;; NOT to `resume break$n;`; the latter would result in an infinite loop.
    (let* ((break-k (genvar "break"))
           (continue-k (genvar "continue"))
           (break-k-decl (make-labelled-continuation break-k statement-tail))
           (*escaping-references* (union *escaping-references* (find-free-variables statement-tail)))
           (*nearest-break* break-k)
           (*nearest-continue* continue-k))
      (when-let (label (source-element-label elm))
        (add-binding (concatenate 'string label "$break") break-k)
        (add-binding (concatenate 'string label "$continue") continue-k))
      (values
       (list
        ;; On other other hand, the continue continuation should be transformed inside the scope
        ;; of the continue binding, because a `continue` inside of a continue continuation should
        ;; restart the same continuation, not some other continuation.
        break-k-decl
        (make-labelled-continuation continue-k (statement-block-statements (while-body elm)))
        (make-resume-statement :target (make-identifier :name continue-k)))
       t))))

(defmethod tx-cps ((elm break-statement) statement-tail)
  (declare (ignore statement-tail))
  (let ((break-name (aif (break-statement-target-label elm)
                         (find-binding (concatenate 'string it "$break"))
                         *nearest-break*)))
    (values
     (make-resume-statement :target (make-identifier :name break-name)
                            :start (source-element-start elm)
                            :end (source-element-end elm))
     nil)))

(defmethod tx-cps ((elm continue-statement) statement-tail)
  (declare (ignore statement-tail))
  (let ((continue-name (aif (continue-statement-target-label elm)
                            (find-binding (concatenate 'string it "$continue"))
                            *nearest-continue*)))
    (values
     (make-resume-statement :target (make-identifier :name continue-name)
                            :start (source-element-start elm)
                            :end (source-element-end elm))
     nil)))

;;;; ======= Branch statements =====================================================================

;; Because if statements are so common, we want to be sure that their output is
;; pretty close to optimal.  In particular, we don't want to output a non-null
;; statement-tail more than once, but we also don't want to generate a named
;; continuation unless absolutely necessary.
(defmethod tx-cps ((elm if-statement) statement-tail)

  (if (null statement-tail)
    ;; No special handling is required when there's no statement tail
    (call-next-method)
    (with-slots (condition then-statement else-statement) elm
      (let ((then-terminated (explicitly-terminated-p then-statement
                                                      '(:return :throw :break :continue :resume :suspend)))
            (else-terminated (explicitly-terminated-p else-statement
                                                      '(:return :throw :break :continue :resume :suspend))))
        (cond
          ;; If the then-statement is terminated and there is no else clause, then there
          ;; is no statement-tail handling required except for denying the then branch the
          ;; statement tail
          ((and then-terminated (null else-statement))
           (values
              (make-if-statement :condition (tx-cps condition nil)
                                 :then-statement (single-statement (tx-cps then-statement nil))
                                 :start (source-element-start elm)
                                 :end (source-element-end elm))
              nil))

          ;; In the case where one branch is terminated and the other isn't, the unterminated
          ;; branch gets the statement-tail and the terminated branch doesn't (because it doesn't
          ;; need it)
          ((and then-terminated (not else-terminated))
           (multiple-value-bind (tx-else else-consumed)
               (tx-cps else-statement statement-tail)
             (values
              (make-if-statement :condition (tx-cps condition nil)
                                 :then-statement (single-statement (tx-cps then-statement nil))
                                 :else-statement (single-statement tx-else)
                                 :start (source-element-start elm)
                                 :end (source-element-end elm))
              else-consumed)))
          
          ((and (not then-terminated) else-terminated)
           (multiple-value-bind (tx-then then-consumed)
               (tx-cps then-statement statement-tail)
             (values
              (make-if-statement :condition (tx-cps condition nil)
                                 :then-statement (single-statement tx-then)
                                 :else-statement (single-statement (tx-cps else-statement nil))
                                 :start (source-element-start elm)
                                 :end (source-element-end elm))
              then-consumed)))
          
          ;; In the case where both branches are terminated, we don't consume the statement-tail
          ;; at all (because neither branch needs it)
          ((and then-terminated else-terminated)
           (multiple-value-bind (tx-then then-consumed)
               (tx-cps then-statement statement-tail)
             (multiple-value-bind (tx-else else-consumed)
                 (tx-cps else-statement statement-tail)
               (assert (not then-consumed))
               (assert (not else-consumed))
               (values
                (make-if-statement :condition (tx-cps condition nil)
                                   :then-statement (single-statement tx-then)
                                   :else-statement (single-statement tx-else)
                                   :start (source-element-start elm)
                                   :end (source-element-end elm))
                (or then-consumed else-consumed)))))

          ;; When neither branch is terminated we need to generate a labelled continuation.
          ;; TODO We could further optimize this to only use a labelled continuation if
          ;; both statements actually /consume/ the tail.
          ((and (not then-terminated) (not else-terminated))
           (let* ((if-k-name (genvar "ifK"))
                  (if-k-decl (make-labelled-continuation if-k-name statement-tail))
                  (*escaping-references* (union *escaping-references* (find-free-variables statement-tail)))
                  (if-k-resume (make-resume-statement :target (make-identifier :name if-k-name))))
             (values
              (list
               if-k-decl
               (make-if-statement :condition (tx-cps condition nil)
                                  :then-statement (single-statement
                                                   (tx-cps (combine-statements then-statement if-k-resume)
                                                           nil))
                                  :else-statement (single-statement
                                                   (tx-cps (combine-statements else-statement if-k-resume)
                                                           nil))
                                  :start (source-element-start elm)
                                  :end (source-element-end elm)))
              t))))))))

(defmethod tx-cps ((elm switch) statement-tail)
  (with-added-environment
    (let* ((switch-k-name (genvar "switchK"))
           (switch-k-decl (make-labelled-continuation switch-k-name statement-tail))
           (*escaping-references* (union *escaping-references* (find-free-variables statement-tail)))
           (*nearest-break* switch-k-name)
           (terminated-clauses (compute-terminated-clauses (switch-clauses elm))))
      (when-let (label (switch-label elm))
        (add-binding (concatenate 'string label "$break") switch-k-name))
      (values
       (list
        switch-k-decl
        (make-switch :value (tx-cps (switch-value elm) nil)
                     :clauses (mapcar (lambda (elm)
                                        (tx-cps elm nil))
                                      terminated-clauses)
                     :start (source-element-start elm)
                     :end (source-element-end elm)))
       t))))
                                                                  
(defun compute-terminated-clauses (clause-list)
  "Takes a list of switch clauses that may or may not be terminated (eg, by a break statement),
   and returns a list of clauses with equivalent effects that are more suitable for cps translation.
   Specifically:
     1) Terminated clauses are returned unchanged.  A terminated clause is one which is terminated
        in all its control paths.
     2) 'Null clauses' (ie, clauses with no body) are also returned unchanged so long as they aren't
        the final clause.  If they are the final clause, their body is set to a single break statement.
     3) Unterminated clauses have the body of each following clause appended to them (up to and including
        the first terminated clause).
     4) If the final clause is unterminated, it will have a break statement appended to it
     5) If there is no default clause, a default clause will be added to the end of the list with a body
        containing a single break statement.

   eg:

      case 10:
        doSomething();
      case 20:
      case 30:
        doSomethingElse();
        break;
      default:

   ===>

      case 10:
        doSomething();
        doSomethingElse();
        break;
      case 20:
      case 30:
        doSomethingElse();
        break;
      default:
        break;"
  (labels ((terminated-p (elm)
             (explicitly-terminated-p elm '(:return :throw :break :continue :resume :suspend)))
           (extend-clause-body (clause-list)
             "Takes a list of switch-clauses whose head is unterminated and returns a body
              that extends until the first terminated clause.  A break will be added to the
              end of the body if every clause in CLAUSE-LIST is unterminated."
             ;; First check for a non-final null clause, since non-final null clauses are not extended
             (if (and (null (slot-value (car clause-list) 'body)) 
                      (consp (cdr clause-list)))
               nil
               (let ((extended-body (loop for clause in clause-list
                                          append (slot-value clause 'body)
                                          until (terminated-p clause))))
                 (if (terminated-p extended-body)
                   extended-body
                   (postpend extended-body (make-break-statement :target-label nil)))))))
    (loop for clause-tail on clause-list
          for clause = (car clause-tail)
          for extended-body = (extend-clause-body clause-tail)
          count (default-clause-p clause) into defaults-encountered
          collect (if (case-clause-p clause)
                    (make-case-clause :value (case-clause-value clause)
                                      :body extended-body
                                      :start (source-element-start clause)
                                      :end (source-element-end clause))
                    (make-default-clause :body extended-body
                                         :start (source-element-start clause)
                                         :end (source-element-end clause)))
          when (and (null (cdr clause-tail))
                    (zerop defaults-encountered))
          collect (make-default-clause :body (list (make-break-statement :target-label nil))))))

(defun make-labelled-catch-continuation (name catch-clause &optional tail-resume-elm)
  "Returns a labelled continuation that can be used as a catch handler based on CATCH-CLAUSE.
   The continuation will accept an argument that represents the binding in the clause.
   Returns a var-init statement that assigns the catch continuation to a var named NAME.
   If TAIL-RESUME-ELM is non-NIL, it will be added as the last statement of the handler function.
   It should be a resume statement that resumes the tail continuation for unterminated catch
   clauses."
  (with-slots (body binding) catch-clause
    (make-var-init name
                   (if (null tail-resume-elm)
                     (make-continuation-function :name nil
                                                 :parameters (list binding)
                                                 :body (transform 'cps
                                                                  body))
                     (make-continuation-function :name nil
                                                 :parameters (list binding)
                                                 :body (transform 'cps
                                                                  (postpend body tail-resume-elm)))))))
        
;; TODO
;; There are some optimizations/improvements possible here:
;; 1-3) done
;; 4) We add $removeHandler at the end of a try block even if all its control paths are explicitly
;;    terminated; we shouldn't.
;; 5) The same single-simple-statement labelled-continuation removal optimization that we're pondering for
;;    if-statements might apply here as well.

(defmethod tx-cps ((elm try) statement-tail)
  (let* ((catch-k-name (genvar "catchK"))
         (catch-k-id (make-identifier :name catch-k-name)))
    
    (unless (null (try-finally-clause elm))
      (error "finally clauses are not yet supported")) ;TODO support finally clauses

    ;; TODO we're explicitly handling neither the case where both catch clause and body are terminated (in
    ;; which case the statement-tail can just be discarded) nor the case where the try body is terminated
    ;; but not the catch clause (in which case the try body needs no final-remove and the catch clause
    ;; can consume the statement tail directly).
    (if (explicit-return-p (try-catch-clause elm))

      ;; no labelled continuation required for statement-tail because catch clause is explicitly
      ;; terminated, so the try body can consume the tail directly.
      (let ((final-remove (make-remove-handler :handler catch-k-id
                                               :thunk-body (tx-cps statement-tail nil)))
            (catch-k-decl (make-labelled-catch-continuation catch-k-name (try-catch-clause elm)))
            (*lexically-active-handlers* (cons catch-k-id *lexically-active-handlers*)))
        (if (explicit-return-p (try-body elm))
          (values (combine-statements
                   catch-k-decl
                   (make-add-handler :handler catch-k-id
                                     :thunk-body (tx-cps (try-body elm) nil)))
                  t)
          (values (combine-statements
                   catch-k-decl
                   (make-add-handler :handler catch-k-id
                                     :thunk-body (tx-cps (postpend (try-body elm) final-remove) nil))) ; We're postpending an already-transformed element to the untransformed body; see the REMOVE-HANDLER method for why this works
                  t)))

      ;; the catch clause is unterminated, so both catch and try are sharing the statement-tail,
      ;; so it needs a labelled continuation.
      (let* ((tail-k-name (genvar "tryK"))
             (tail-k-id (make-identifier :name tail-k-name))
             (tail-resume-elm (make-resume-statement :target tail-k-id))
             (tail-k-decl (make-labelled-continuation tail-k-name statement-tail))
             (catch-k-decl (make-labelled-catch-continuation catch-k-name (try-catch-clause elm) tail-resume-elm))
             (final-remove (make-remove-handler :handler catch-k-id
                                                :thunk-body (list tail-resume-elm)))
             (*escaping-references* (union (find-free-variables statement-tail) *escaping-references*))
             (*lexically-active-handlers* (cons catch-k-id *lexically-active-handlers*)))
        (if (explicit-return-p (try-body elm))
          (values (combine-statements
                   tail-k-decl
                   catch-k-decl
                   (make-add-handler :handler catch-k-id
                                     :thunk-body (tx-cps (try-body elm) nil)))
                  t)
          (values (combine-statements
                   tail-k-decl
                   catch-k-decl
                   (make-add-handler :handler catch-k-id
                                     :thunk-body (tx-cps (postpend (try-body elm) final-remove) nil)))
                  t))))))

(defmethod tx-cps ((elm remove-handler) statement-tail)
  ;; The only REMOVE-HANDLER elements that we encounter should be those that we've added during
  ;; the cps transformation, which means that their slots will already have been processed, so
  ;; we can just return the element unchanged.
  (values elm nil))

;;;; ======= default behaviour =====================================================================
;;;
;;; We have to re-implement the default behaviour that is normally provided by the
;;; TRANSFORM generic function, since we are using a special generic function of
;;; our own.

;; The default behaviour for any element is to do nothing
(defmethod tx-cps (elm statement-tail)
  (declare (ignore statement-tail))
  (values elm nil))

(defmethod tx-cps ((elm source-element) statement-tail)
  (let ((consumed nil))
    (flet ((slot-tx (slot-elm)
             (if consumed
               (tx-cps slot-elm nil)
               (multiple-value-bind (new-arg slot-consumed)
                   (tx-cps slot-elm statement-tail)
                 (setf consumed slot-consumed)
                 new-arg))))
      (values
       (apply
        (get-constructor elm)
        (loop for slot in (structure-slots elm)
              collect (make-keyword slot)
              collect (slot-tx (slot-value elm slot))))
       consumed))))

;; Special case for object-literals to account for the fact that object-literal-properties
;; is an alist rather than a list of structures.
(defmethod tx-cps ((elm object-literal) statement-tail)
  (let ((consumed nil))
    (flet ((slot-tx (slot-elm)
             (if consumed
               (tx-cps slot-elm nil)
               (multiple-value-bind (new-arg slot-consumed)
                   (tx-cps slot-elm statement-tail)
                 (setf consumed slot-consumed)
                 new-arg))))
      (values
       (make-object-literal :properties
                            (loop for (prop-name . prop-value) in (object-literal-properties elm)
                                  collect (cons (slot-tx prop-name)
                                                (slot-tx prop-value)))
                            :start (source-element-start elm)
                            :end (source-element-end elm))
       consumed))))
