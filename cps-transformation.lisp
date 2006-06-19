;;;; cps-transformation.lisp
;;;
;;; Define the cps transformation and supporting functionality.
(in-package :jwacs)

;;;; CPS transformation
;;; Initial, naive version does the following:
;;; - All function calls transformed to fn-calls in continuation-passing style
;;; - All new expressions transformed to new expressions that pass a continuation as the first argument
;;; - All assignments to function call or `new` results transformed to new continuations
;;; - All returns transformed to returns of the arg passed to the current continuation
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

;;;================================================================================
;;;; TRANSFORM method
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
    
;;;================================================================================
;;;; TX-CPS
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
        (in-local-scope
          (tx-cps body nil)) ; Each function starts with a fresh statement-tail
      (values
       (make-function-decl :name (function-decl-name elm)
                           :parameters new-parameters
                           :body (append bare-var-decls
                                         tx-body))
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
        (in-local-scope
          (tx-cps body nil)) ; Each function starts with a fresh statement-tail
    (values
     (make-function-expression :name (function-expression-name elm)
                                :parameters new-parameters
                                :body (append bare-var-decls
                                              tx-body))
     nil))))

(defmethod tx-cps ((elm return-statement) statement-tail)
  (with-slots (arg) elm
    (let ((new-fn-call
           (cond
             ;; Tail call
             ((fn-call-p arg)
              (make-fn-call :fn (tx-cps (fn-call-fn arg) nil)
                            :args (cons *cont-id*
                                        (mapcar (lambda (item)
                                                  (tx-cps item nil))
                                                (fn-call-args arg)))))

             ;; Tail call to $new0
             ((new-expr-p arg)
              (make-new-expr :constructor (tx-cps (new-expr-constructor arg) nil)
                             :args (cons *cont-id*
                                         (mapcar (lambda (item)
                                                   (tx-cps item nil))
                                                 (new-expr-args arg)))))
              
              ;; Simple return
              (t
               (make-fn-call :fn *cont-id*
                             :args (unless (null (return-statement-arg elm))
                                     (list (tx-cps (return-statement-arg elm) nil))))))))
      (values
       (make-return-statement :arg new-fn-call)
       nil))))
      

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
                                (fn-call-args elm))))

           ;; Call w/statement-tail
           (make-fn-call
            :fn (tx-cps (fn-call-fn elm) nil)
            :args (cons (make-continuation-function :parameters nil
                                                    :body (in-local-scope
                                                            (tx-cps statement-tail nil)))
                          (mapcar (lambda (item)
                                    (tx-cps item nil))
                                  (fn-call-args elm)))))))
    (if *in-local-scope*
      (values (make-return-statement :arg new-fn-call) t)
      (values new-fn-call t))))

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
                                (new-expr-args elm)))))))
    (if *in-local-scope*
      (values (make-return-statement :arg tx-expr) t)
      (values tx-expr t))))

(defmethod tx-cps ((elm-list list) statement-tail)
  (unless (null elm-list)
    ;; Transform the first source element with the rest prepended to the statement tail.
    (multiple-value-bind (head consumed)
        (tx-cps (car elm-list) (append (cdr elm-list) statement-tail))
      
      ;; Guarantee that HEAD is a list
      (unless (listp head)
        (setf head (list head)))
      
      ;; If the statement tail was consumed, then the incoming STATEMENT-TAIL was consumed,
      ;; so return T as our second return value.  The CDR of elm-list was also consued,
      ;; so don't recurse.
      ;;
      ;; If the statement tail wasn't consumed, then pass the same statement tail to
      ;; the recursive call.
      (if consumed
        (values head t)
        (multiple-value-bind (tail recursive-consumed)
            (tx-cps (cdr elm-list) statement-tail)
          (values (append head tail) recursive-consumed))))))

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
                                                (fn-call-args initializer)))))
             (if *in-local-scope*
               (values (make-return-statement :arg new-call) t)
               (values new-call t))))

        ;; eg: var x = new Foo;
        ((new-expr-p initializer)
         (let ((new-construction (make-new-expr :constructor (tx-cps (new-expr-constructor initializer) nil)
                                                :args (cons
                                                       (make-continuation-function :parameters (list name)
                                                                                   :body (in-local-scope
                                                                                           (tx-cps augmented-statement-tail nil)))
                                                       (new-expr-args initializer)))))
           (if *in-local-scope*
             (values (make-return-statement :arg new-construction) t)
             (values new-construction t))))

        ;; eg: var x = 20;
        (initializer
         (if escaping-reference
           (multiple-value-bind (new-stmt consumed)
               (tx-cps assignment-stmt statement-tail)
             (values new-stmt consumed))
           (multiple-value-bind (new-decl consumed)
               (tx-cps (car var-decls) statement-tail)
             (values (make-var-decl-statement :var-decls (list new-decl))
                     consumed))))

        ;; eg: var x;
        (t
         (if escaping-reference
           (values nil nil)
           (values elm nil)))))))

;;;================================================================================
;;;; function_continuation transformation

(defmethod tx-cps ((elm special-value) statement-tail)
  (if (eq :function_continuation
          (special-value-symbol elm))
    (values *cont-id* nil)
    (call-next-method)))

;;;================================================================================
;;;; loop transformation

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
     (make-resume-statement :target (make-identifier :name break-name))
     nil)))

(defmethod tx-cps ((elm continue-statement) statement-tail)
  (declare (ignore statement-tail))
  (let ((continue-name (aif (continue-statement-target-label elm)
                            (find-binding (concatenate 'string it "$continue"))
                            *nearest-continue*)))
    (values
     (make-resume-statement :target (make-identifier :name continue-name))
     nil)))

;;;================================================================================
;;;; Branch statements

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
                                 :then-statement (single-statement (tx-cps then-statement nil)))
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
                                 :else-statement (single-statement tx-else))
              else-consumed)))
          
          ((and (not then-terminated) else-terminated)
           (multiple-value-bind (tx-then then-consumed)
               (tx-cps then-statement statement-tail)
             (values
              (make-if-statement :condition (tx-cps condition nil)
                                 :then-statement (single-statement tx-then)
                                 :else-statement (single-statement (tx-cps else-statement nil)))
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
                                   :else-statement (single-statement tx-else))
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
                                                           nil))))
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
                                      terminated-clauses)))
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
                                      :body extended-body)
                    (make-default-clause :body extended-body))
          when (and (null (cdr clause-tail))
                    (zerop defaults-encountered))
          collect (make-default-clause :body (list (make-break-statement :target-label nil))))))

;;;================================================================================
;;;; default behaviour 
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
                                                (slot-tx prop-value))))
       consumed))))

;;;; ======= FIND-FREE-VARIABLES generic function ==================================================

;; TODO this is horrible because it duplicates so much code in uniquify

(defgeneric find-free-variables (elm)
  (:documentation
   "Return a list of all the free variables in ELM"))

(defun find-free-variables-in-scope (elm)
  "This is basically TRANSFORM-IN-SCOPE.  It adds bindings for each variable and function
   declaration that it encounters."
  (dolist (var-decl (collect-in-scope elm 'var-decl)) 
    (add-binding (var-decl-name var-decl) t))
  (dolist (fun-decl (collect-in-scope elm 'function-decl))
    (add-binding (function-decl-name fun-decl) t))
  (find-free-variables elm))

(defmethod find-free-variables ((elm identifier))
  (with-slots (name) elm
    (unless (find-binding name)
      (list name))))

(defmethod find-free-variables ((elm function-decl))
  (with-added-environment
    (dolist (param (function-decl-parameters elm))
      (add-binding param t))
    (find-free-variables-in-scope (function-decl-body elm))))

(defmethod find-free-variables ((elm function-expression))
  (with-added-environment
    (when-let (name (function-expression-name elm))
      (add-binding name t))
    (dolist (param (function-expression-parameters elm))
      (add-binding param t))
    (find-free-variables-in-scope (function-expression-body elm))))

(defmethod find-free-variables ((elm-list list))
  (remove-duplicates
   (mapcan #'find-free-variables elm-list)
   :test #'equal))

(defmethod find-free-variables ((elm source-element))
  (remove-duplicates
   (loop for slot in (structure-slots elm)
         append (find-free-variables (slot-value elm slot)))
   :test #'equal))

(defmethod find-free-variables ((elm object-literal))
  (remove-duplicates
   (loop for prop-cell in (object-literal-properties elm)
         append (find-free-variables (cdr prop-cell)))
   :test #'equal))

(defmethod find-free-variables (elm)
  nil)

(defmethod find-free-variables :around (elm)
  (if (null *environment*)
    (with-added-environment
      (if (listp elm)
        (find-free-variables-in-scope elm)
        (call-next-method)))
    (call-next-method)))
