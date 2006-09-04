;;;; explicitize-transformation.lisp
;;;
;;; Defines the explicitize source transformation and supporting
;;; functionality.
;;;
;;; Copyright (c) 2005-2006 James Wright
;;; See LICENSE for full licensing details.
;;;
(in-package :jwacs)

;;;; ======= Explicitize transformation ============================================================
;;;
;;; Makes intermediate values explicit by assigning them to named variables.
;;; An intermediate value is any value that is passed to an operator or
;;; function that was produced by another operator or function.
;;; We make exceptions for values that are produced by operators applied
;;; to non-function-calls.  So in the code
;;;   bar(50 + foo())
;;; the expression `foo()` produces an intermediate value that will be
;;; explicitly named.  However, in the similar code
;;;   bar(50 + 100)
;;; we don't treat `50 + 100` as producing an intermediate value, because
;;; it is an operator applied to two constants.  Same goes for `50 + x`
;;; in the code
;;;   bar(50 + x)
;;;
;;; As a convenience, the explicitize transformation also breaks up
;;; var-decl-statements that contain more than one var-decl into multiple
;;; single-decl var-decl-statements.  So
;;;   var x = 10, y = 20
;;; will become
;;;   var x = 10;
;;;   var y = 20;

;;;; ======= Implementation note ===================================================================
;;;
;;; The explicitize transformation has a more complicated protocol than most of
;;; the other transformations, so the TRANSFORM method is just a thin wrapper that
;;; calls a separate generic function called TX-EXPLICITIZE and translates the
;;; results to the TRANSFORM protocol.
;;;
;;; Sometimes a source-element will be in a position
;;; in the code where it can be replaced with a series of new statements.  Ex:
;;;
;;;   echo('hello', newline());
;;;
;;; can legally be replaced by
;;;
;;;   var JW0 = newline();
;;;   echo('hello', JW0);
;;;
;;; However, at other times, a statement will be "nested" in such a way that it
;;; is /not/ legal to replace it with a series of arbitrary statements (this is
;;; usually when it is nested into an expression).  In this code
;;;
;;;   return process('hello', newline()) + process('seeya', newline());
;;;
;;; the first call to `process` cannot be replaced with an arbitrary list of
;;; statements.  Instead, we can place some "prerequisite" statements before the
;;; entire return statement, and represent the value that results from executing
;;; the prerequisites by a "proxy" variable that will be defined as part of the
;;; prerequisites:
;;;
;;;   var JW0 = newline();
;;;   var JW1 = process('hello', JW0);
;;;   var JW2 = newline();
;;;   var JW3 = process('seeya', JW2);
;;;   return JW1 + JW3;
;;;
;;; In this case, `JW1` is the proxy for the first call to process, and `JW3` is
;;; the proxy for the second call to process.
;;;
;;; The main job of the explicitization transformation is to make sure that every
;;; function call in a nested context is replaced with a proxy variable.
;;;
;;; Since the fn-call method needs to return a proxy with prerequisites when it
;;; is in a nested context, but may return a function call (possibly also with
;;; some prerequisites) in a non-nested context, we need some way to tell it whether
;;; it is in a nested context or not.  We use the dynamic variable *NESTED-CONTEXT*
;;; to track this information (see next section comment).
;;;
;;; The TX-EXPLICITIZE transformation function returns 2 values.
;;; The first value is a source-element that should go wherever the original element
;;; lived in the parse tree.  I refer to this as the "proxy" value, even though it
;;; isn't always a proxy (it may just be a transformed version of the original element).
;;;
;;; The second value is a list of "prerequisite" statements that should go before the
;;; innermost enclosing non-nested statement (which may be this statement).

;;;; ======= Sub-expression protocol ===============================================================
;;;
;;; Transformations are responsible for knowing whether they are nested within subexpressions
;;; or not, and therefore whether they should return using a proxy.  For example, the element
;;;    foo(bar());
;;; should return `foo(JWn);` with prereqs of `var JWn = bar();` when it is
;;; not nested.  However, when it /is/ nested, it should return `JWn` with prereqs
;;; of `var JWm = bar(); var JWn = foo(JWm);`.
;;;
;;; In order for the methods of TX-EXPLICITIZE to know whether they are in a nested
;;; context or not, we bind *NESTED-CONTEXT* to T when calling TX-EXPLICITIZE on nested expressions.

;;TODO I'm not sure I like the term "nested".  Better ideas, anyone?

(defparameter *nested-context* nil
  "T when processing nested subexpressions")

(defun nested-explicitize (elm)
  "Call TX-EXPLICITIZE on ELM in a nested context (ie, with *NESTED-CONTEXT* bound to T)"
  (let ((*nested-context* t))
    (tx-explicitize elm)))

(defun unnested-explicitize (elm)
  "Call TX-EXPLICITIZE on ELM in an unnested context (ie, with *NESTED-CONTEXT* bound to NIL)"
  (let ((*nested-context* nil))
    (tx-explicitize elm)))

(defmacro with-nesting (&body body)
  "Execute the forms of BODY with *NESTED-CONTEXT* bound to T."
  `(let ((*nested-context* t))
    ,@body))

;;;; ======= TRANSFORM method ======================================================================

(defmethod transform ((xform (eql 'explicitize)) elm)
  (multiple-value-bind (proxy prereqs)
      (unnested-explicitize elm)
    (if (null prereqs)
      proxy
      (postpend prereqs proxy))))

;; These elements should have been removed by LOOP-TO-FUNCTION
(forbid-transformation-elements explicitize (do-statement for))

;;;; ======= TX-EXPLICITIZE methods ================================================================

(defgeneric tx-explicitize (elm)
  (:documentation
   "Performs the explicitization transformation on ELM and returns two values.

    The first value is a 'proxy' that should be place in the source tree at the
    same location that ELM originally occupied.

    The second value is a list of 'prerequisite' statements that should go
    immediately before the nearest enclosing statement of ELM.  The nearest
    enclosing statement is a simple statement.  The list of prerequisites may
    be NIL."))

(defmethod tx-explicitize ((elm fn-call))
  (loop for arg in (fn-call-args elm)
        for (proxy prereq) = (multiple-value-list (nested-explicitize arg))
        collect proxy into new-args
        append prereq into new-stmts
        finally
        (multiple-value-bind (call-proxy call-prereqs)
            (nested-explicitize (fn-call-fn elm))
          (let ((new-stmts (append call-prereqs new-stmts)) ; The new binding uses the old binding
                (new-elm (make-fn-call :fn call-proxy
                                       :args new-args
                                       :start (source-element-start elm)
                                       :end (source-element-end elm))))
            (if *nested-context*
              (let ((new-var (genvar)))
                (return (values (make-identifier :name new-var)
                                (postpend new-stmts (make-var-init new-var new-elm)))))
              (return (values new-elm
                              new-stmts)))))))

(defmethod tx-explicitize ((elm new-expr))
  (loop for arg in (new-expr-args elm)
        for (proxy prereq) = (multiple-value-list (nested-explicitize arg))
        collect proxy into new-args
        append prereq into new-stmts
        finally
        (multiple-value-bind (ctor-proxy ctor-prereqs)
            (nested-explicitize (new-expr-constructor elm))
          (let ((new-stmts (append ctor-prereqs new-stmts))
                (new-elm (make-new-expr :constructor ctor-proxy
                                        :args new-args
                                        :start (source-element-start elm)
                                        :end (source-element-end elm))))
            ;; A new expression is an implicit function call to the constructor
            ;; which we make explicit through a call to $new,
            ;; so we need to pull it out of nested contexts.
            (if *nested-context*
              (let ((new-var (genvar)))
                (return (values (make-identifier :name new-var)
                                (postpend new-stmts (make-var-init new-var new-elm)))))
              (return (values new-elm
                              new-stmts)))))))
(defmethod tx-explicitize ((elm resume-statement))
  (multiple-value-bind (target-proxy target-prereqs)
      (nested-explicitize (resume-statement-target elm))
    (multiple-value-bind (arg-proxy arg-prereqs)
        (nested-explicitize (resume-statement-arg elm))
      (values (make-resume-statement :target target-proxy
                                     :arg arg-proxy
                                     :start (source-element-start elm)
                                     :end (source-element-end elm))
              (append target-prereqs
                      arg-prereqs)))))

(defmethod tx-explicitize ((elm throw-statement))
  (multiple-value-bind (value-proxy value-prereqs)
      (nested-explicitize (throw-statement-value elm))
    (multiple-value-bind (target-proxy target-prereqs)
        (nested-explicitize (throw-statement-target elm))
      (values (make-throw-statement :value value-proxy
                                    :target target-proxy
                                    :start (source-element-start elm)
                                    :end (source-element-end elm))
              (append value-prereqs
                      target-prereqs)))))

(defmethod tx-explicitize ((elm var-decl-statement))
  (if (> (length (var-decl-statement-var-decls elm)) 1)
    (tx-explicitize (mapcar (lambda (decl)
                                      (make-var-decl-statement :var-decls (list decl)
                                                               :start (source-element-start decl)
                                                               :end (source-element-end decl)))
                                    (var-decl-statement-var-decls elm)))
    (multiple-value-bind (proxy prereqs)
        (tx-explicitize (first (var-decl-statement-var-decls elm)))
      (values (make-var-decl-statement :var-decls (list proxy)
                                       :start (source-element-start elm)
                                       :end (source-element-end elm))
              prereqs))))

;; We need to override the default handling to ensure that the pre-statments for the
;; then-statement occur inside the then-statement instead of before the entire if-statement,
;; and similarly for the else-statement.  Otherwise portions of the then-statement (AND
;; else-statement) might be executed unconditionally.
(defmethod tx-explicitize ((elm if-statement))
  (multiple-value-bind (cond-proxy cond-prereqs)
      (nested-explicitize (if-statement-condition elm))
    (multiple-value-bind (then-proxy then-prereqs)
        (unnested-explicitize (if-statement-then-statement elm))
      (multiple-value-bind (else-proxy else-prereqs)
          (unnested-explicitize (if-statement-else-statement elm))
        (values (make-if-statement :condition cond-proxy
                                   :then-statement (single-statement then-prereqs then-proxy)
                                   :else-statement (single-statement else-prereqs else-proxy)
                                   :start (source-element-start elm)
                                   :end (source-element-end elm))
                cond-prereqs)))))

(defmethod tx-explicitize ((elm switch))
  (multiple-value-bind (cond-proxy cond-prereqs)
      (nested-explicitize (switch-value elm))
    (values
     (make-switch :value cond-proxy
                  :clauses (mapcar (lambda (clause) ; Clauses never have prereqs, so a simple mapcar is fine
                                     (unnested-explicitize clause))
                                   (switch-clauses elm))
                  :start (source-element-start elm)
                  :end (source-element-end elm))
     cond-prereqs)))

(defmethod tx-explicitize ((elm case-clause))
  (multiple-value-bind (body-proxy body-prereqs)
      (unnested-explicitize (case-clause-body elm))
    (make-case-clause :value (case-clause-value elm)
                      :body
                      (cond
                        ((statement-block-p body-proxy)
                         (make-statement-block :statements (append body-prereqs
                                                                   (statement-block-statements body-proxy))))
                        ((listp body-proxy)
                         (append body-prereqs body-proxy))
                        (t
                         (postpend body-prereqs body-proxy)))
                      :start (source-element-start elm)
                      :end (source-element-end elm))))

(defmethod tx-explicitize ((elm while))
  (assert (idempotent-expression-p (while-condition elm))) ; LOOP-CANONICALIZATION should reduce all while loops to idempotent conditions (viz. `true`)
  (multiple-value-bind (body-proxy body-prereqs)
      (unnested-explicitize (while-body elm))
    (values (make-while :label (source-element-label elm)
                        :condition (while-condition elm)
                        :body (single-statement body-prereqs body-proxy)
                        :start (source-element-start elm)
                        :end (source-element-end elm))
            nil)))

(defmethod tx-explicitize ((elm for-in))
  (multiple-value-bind (collection-proxy collection-prereqs)
      (nested-explicitize (for-in-collection elm))
    (multiple-value-bind (body-proxy body-prereqs)
        (unnested-explicitize (for-in-body elm))
      (values (make-for-in :binding (for-in-binding elm)
                           :collection collection-proxy
                           :body (single-statement body-prereqs body-proxy)
                           :start (source-element-start elm)
                           :end (source-element-end elm))
              collection-prereqs))))

(defmethod tx-explicitize ((elm conditional))
  (multiple-value-bind (cond-proxy cond-prereqs)
      (nested-explicitize (conditional-condition elm))
    (multiple-value-bind (then-proxy then-prereqs)
        (nested-explicitize (conditional-true-arg elm))
      (multiple-value-bind (else-proxy else-prereqs)
          (nested-explicitize (conditional-false-arg elm))
        (cond
          ;; If neither of the non-guaranteed branches have prereqs, then
          ;; no need to do any special processing
          ((and (null then-prereqs)
                (null else-prereqs))
           (values (make-conditional :condition cond-proxy
                                     :true-arg then-proxy
                                     :false-arg else-proxy
                                     :start (source-element-start elm)
                                     :end (source-element-end elm))
                   cond-prereqs))
          ;; If the cond-proxy is idempotent, then we can just wrap the
          ;; non-guaranteed prereqs in an if-statement and be done with it
          ((idempotent-expression-p cond-proxy)
           (values (make-conditional :condition cond-proxy
                                     :true-arg then-proxy
                                     :false-arg else-proxy
                                     :start (source-element-start elm)
                                     :end (source-element-end elm))
                   (postpend cond-prereqs
                             ;;TODO make an if with a negated conditional if there are only else-prereqs
                             (make-if-statement :condition cond-proxy
                                                :then-statement (single-statement then-prereqs)
                                                :else-statement (single-statement else-prereqs)))))
          ;; Non-idempotent cond proxy with at least
          ;; one non-guaranteed prereq
          (t
           (let* ((cond-proxy-name (genvar))
                  (cond-prereqs (postpend cond-prereqs
                                          (make-var-init cond-proxy-name cond-proxy)))
                  (cond-proxy (make-identifier :name cond-proxy-name)))
             (values (make-conditional :condition cond-proxy
                                       :true-arg then-proxy
                                       :false-arg else-proxy
                                       :start (source-element-start elm)
                                       :end (source-element-end elm))
                     (postpend cond-prereqs
                               ;;TODO make an if with a negated conditional if there are only else-prereqs
                               (make-if-statement :condition cond-proxy
                                                  :then-statement (single-statement then-prereqs)
                                                  :else-statement (single-statement else-prereqs)))))))))))


(defun explicitize-short-circuit-operator (elm)
  "Transforms ELM (should be a BINARY-OPERATOR) in a way that preserves the short-circuit semantics"
  (assert (binary-operator-p elm))
  (flet ((make-if-wrapper (cond-proxy wrapped-prereqs)
           "Create the kind of if wrapper for WRAPPED-PREREQS that is appropriate for
            ELM's operator"
           (make-if-statement :condition
                              (ecase (binary-operator-op-symbol elm)
                                (:logical-and cond-proxy)
                                (:logical-or (make-unary-operator :op-symbol :logical-not
                                                                  :arg cond-proxy)))
                              :then-statement (single-statement wrapped-prereqs))))
    (multiple-value-bind (left-proxy left-prereqs)
        (nested-explicitize (binary-operator-left-arg elm))
      (multiple-value-bind (right-proxy right-prereqs)
          (nested-explicitize (binary-operator-right-arg elm))
        (cond
          ;; No special handling required if right arg has no prereqs
          ((null right-prereqs)
           (values (make-binary-operator :op-symbol (binary-operator-op-symbol elm)
                                         :left-arg left-proxy
                                         :right-arg right-proxy
                                         :start (source-element-start elm)
                                         :end (source-element-end elm))
                   left-prereqs))
          ;; If the right arg has prereqs, they need to be wrapped in an if.
          ;; If the left arg's proxy is idempotent, then we needn't add a variable initialization for it
          ((idempotent-expression-p left-proxy)
           (values (make-binary-operator :op-symbol (binary-operator-op-symbol elm)
                                         :left-arg left-proxy
                                         :right-arg right-proxy
                                         :start (source-element-start elm)
                                         :end (source-element-end elm))
                   (postpend left-prereqs
                             (make-if-wrapper left-proxy right-prereqs))))
          ;; If the right arg has prereqs and the left proxy is not idempotent, we need to
          ;; construct a left proxy and use it to wrap the right prereqs.
          (t
           (let* ((left-proxy-name (genvar))
                  (left-prereqs (postpend left-prereqs
                                          (make-var-init left-proxy-name left-proxy)))
                  (left-proxy (make-identifier :name left-proxy-name)))
             (values (make-binary-operator :op-symbol (binary-operator-op-symbol elm)
                                           :left-arg left-proxy
                                           :right-arg right-proxy
                                           :start (source-element-start elm)
                                           :end (source-element-end elm))
                     (postpend left-prereqs
                               (make-if-wrapper left-proxy right-prereqs))))))))))

(defmethod tx-explicitize ((elm binary-operator))
  (if (member (binary-operator-op-symbol elm)
              '(:logical-and :logical-or))
    (explicitize-short-circuit-operator elm)
    (with-nesting
      (call-next-method))))

(defmethod tx-explicitize ((elm unary-operator))
  (with-nesting
    (call-next-method)))

(defmethod tx-explicitize ((elm property-access))
  (multiple-value-bind (target-proxy target-prereqs)
      (nested-explicitize (property-access-target elm))
    (multiple-value-bind (field-proxy field-prereqs)
        (nested-explicitize (property-access-field elm))
      (values (make-property-access :target target-proxy
                                    :field field-proxy
                                    :start (source-element-start elm)
                                    :end (source-element-end elm))
              (append target-prereqs field-prereqs)))))

(defmethod tx-explicitize ((elm comma-expr))
  (loop for expr in (comma-expr-exprs elm)
        for (expr-proxy expr-prereqs) = (multiple-value-list (nested-explicitize expr))
        append expr-prereqs into prereqs
        collect expr-proxy into proxies
        finally (return (values (make-comma-expr :exprs proxies
                                                 :start (source-element-start elm)
                                                 :end (source-element-end elm))
                                prereqs))))
      
(defmethod tx-explicitize ((elm object-literal))
  (loop for (prop-name . prop-val) in (object-literal-properties elm)
        for (val-proxy val-prereqs) = (multiple-value-list (nested-explicitize prop-val))
        collect (cons prop-name val-proxy) into props
        append val-prereqs into prereqs
        finally (return (values (make-object-literal :properties props
                                                     :start (source-element-start elm)
                                                     :end (source-element-end elm))
                                prereqs))))

(defmethod tx-explicitize ((elm-list list))
  (loop for elm in elm-list
        for (proxy prereqs) = (multiple-value-list (unnested-explicitize elm))
        append prereqs
        if (listp proxy)
          append proxy
        else
          collect proxy))

;; For the EXPLICITIZE transformation, the default behaviour must collect prerequisite
;; statements as well as proxies.
(defmethod tx-explicitize ((elm source-element))
  (let ((pre-stmts '()))
    (values
     (apply
      (get-constructor elm)
      (loop for slot in (structure-slots elm)
            for (proxy prereqs) = (multiple-value-list (tx-explicitize (slot-value elm slot)))
           collect (make-keyword slot)
           collect proxy
           do (setf pre-stmts (append pre-stmts prereqs))))
     pre-stmts)))

(defmethod tx-explicitize (elm)
  elm)
