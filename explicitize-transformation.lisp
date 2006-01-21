;;;; explicitize-transformation.lisp
;;;
;;; Defines the explicitize source transformation and supporting
;;; functionality.
(in-package :jwacs)

;;================================================================================
;;;; Explicitize transformation
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

;;================================================================================
;;;; Implementation note
;;;
;;; The explicitize transformation has a more complicated protocol than most of
;;; the other transformations.  Sometimes a source-element will be in a position
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
;;; entire return statement, and represent the valuet that results from executing
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
;;; The TRANSFORM method for the EXPLICITIZE transformation returns 2 values.
;;; The first value is a source-element that should go wherever the original element
;;; lived in the parse tree.  I refer to this as the "proxy" value, even though it
;;; isn't always a proxy (it may just be a transformed version of the original element).
;;;
;;; The second value is a list of "prerequisite" statements that should go before the
;;; innermost enclosing non-nested statement (which may be this statement).

;;================================================================================
;;;; Sub-expression protocol
;;;
;;; Transformations are responsible for knowing whether they are nested within subexpressions
;;; or not, and therefore whether they should return using a proxy.  For example, the element
;;;    foo(bar());
;;; should return `foo(JWn);` with prereqs of `var JWn = bar();` when it is
;;; not nested.  However, when it /is/ nested, it should return `JWn` with prereqs
;;; of `var JWm = bar(); var JWn = foo(JWm);`.
;;;
;;; In order for the explicitization methods of TRANSFORM to know whether they are in a nested
;;; context or not, we bind *NESTED-CONTEXT* to T when calling TRANSFORM on nested expressions.

;;TODO I'm not sure I like the term "nested".  Better ideas, anyone?

(defparameter *nested-context* nil
  "T when processing nested subexpressions")

(defun nested-transform (xform elm)
  "Apply XFORM to ELM in a nested context (ie, with *NESTED-CONTEXT* bound to T)"
  (let ((*nested-context* t))
    (transform xform elm)))

(defmacro with-nesting (&body body)
  "Execute the forms of BODY with *NESTED-CONTEXT* bound to T."
  `(let ((*nested-context* t))
    ,@body))
    
;;TODO move this somewhere more general, use it generally
(defun make-var-init (var-name init-value)
  "Create a VAR-DECL-STATEMENT that initializes a variable named VAR-NAME to INIT-VALUE"
  (make-var-decl-statement :var-decls
                           (list (make-var-decl :name var-name :initializer init-value))))

;;================================================================================
;;;; TRANSFORM methods

;; These elements should have been removed by LOOP-TO-FUNCTION
(forbid-transformation-elements explicitize (do-statement for))

(defmethod transform ((xform (eql 'explicitize)) (elm fn-call))
  (loop for arg in (fn-call-args elm)
        for (proxy prereq) = (multiple-value-list (nested-transform 'explicitize arg))
        collect proxy into new-args
        nconc prereq into new-stmts
        finally
        (multiple-value-bind (call-proxy call-prereqs)
            (nested-transform 'explicitize (fn-call-fn elm))
          (let ((new-stmts (nconc call-prereqs new-stmts)) ; The new binding uses the old binding
                (new-elm (make-fn-call :fn call-proxy
                                       :args new-args)))
            (if *nested-context*
              (let ((new-var (genvar)))
                (return (values (make-identifier :name new-var)
                                (nconc new-stmts (list (make-var-init new-var new-elm))))))
              (return (values new-elm
                              new-stmts)))))))

(defmethod transform ((xform (eql 'explicitize)) (elm var-decl-statement))
  (if (> (length (var-decl-statement-var-decls elm)) 1)
    (transform 'explicitize (mapcar (lambda (decl)
                                      (make-var-decl-statement :var-decls (list decl)))
                                    (var-decl-statement-var-decls elm)))
    (multiple-value-bind (proxy prereqs)
        (transform 'explicitize (first (var-decl-statement-var-decls elm)))
      (values (make-var-decl-statement :var-decls (list proxy))
              prereqs))))

;; We need to override the default handling to ensure that the pre-statments for the
;; then-statement occur inside the then-statement instead of before the entire if-statement,
;; and similarly for the else-statement.  Otherwise portions of the then-statement (AND
;; else-statement) might be executed unconditionally.
(defmethod transform ((xform (eql 'explicitize)) (elm if-statement))
  (flet ((maybe-block (proxy prereqs)
           "Combine PROXY and PREREQS into a block if necessary"
           (cond
             ((null proxy)
              (if prereqs
                (make-statement-block :statements prereqs)
                nil))
             ((listp proxy)
              (make-statement-block :statements (append prereqs proxy)))
             ((statement-block-p proxy)
              (make-statement-block :statements (append prereqs 
                                                        (statement-block-statements proxy))))
             (prereqs
              (make-statement-block :statements (append prereqs (list proxy))))
             (t
              proxy))))
    (multiple-value-bind (cond-proxy cond-prereqs)
        (nested-transform 'explicitize (if-statement-condition elm))
      (multiple-value-bind (then-proxy then-prereqs)
          (transform 'explicitize (if-statement-then-statement elm))
        (multiple-value-bind (else-proxy else-prereqs)
            (transform 'explicitize (if-statement-else-statement elm))
          (values (make-if-statement :condition cond-proxy
                                     :then-statement (maybe-block then-proxy then-prereqs)
                                     :else-statement (maybe-block else-proxy else-prereqs))
                  cond-prereqs))))))

(defmethod transform ((xform (eql 'explicitize)) (elm switch))
  (multiple-value-bind (cond-proxy cond-prereqs)
      (nested-transform 'explicitize (switch-value elm))
    (values
     (make-switch :value cond-proxy
                  :clauses (mapcar (lambda (clause) ; Clauses never have prereqs, so a simple mapcar is fine
                                     (transform 'explicitize clause))
                                   (switch-clauses elm)))
     cond-prereqs)))

(defmethod transform ((xform (eql 'explicitize)) (elm case-clause))
  (multiple-value-bind (body-proxy body-prereqs)
      (transform 'explicitize (case-clause-body elm))
    (make-case-clause :label (case-clause-label elm)
                      :body
                      (cond
                        ((statement-block-p body-proxy)
                         (make-statement-block :statements (append body-prereqs
                                                                   (statement-block-statements body-proxy))))
                        ((listp body-proxy)
                         (append body-prereqs body-proxy))
                        (t
                         (append body-prereqs (list body-proxy)))))))                        

;;TODO special handling for AND and OR expressions
(defmethod transform ((xform (eql 'explicitize)) (elm binary-operator))
  (with-nesting
    (call-next-method)))

(defmethod transform ((xform (eql 'explicitize)) (elm unary-operator))
  (with-nesting
    (call-next-method)))

(defmethod transform ((xform (eql 'explicitize)) (elm object-literal))
  (loop for (prop-name . prop-val) in (object-literal-properties elm)
        for (val-proxy val-prereqs) = (multiple-value-list (nested-transform 'explicitize prop-val))
        collect (cons prop-name val-proxy) into props
        nconc val-prereqs into prereqs
        finally (return (values (make-object-literal :properties props)
                                prereqs))))

(defmethod transform ((xform (eql 'explicitize)) (elm-list list))
  (loop for elm in elm-list
        for (proxy prereqs) = (multiple-value-list (transform 'explicitize elm))
        nconc prereqs
        if (listp proxy)
          nconc proxy
        else
          collect proxy))

;; For the EXPLICITIZE transformation, the default behaviour must collect prerequisite
;; statements as well as proxies.
;; TODO Automatically add blocks if a singleton statement is converted to a 
(defmethod transform ((xform (eql 'explicitize)) (elm source-element))
  (let ((fresh-elm (funcall (get-constructor elm))))
    (loop for slot in (structure-slots elm)
          for (proxy prereqs) = (multiple-value-list (transform xform (slot-value elm slot)))
          do
          (setf (slot-value fresh-elm slot)
                proxy)
          nconc prereqs into pre-stmts
          finally (return (values fresh-elm pre-stmts)))))
