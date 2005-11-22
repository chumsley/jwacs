;;;; source-transformations.lisp
;;;
;;; Implements source transformations.  The interface is through the TRANSFORM generic function.
;;; Unit tests are in tests/test-source-transformations.lisp.

(in-package :jwacs)

;;;;= Utility functions =
;;TODO Structure-slots and friends may want to go into a separate "structure-helpers" file
(defun structure-slots (object)
  "Returns a list of the slot-names of the provided structure object"
  #+openmcl
  (let* ((sd (gethash (class-name (class-of object)) ccl::%defstructs%))
	 (slots (if sd (ccl::sd-slots sd))))
    (mapcar #'car (if (symbolp (caar slots)) slots (cdr slots))))
  #+cmu
  (mapcar #'pcl:slot-definition-name (pcl:class-slots (class-of object)))
  #+lispworks
  (structure:structure-class-slot-names (class-of object))
  #+sbcl
  (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (class-of object)))
  #+allegro
  (mapcar #'mop:slot-definition-name (mop:class-slots (class-of object))))

(defparameter *constructor-cache* (make-hash-table :test 'eq)
  "Map from structure-type symbol to copy-function symbol")

(defun get-constructor (struct-object)
  "Accept a structure object and return the (likely) name of its constructor.
   CAVEAT: Assumes that the structure was defined in the same package as its name."
  (let* ((name (type-of struct-object))
         (make-fn (gethash name *constructor-cache*)))
    (if (null make-fn)
      (setf (gethash name *constructor-cache*) (intern (format nil "MAKE-~A" name)
                                                       (symbol-package name)))
      make-fn)))

;;;;= Default transformation behaviour =

;;; The top-level TRANSFORM methods provide the default code-walking behaviour,
;;; so that individual transformations can override just the important parts.

(defgeneric transform (xform elm)
  (:documentation
   "Accepts a transformation name (symbol) and a source element, and returns a new
    source element that has been transformed in some way.  Methods should /not/ perform
    destructive updates on the provided source-element."))

;; The default behaviour for any transformation is to do nothing
(defmethod transform (xform elm)
  elm)

;; The default behaviour for any transformation on a source-element that has children
;; is to return a new source-element whose children have been transformed.
(defmethod transform (xform (elm source-element))
  (let ((fresh-elm (funcall (get-constructor elm))))
    (dolist (slot (structure-slots elm))
      (setf (slot-value fresh-elm slot)
            (transform xform (slot-value elm slot))))
    fresh-elm))

(defmethod transform (xform (elm list))
  (mapcar (lambda (arg)
            (transform xform arg))
          elm))

;;;;= Explicitize transformation =
;;; The explicitize transformation gives each intermediate value a
;;; name.  After the transformation is done, the result of every
;;; function call will be assigned to a variable.
;;;
;;; As a convenience, variable statements that contain declarations for
;;; multiple variables will also be converted to series of single
;;; variable declarations (eg `var x, y=10;` ---> `var x; var y = 10;`)

(defun expose-intermediate (elm)
  "Return a cons cell whose CAR is the name of the result of this element,
   and whose CDR is a list of statements that need to be added before the element"
  (cond
    ((fn-call-p elm)
     (let ((new-var (genvar)))
       (list (make-identifier :name new-var)
             (make-var-decl-statement
              :var-decls (list (make-var-decl :name new-var :initializer elm))))))
    ((listp elm)
     (let ((new-var (genvar))
           (final-stmt (car (last elm))))
       (assert (not (var-decl-statement-p final-stmt)))

       ;; The last statement in a statement list is a version of the original that contains no nested
       ;; intermediate expressions.  If it is a function call, then we convert it to a var decl and
       ;; return the var name as the expression to reference.  Otherwise, we remove it and return it
       ;; as the expression to reference.  This helps to avoid redundant code like
       ;;       `var r3 = r1 + r2; foo(r3);`
       ;; In the above expression, it is safe to eliminate `r3` and call `foo(r1 + r2)` directly, since
       ;; operator calls never need to be converted to CPS.
       (if (fn-call-p final-stmt)
         (cons (make-identifier :name new-var)
               (substitute (make-var-decl-statement
                            :var-decls (list (make-var-decl :name new-var :initializer final-stmt)))
                           final-stmt
                           elm
                           :from-end t
                           :count 1))
         (cons final-stmt
               (remove final-stmt elm :from-end t :count 1)))))
                
    (t (list elm))))

(defmethod transform ((xform (eql 'explicitize)) (elm fn-call))
  (let ((transformed-args (mapcar (lambda (x) (transform 'explicitize x))
                                  (fn-call-args elm))))
    (loop for tx-arg in transformed-args
          for intermediates = (expose-intermediate tx-arg)
          collect (car intermediates) into new-args
          nconc (cdr intermediates) into new-stmts
          finally
          (let ((new-elm (make-fn-call :fn (fn-call-fn elm)
                                       :args new-args)))
            (if new-stmts
              (return (nconc new-stmts (list new-elm)))
              (return new-elm))))))
  
(defun explicitize-rhs (elm &optional assignment-slot)
  "Helper function for transforming the right-hand-side of assignments and initializations.
   Returns either an updated copy of ELM, or a list of statements that should go roughly
   where ELM used to be."
  (let ((slot-names (structure-slots elm))
        (pre-statements nil)
        (new-elm (funcall (get-constructor elm))))
    (loop for slot in slot-names
          for intermediates = (expose-intermediate (transform 'explicitize (slot-value elm slot)))
          do (setf (slot-value new-elm slot) (car intermediates))
          nconc (cdr intermediates) into pre-statements-l
          finally (setf pre-statements pre-statements-l))

    (cond
      ((and pre-statements
            assignment-slot)
       (let ((final-pre-statement (car (last pre-statements))))
         (assert (and ;TODO I /think/ this is guaranteed, but I need to think about it more
                  (identifier-p (slot-value new-elm assignment-slot))
                  (var-decl-statement-p final-pre-statement)
                  (equal (identifier-name (slot-value new-elm assignment-slot))
                         (var-decl-name (car (var-decl-statement-var-decls final-pre-statement))))))
       (setf (slot-value new-elm assignment-slot)
             (var-decl-initializer (car (var-decl-statement-var-decls final-pre-statement))))
       (nconc (remove final-pre-statement pre-statements :from-end t :count 1)
              (list new-elm))))

      (pre-statements
       (nconc pre-statements (list new-elm)))

      (t
       new-elm))))

(defmethod transform ((xform (eql 'explicitize)) (elm binary-operator))
  (explicitize-rhs elm))

(defmethod transform ((xform (eql 'explicitize)) (elm var-decl))
  (explicitize-rhs elm 'initializer))

(defmethod transform ((xform (eql 'explicitize)) (elm return-statement))
  (explicitize-rhs elm 'arg))

(defmethod transform ((xform (eql 'explicitize)) (elm var-decl-statement))
  (let ((pre-statements nil)
        (new-decls nil))
    (loop for decl in (var-decl-statement-var-decls elm)
          for intermediates = (expose-intermediate (transform 'explicitize decl))
          collect (car intermediates) into new-decls-l
          nconc (cdr intermediates) into pre-statements-l
          finally
          (setf new-decls new-decls-l)
          (setf pre-statements pre-statements-l))
    (cond
      (pre-statements
       (nconc pre-statements
              (mapcar (lambda (x)
                        (make-var-decl-statement :var-decls (list x)))
                      new-decls)))
      ((> (length new-decls) 1)
       (mapcar (lambda (x)
                 (make-var-decl-statement :var-decls (list x)))
               new-decls))
      (t
       (make-var-decl-statement :var-decls new-decls)))))

(defmethod transform ((xform (eql 'explicitize)) (elm-list list))
  (unless (null elm-list)
    (let ((head (transform 'explicitize (car elm-list))))
      (if (listp head)
        (append head (transform 'explicitize (cdr elm-list)))
        (cons head  (transform 'explicitize (cdr elm-list)))))))


;;;;= CPS transformation =
;;; Initial, naive version does the following:
;;; - All function calls transformed to fn-calls in continuation-passing style
;;; - All assignments transformed to new continuations
;;; - All returns transformed to returns of the arg passed to the current continuation
;;;
;;;;== Preconditions ==
;;; The CPS transform assumes the following:
;;; 1. Scope analysis transformation has been performed (and therefore all identifiers are unique)
;;; 2. Explicitization transformation has been performed (and therefore all result values are unique)

(defun assignment-operator-p (elm)
  "Returns non-NIL if ELM represents an operation with an assignment side-effect"
  (and
   (binary-operator-p elm)
   (member (binary-operator-op-symbol elm)
           '(:assign :plus-equals :minus-equals :times-equals :divide-equals
             :lshift-equals :rshift-equals :urshift-equals
             :and-equals :or-equals :xor-equals))))

(defparameter *cont-name* "$k"
  "The name of the standard continuation parameter.  See SBCL-DEFPARAMETER-NOTE.")

(defparameter *cont-id* (make-identifier :name *cont-name*)
  "An identifier whose name is *cont-name*.  See SBCL-DEFPARAMETER-NOTE.")

;;;;== Statement tails ==
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
;;;;=== Statement tail protocol ===
;;;
;;; The `*statement-tail*` special variable holds the current state of the statement-tail.
;;; The macro `with-statement-tail` adds new statements to the current statement-tail (eg,
;;; when entering a new block scope) by prepending them to the current tail.
;;;
;;; The macro `consume-statement-tail` grabs the current statement-tail and sets it to null
;;; to indicate that it has been "used up", and therefore the statements in the tail needn't
;;; be processed, because they will have already been processed as part of generating a
;;; continuation at an innermore scope.
;;;
;;; When we're processing return statements, we don't want to include the current tail in
;;; any function calls that might be part of the return value.  The `without-statement-tail`
;;; macro will suppress the current statement tail (by temporarily binding it to null).

(defparameter *statement-tail* nil
  "Statements following the current statement in the current scope.")

(defmacro with-statement-tail ((new-statements) &body body)
  "Execute BODY with NEW-STATEMENTS prepended to the current statement-tail"
  `(let ((*statement-tail* (append ,new-statements *statement-tail*)))
    ,@body))

(defmacro consume-statement-tail ((tail-symbol) &body body)
  "Set the current statement tail to NIL (signalling that it has been consumed), and
   the execute BODY with TAIL-SYMBOL bound to the original value of *statement-tail*."
  `(let ((,tail-symbol *statement-tail*))
    (setf *statement-tail* nil)
    ,@body))

(defmacro without-statement-tail (&body body)
  "Execute BODY with *STATEMENT-TAIL* bound to NIL"
  `(let ((*statement-tail* nil))
    ,@body))

;;;;== CPS transform methods ==

(defmethod transform ((xform (eql 'cps)) (elm function-decl))
  (make-function-decl :name (function-decl-name elm)
                      :parameters (cons *cont-name* (function-decl-parameters elm))
                      :body (transform 'cps (function-decl-body elm))))

(defmethod transform ((xform (eql 'cps)) (elm return-statement))
  (make-return-statement :arg
                   (without-statement-tail
                     (make-fn-call :fn *cont-id*
                                   :args (list
                                          (transform 'cps (return-statement-arg elm)))))))

(defmethod transform ((xform (eql 'cps)) (elm fn-call))
  (if (null *statement-tail*)
    (make-return-statement :arg
                     (make-fn-call
                      :fn (fn-call-fn elm)
                      :args (cons *cont-id*
                                  (mapcar (lambda (item)
                                            (transform 'cps item))
                                          (fn-call-args elm)))))
    (make-return-statement :arg
                     (make-fn-call
                      :fn (fn-call-fn elm)
                      :args (consume-statement-tail (statement-tail)
                              (cons (make-function-expression :parameters (list (genvar))
                                                              :body (transform 'cps statement-tail))
                                    (mapcar (lambda (item)
                                              (transform 'cps item))
                                            (fn-call-args elm))))))))

(defmethod transform ((xform (eql 'cps)) (elm-list list))
  (unless (null elm-list)

    (let ((statements-consumed nil)
          (head nil))

      (with-statement-tail ((cdr elm-list))
        (setf head (transform 'cps (car elm-list)))
        (when (null *statement-tail*)
          (setf statements-consumed t)))
      
      (if statements-consumed
        (progn
          (setf *statement-tail* nil)
          (list head))
        (cons head (transform 'cps (cdr elm-list)))))))

(defmethod transform ((xform (eql 'cps)) (elm var-decl-statement))
  ;;TODO Assuming one decl per statment because that is one of the results of explicitization
  (with-slots (var-decls) elm
    (assert (<= (length var-decls) 1))
    (let ((name (var-decl-name (car var-decls)))
          (initializer (var-decl-initializer (car var-decls))))
      (cond
        ((fn-call-p initializer)
         (consume-statement-tail (statement-tail)
           (let ((new-call  (make-fn-call :fn (fn-call-fn initializer)
                                          :args (cons
                                                 (make-function-expression :parameters (list name)
                                                                           :body (transform 'cps statement-tail))
                                                 (fn-call-args initializer)))))
             (make-return-statement :arg new-call))))
        (t
         (make-var-decl-statement :var-decls
                                  (mapcar (lambda (item) (transform 'cps item))
                                          var-decls)))))))
  
(defmethod transform ((xform (eql 'cps)) (elm if-statement))
  (let ((saved-statement-tail *statement-tail*)
        condition
        then-statement post-then-tail
        else-statement)
    
    ;; TODO There must be a nicer way to do this.  Think about refactoring
    ;; after we've done switch statements.
    (setf condition (transform 'cps (if-statement-condition elm)))

    (setf *statement-tail* saved-statement-tail)
    (setf then-statement (transform 'cps (if-statement-then-statement elm)))
    (setf post-then-tail *statement-tail*)

    (setf *statement-tail* saved-statement-tail)
    (setf else-statement (transform 'cps (if-statement-else-statement elm)))

    (setf *statement-tail* (or post-then-tail *statement-tail*))
    (make-if-statement :condition condition
                       :then-statement then-statement
                       :else-statement else-statement)))

;;TODO Still more to do here
