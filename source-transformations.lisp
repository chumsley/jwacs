;;;; source-transformations.lisp
;;;
;;; Implements source transformations.  The interface is through the TRANSFORM generic function.

(in-package :jwacs)

;;;;= Utility functions =
;;TODO Structure-slots and friends may want to go into a separate "structure-helpers" file
(defun structure-slots (object)
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

(defparameter *copy-functions* (make-hash-table :test 'eq)
  "Map from structure-type symbol to copy-function symbol")

(defun get-copier (struct-object)
  "Accept a structure object and return the (likely) name of its copy-function.
   CAVEAT: Assumes that the structure was defined in the current package."
  (let* ((name (type-of struct-object))
         (copy-fn (gethash name *copy-functions*)))
    (if (null copy-fn)
      (setf (gethash name *copy-functions*) (intern (format nil "COPY-~A" name)))
      copy-fn)))

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
  (let ((fresh-elm (funcall (get-copier elm) elm)))
    (dolist (slot (structure-slots elm))
      (setf (slot-value fresh-elm slot)
            (transform xform (slot-value elm slot))))
    fresh-elm))        

(defmethod transform (xform (elm list))
  (mapcar (lambda (arg)
            (transform xform arg))
          elm))

;;;;= CPS transformation =
;;; Initial, naive version does the following:
;;; - All function calls transformed to cps-fn-calls
;;; - All assignments transformed to new continuations
;;; - All returns transformed to cps-returns
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

(defvar *statement-tail* nil
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
  (make-cps-return :arg
                   (without-statement-tail
                     (make-fn-call :fn *cont-id*
                                   :args (list
                                          (transform 'cps (return-statement-arg elm)))))))

(defmethod transform ((xform (eql 'cps)) (elm fn-call))
  (if (null *statement-tail*)
    (make-cps-fn-call :fn (fn-call-fn elm)
                      :args (cons *cont-id*
                                  (mapcar (lambda (item)
                                              (transform 'cps item))
                                          (fn-call-args elm))))
    (make-cps-fn-call :fn (fn-call-fn elm)
                      :args (consume-statement-tail (statement-tail)
                              (cons (make-function-expression :parameters (list (genvar))
                                                              :body (transform 'cps statement-tail))
                                    (mapcar (lambda (item)
                                              (transform 'cps item))
                                            (fn-call-args elm)))))))

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
             (if (return-statement-p (car (last statement-tail)))
               (make-return-statement :arg new-call)
               new-call))))
        (t
         (make-var-decl-statement :var-decls
                                  (mapcar (lambda (item) (transform 'cps item))
                                          var-decls)))))))
  
(defmethod transform ((xform (eql 'cps)) (elm if-statement))
  (let ((saved-statement-tail *statement-tail*)
        condition post-condition-tail
        then-statement post-then-tail
        else-statement)
    
    ;; TODO There must be a nicer way to do this.  Think about refactoring
    ;; after we've done switch statements.
    (setf condition (transform 'cps (if-statement-condition elm)))
    (setf post-condition-tail *statement-tail*)

    (setf *statement-tail* saved-statement-tail)
    (setf then-statement (transform 'cps (if-statement-then-statement elm)))
    (setf post-then-tail *statement-tail*)

    (setf *statement-tail* saved-statement-tail)
    (setf else-statement (transform 'cps (if-statement-else-statement elm)))

    (setf *statement-tail* (and post-condition-tail post-then-tail *statement-tail*))
    (make-if-statement :condition condition
                       :then-statement then-statement
                       :else-statement else-statement)))
       
        

;;TODO Still more to do here
      
;;;;= Unit tests =
(defmethod transform ((xform (eql 'hello)) (elm string))
    "hello there!")

(defun test-transform ()
  (and
   (equal (continue-statement-label (transform 'hello (make-continue-statement :label "go away!")))
          "hello there!")
   (equal (transform 'hello '("string 1" symbol ("string 2")))
                     '("hello there!" symbol ("hello there!")))))