;;;; explicitize-transformation.lisp
;;;
;;; Defines the explicitize source transformation and supporting
;;; functionality.
(in-package :jwacs)

;;;;= Explicitize transformation =
;;; The explicitize transformation gives each intermediate value a
;;; name.  After the transformation is done, the result of every
;;; function call will be assigned to a variable.
;;;
;;; As a convenience, variable statements that contain declarations for
;;; multiple variables will also be converted to series of single
;;; variable declarations (eg `var x, y=10;` ---> `var x; var y = 10;`)

;; These elements should have been removed by LOOP-TO-FUNCTION
(forbid-transformation-elements explicitize (while do-statement for for-in))

;;TODO It is difficult to express in a single sentence exactly what EXPOSE-INTERMEDIATE
;; and EXPLICITIZE-RHS actually do.  These functions need to be refactored.
(defun expose-intermediate (elm)
  "Return a cons cell whose CAR is the name of the result of this element,
   and whose CDR is a list of statements that need to be added before the element.
   ELM should be the output of an explicitization transformation.  This function
   exists purely to make it easier to deal with the pre-statements issue."
  (cond
    ((fn-call-p elm)
     (let ((new-var (genvar)))
       (list (make-identifier :name new-var)
             (make-var-decl-statement
              :var-decls (list (make-var-decl :name new-var :initializer elm))))))
    ((listp elm)
     (let ((new-var (genvar))
           (final-stmt (car (last elm))))

       ;; The last statement in a statement list is a version of the original that contains no nested
       ;; intermediate expressions.  If it is a function call, then we convert it to a var decl and
       ;; return the var name as the expression to reference.  Otherwise, we remove it and return it
       ;; as the expression to reference.  This helps to avoid redundant code like
       ;;       `var r3 = r1 + r2;
       ;;        foo(r3);`
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
    
    (let ((final-pre-statement (car (last pre-statements))))
      (cond
        ;; This case "compresses" out extraneous variables.
        ;; (ie, "var JW0 = <something>; var JW1 = JW0;" is a pointless sequence)
        ;; The condition is:
        ;; There are pre-statements, and we have an assignment-slot, and the last
        ;; pre-statement is a variable declaration, and the assignment-slot is just
        ;; a reference to that final variable declaration.
        ((and pre-statements
              assignment-slot
              (var-decl-statement-p final-pre-statement)
              (identifier-p (slot-value new-elm assignment-slot))
              (equal (identifier-name (slot-value new-elm assignment-slot))
                     (var-decl-name (car (var-decl-statement-var-decls final-pre-statement)))))
         (setf (slot-value new-elm assignment-slot)
               (var-decl-initializer (car (var-decl-statement-var-decls final-pre-statement))))
         (nconc (remove final-pre-statement pre-statements :from-end t :count 1)
                (list new-elm)))

        (pre-statements
         (nconc pre-statements (list new-elm)))

        (t
         new-elm)))))

(defmethod transform ((xform (eql 'explicitize)) (elm binary-operator))
  (explicitize-rhs elm))

(defmethod transform ((xform (eql 'explicitize)) (elm unary-operator))
  (explicitize-rhs elm))

(defmethod transform ((xform (eql 'explicitize)) (elm var-decl))
  (explicitize-rhs elm 'initializer))

(defmethod transform ((xform (eql 'explicitize)) (elm return-statement))
  (explicitize-rhs elm 'arg))

(defmethod transform ((xform (eql 'explicitize)) (elm if-statement))
  (flet ((maybe-block (intermediate)
           (cond
             ((and (cdr intermediate)
                   (statement-block-p (car intermediate)))
              (make-statement-block :statements (nconc (cdr intermediate)
                                                       (statement-block-statements (car intermediate)))))
             ((cdr intermediate)
              (make-statement-block :statements (nconc (cdr intermediate)
                                                       (list (car intermediate)))))
             (t (car intermediate)))))
    (let* ((cond-intermediate (expose-intermediate (transform 'explicitize (if-statement-condition elm))))
           (then-intermediate (expose-intermediate (transform 'explicitize (if-statement-then-statement elm))))
           (else-intermediate (expose-intermediate (transform 'explicitize (if-statement-else-statement elm))))
           (new-elm (make-if-statement :condition (car cond-intermediate)
                                       :then-statement (maybe-block then-intermediate)
                                       :else-statement (maybe-block else-intermediate))))
      (if (cdr cond-intermediate)
        (nconc (cdr cond-intermediate) (list new-elm))
        new-elm))))
                  
(defmethod transform ((xform (eql 'explicitize)) (elm switch))
  (let ((val-intermediate (expose-intermediate (transform 'explicitize (switch-value elm))))
        (new-clauses (mapcar (lambda (clause) (transform 'explicitize clause))
                             (switch-clauses elm))))
    (if (cdr val-intermediate)
      (nconc (cdr val-intermediate)
             (list (make-switch :value (car val-intermediate)
                                :clauses new-clauses)))
      (make-switch :value (car val-intermediate)
                   :clauses new-clauses))))

;;TODO conditional expressions, comma expressions

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
