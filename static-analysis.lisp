;;;; static-analysis.lisp
;;;
;;; Defines some utilities for statically analyzing the control flow of
;;; jwacs source elements.
;;;
;;; Copyright (c) 2006 James Wright
;;; See LICENSE for full licensing details.
;;;
(in-package :jwacs)

;;;; ======= Simple predicates =====================================================================

(defun effective-fn-call-p (elm)
  "Both function calls and new expressions are 'effective' function calls, because they
   will both wind up as function calls after the compiler pipeline gets through with them."
  (or (fn-call-p elm)
      (new-expr-p elm)))

;;;; ======= explicitly-terminated-p generic function ==============================================

(defun explicit-return-p (elm)
  "Convenience function for the common case where we're concerned about function
   termination (rather than loop or clause termination)"
  (explicitly-terminated-p elm '(:return :throw :resume :suspend)))

(defgeneric explicitly-terminated-p (elm terminators)
  (:documentation
   "Returns non-NIL if ELM explicitly terminates via all control paths.  The definition
    of 'termination' is configurable by the TERMINATORS argument.  TERMINATORS is a list
    containing any or all of :RETURN :THROW :BREAK :CONTINUE :RESUME :SUSPEND.  When :RETURN
    is in TERMINATORS, return statements are considered to terminate a control path; similarly
    for the other keywords."))

;;; Non-escaping label tracking
;;;
;;; We track 'non-escaping' breaks and continues that don't terminate the entire statement
;;; being considered.  For example, this call:
;;;
;;;    (EXPLICITLY-TERMINATED-P (PARSE "x = 10; break;"))
;;;
;;; should return non-NIL, but this one:
;;;
;;;    (EXPLICITLY-TERMINATED-P (PARSE "x = 10; while(true) { break; } x = 20;"))
;;;
;;; should return NIL, because the `break` statement only terminates the `while` loop,
;;; not the entire control path being considered.

(defparameter *non-escaping-break-labels* nil
  "Non-escaping label names that are valid targets for a `break` statement")

(defparameter *non-escaping-continue-labels* nil
  "Non-escaping label names that are valid targets for a `continue` statement")

(defmacro with-non-escaping-break-target ((elm) &body body)
  `(let ((*non-escaping-break-labels* (cons (source-element-label ,elm)
                                            *non-escaping-break-labels*)))
    ,@body))

(defmacro with-non-escaping-continue-target ((elm) &body body)
  `(let ((*non-escaping-continue-labels* (cons (source-element-label ,elm)
                                            *non-escaping-continue-labels*)))
    ,@body))

;;; Unless otherwise specified, a source element does not explicitly terminate
(defmethod explicitly-terminated-p (elm terminators)
  nil)

;;; Base cases
(defmethod explicitly-terminated-p ((elm return-statement) terminators)
  (find :return terminators))

(defmethod explicitly-terminated-p ((elm throw-statement) terminators)
  (find :throw terminators))

(defmethod explicitly-terminated-p ((elm resume-statement) terminators)
  (find :resume terminators))

(defmethod explicitly-terminated-p ((elm suspend-statement) terminators)
  (find :suspend terminators))

(defmethod explicitly-terminated-p ((elm break-statement) terminators)
  (with-slots (target-label) elm
    (unless (or (and (null target-label)
                     (> (length *non-escaping-break-labels*) 0))
                (member target-label *non-escaping-break-labels*
                        :test 'equal))
      (find :break terminators))))
  
(defmethod explicitly-terminated-p ((elm continue-statement) terminators)
  (with-slots (target-label) elm
    (unless (or (and (null target-label)
                     (> (length *non-escaping-continue-labels*) 0))
                (member target-label *non-escaping-continue-labels*
                        :test 'equal))
      (find :continue terminators))))

;;; Sequences
(defmethod explicitly-terminated-p ((elm-list list) terminators)
  (unless (null elm-list)
    (or (explicitly-terminated-p (car elm-list) terminators)
        (explicitly-terminated-p (cdr elm-list) terminators))))

;;; Branches
(defmethod explicitly-terminated-p ((elm if-statement) terminators)
  (and (explicitly-terminated-p (if-statement-then-statement elm) terminators)
       (explicitly-terminated-p (if-statement-else-statement elm) terminators)))

(defmethod explicitly-terminated-p ((elm switch) terminators)
  (with-non-escaping-break-target (elm)
    (reduce (lambda (x y)
              (and x y))
            (switch-clauses elm)
            :key (lambda (clause)
                   (explicitly-terminated-p clause terminators)))))

(defmethod explicitly-terminated-p ((elm try) terminators)
  (with-slots (body catch-clause finally-clause) elm
    (or (explicitly-terminated-p finally-clause terminators)
        (if (null catch-clause)
          (explicitly-terminated-p body terminators)
          (and (explicitly-terminated-p body terminators)
               (explicitly-terminated-p catch-clause terminators))))))
      
;;; Simple recursion
(defmethod explicitly-terminated-p ((elm statement-block) terminators)
  (explicitly-terminated-p (statement-block-statements elm) terminators))

(defmethod explicitly-terminated-p ((elm case-clause) terminators)
  (explicitly-terminated-p (case-clause-body elm) terminators))

(defmethod explicitly-terminated-p ((elm default-clause) terminators)
  (explicitly-terminated-p (default-clause-body elm) terminators))

(defmethod explicitly-terminated-p ((elm do-statement) terminators)
  ;; If the body of a do loop is explicitly terminated, then so is the whole
  ;; statement, because the body always executes at least once.
  (with-non-escaping-break-target (elm)
    (with-non-escaping-continue-target (elm)
      (explicitly-terminated-p (do-statement-body elm) terminators))))

(defmethod explicitly-terminated-p ((elm while) terminators)
  ;; The only time that a while loop's body is statically guaranteed to execute is
  ;; when its condition is true
  (when (and (special-value-p (while-condition elm))
             (eq (special-value-symbol (while-condition elm))
                 :true))
    (with-non-escaping-break-target (elm)
      (with-non-escaping-continue-target (elm)
        (explicitly-terminated-p (while-body elm) terminators)))))

(defmethod explicitly-terminated-p ((elm for) terminators)
  ;; The only time that a for loop's body is statically guaranteed to execute is
  ;; when its condition is true
  (when (and (special-value-p (for-condition elm))
             (eq (special-value-symbol (for-condition elm))
                 :true))
    (with-non-escaping-break-target (elm)
      (with-non-escaping-continue-target (elm)
        (explicitly-terminated-p (for-body elm) terminators)))))

(defmethod explicitly-terminated-p ((elm for-in) terminators)
  ;; The only time that a for-in loop's body is statically guaranteed to execute is
  ;; when its collection is a non-empty literal
  (when (or (and (object-literal-p (for-in-collection elm))
                 (> (length (object-literal-properties (for-in-collection elm))) 0))
            (and (array-literal-p (for-in-collection elm))
                 (> (length (array-literal-elements (for-in-collection elm))) 0)))
    (with-non-escaping-break-target (elm)
      (with-non-escaping-continue-target (elm)
        (explicitly-terminated-p (for-in-body elm) terminators)))))
  
(defmethod explicitly-terminated-p ((elm with) terminators)
  (explicitly-terminated-p (with-body elm) terminators))

(defmethod explicitly-terminated-p ((elm catch-clause) terminators)
  (explicitly-terminated-p (catch-clause-body elm) terminators))

(defmethod explicitly-terminated-p ((elm finally-clause) terminators)
  (explicitly-terminated-p (finally-clause-body elm) terminators))

;;;; ======= introduces-fn-call-p generic function =================================================

(defgeneric introduces-fn-call-p (elm)
  (:documentation
   "Returns non-NIL if there exists a control path through ELM that contains
    an effective function call."))

;;; Base cases

(defmethod introduces-fn-call-p (elm)
  nil)

(defmethod introduces-fn-call-p ((elm fn-call))
  t)

(defmethod introduces-fn-call-p ((elm new-expr))
  t)

;;; Recursion

(defmethod introduces-fn-call-p ((elm-list list))
  (some #'introduces-fn-call-p elm-list))

(defmethod introduces-fn-call-p ((elm source-element))
  (some (lambda (slot)
          (introduces-fn-call-p (slot-value elm slot)))
        (structure-slots elm)))

(defmethod introduces-fn-call-p ((elm object-literal))
  (some #'introduces-fn-call-p
        (mapcar #'cdr (object-literal-properties elm))))

;;; Don't recurse into functions

(defmethod introduces-fn-call-p ((elm function-decl))
  nil)

(defmethod introduces-fn-call-p ((elm function-expression))
  nil)

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
