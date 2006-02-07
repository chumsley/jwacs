;;;; general-utilities.lisp
;;;
;;; Handy utility functions that don't really belong anywhere else
;;; (these aren't really jwacs-specific in any way)
(in-package :jwacs)

(defmacro aif (test-form then-form &optional else-form)
  "Anaphoric IF expression; binds IT."
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  "Anaphoric WHEN expression; binds IT."
  `(aif ,test-form
	(progn ,@body)))

(defmacro when-let ((bind-var test-form) &body body)
  "Anaphoric WHEN expression that allows the caller to specify the name of the bound variable"
  `(let ((,bind-var ,test-form))
    (when ,bind-var
      ,@body)))

(defun postpend (list-arg atom-arg)
  "Appends a list containing ATOM-ARG to LIST-ARG.
   eg: (POSTPEND '(1 2) 3) ===> '(1 2 3)"
  (append list-arg (list atom-arg)))
