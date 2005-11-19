;;;; ugly-print.lisp 
;;; 
;;; Provides functions for printing ASTs in a pareseable, but hopefully un-human readable form
;;; The key functionality provided is a source transformation to ensure ALL identifiers are made unique
;;; Unit tests in tests/ugly-print.lisp

(in-package :jwacs)

;;;
;;; Simple environment ADT (ribcage)
;;;
;;; the environment is a list of association lists 
;;; alists contain pairs of (oldname . newname), in this case they will be strings
;;;
;;; Everytime we enter a new lexical environment, we add a new alist to the front.
;;; When looking for a binding, we start at the front (innermost) and look through until we get to the back (global)
;;;
;;; Currently this transformation does not support free variables.

(defparameter *environment* '(nil))

(defun add-environment ()
  "Adds a new environment to the environment stack, makes it the current environment"
  (setf *environment* (cons '() *environment*)))

(defun remove-environment ()
  "Removes the current environment from the stack. All bindings will be lost for this environment"
  (setf *environment* (cdr *environment*)))

(defun find-binding (var-name)
  "Looks through the set of environments and finds the most recently bound variable, returns its bound value"
  (find-binding-h var-name *environment*))

(defun find-binding-h (var-name environment)
  "Helper method for find-binding"
  (if (null environment)
      nil
      (let ((var-pair (assoc var-name (car environment) :test #'equalp)))
	(if (null var-pair)
	    (find-binding-h var-name (cdr environment))
	    (cdr var-pair)))))

(defun add-binding (var-name var-newname)
  "Add a binding to the environment. In our case, name and new name"
  (setf (car *environment*) (cons (cons var-name var-newname) (car *environment*))))

(defun add-ugly-binding (var-name)
  "Takes a variable name and creates a new ugly name and adds that to the environment. Returns the ugly name"
  (let ((ugly-name (genvar)))
    (add-binding var-name ugly-name)
    ugly-name))


(defparameter *genvar-counter* 0)

(defun genvar ()
  "Generates a unique string that will be our ugly name for variables"
  (let ((old *genvar-counter*))
    (setq *genvar-counter* (1+ old))
    (concatenate 'string "JW$" (format nil "~a" old))))


;;;
;;; Now the actual source transformation code
;;;
;;;

(defun uglify-vars (program)
  "Entry point for our source transformation. Turn all var decls and refs into ugly ones"
  (setq *environment* '(nil))
  (transform 'var-unique program))

(defmethod transform ((xform (eql 'var-unique)) (elm var-decl))
  "Vardecls add new ugly binding to the environment"
  (let ((new-name (add-ugly-binding (var-decl-name elm))))
    (setf (var-decl-name elm) new-name)
    elm))



;;; Insert more here! :)
