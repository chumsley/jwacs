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

(defparameter *environment* '())

(defun add-environment ()
  "Adds a new environment to the environment stack, makes it the current environment"
  (cons '() *environment*))

(defun find-binding (var-name)
  "Looks through the set of environments and finds the most recently bound variable, returns its bound value"
  (labels ((f-b-h (environment)
	     (if (null environment)
		 nil
		 (let ((var-pair (assoc var-name (car environment) :test #'equalp)))
		   (if (null var-pair)
		       (f-b-h (cdr environment))
		       (cdr var-pair))))))
    (f-b-h *environment*)))

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

;; A couple helper methods

(defun uglify-vars (program)
  "Entry point for our source transformation. Turn all var decls and refs into ugly ones"
  (let ((*environment* (add-environment)))
    (transform 'var-unique program)))

(defmacro define-scoped-transform (xform type)
  (let ((new-elm (gensym))
	(slot (gensym)))
  `(defmethod transform ((xform (eql ,xform)) (elm ,type))
    (let ((*environment* (add-environment))
	  (,new-elm (funcall (get-constructor elm))))
      (dolist (,slot (structure-slots elm))
	(setf (slot-value ,new-elm ,slot)
	      (transform ,xform (slot-value elm ,slot))))
      ,new-elm))))

;;
;; The actual methods that apply on the appropriate elements
;; 
;; statement-blocks, function-decls, and function-expressions wrap new environments
;; var-decls create new bindings
;; identifiers look up bindings

(defmethod transform ((xform (eql 'var-unique)) (elm var-decl))
  "Vardecls add new ugly binding to the environment"
  (let ((new-name (add-ugly-binding (var-decl-name elm))))
    (make-var-decl :name new-name 
		   :initializer (transform xform (var-decl-initializer elm)))))


; TODO: this currently does not throw an error when NO binding is found (ie. nil)
(defmethod transform ((xform (eql 'var-unique)) (elm identifier))
  (let ((new-name (find-binding (identifier-name elm))))
    (make-identifier :name new-name)))

(define-scoped-transform 'var-unique statement-block)
(define-scoped-transform 'var-unique function-decl)
(define-scoped-transform 'var-unique function-expression)


;;;
;;;
;;; Now that our vars are uglified, we can now just go ahead and print everything in as compressed and 
;;; ugly a format as possible.
;;;

;; Do we even want to include newlines?? How do browsers deal with superlong strings?

(defun ugly-print (elm stream)
  (let ((*pretty-mode* nil)
	(*opt-space* "")
	(new-elm (uglify-vars elm)))
    (pretty-print new-elm stream)))


(defun ugly-string (elm)
  "Pretty-print ELM to a string value instead of a stream."
  (with-output-to-string (s)
    (ugly-print elm s)))
