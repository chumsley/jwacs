;;;; ugly-print.lisp 
;;; 
;;; Provides functions for printing ASTs in a pareseable, but hopefully un-human readable form
;;; The key functionality provided is a source transformation to ensure ALL identifiers are made 
;;; unique
;;;
;;; Note that we pretty-print was modified to output without formatting if *pretty-mode*
;;; and *opt-space* are correctly set.
;;; Essentially all we do in this is the uniquifying transformation.
;;;
;;; Unit tests in tests/ugly-print.lisp

(in-package :jwacs) 


; Our main entry point to the ugly printer
;   Please note that we're cheating somewhat-- the source
(defun ugly-print (elm stream)
  "Outputs the AST to a stream with variables and function names converted to
   unique identifiers (ie. JW$0) and with all formatting removed."
  (let ((*pretty-mode* nil)
	(*opt-space* "")
	(new-elm (uglify-vars elm)))
    (pretty-print new-elm stream)))

;;; ==================================================
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
    (concatenate 'string "JW" (format nil "~a" old))))

;;; ==================================================
;;;
;;; Now the actual source transformation code
;;;
;;;

(defun uglify-vars (program)
  "Entry point for our source transformation. Turn all var decls and refs into ugly ones"
  (let ((*environment* (add-environment)))    
    (transform 'var-unique 
	       (transform 'fun-unique program))))

;;; ==================================================
;;
;; var-unique transformation generic functions
;; 
;; statement-blocks, function-decls, and function-expressions wrap new environments
;; var-decls create new bindings
;; identifiers look up bindings

(defmethod transform ((xform (eql 'var-unique)) (elm var-decl))
  "Vardecls add new ugly binding to the environment"
  (let ((new-name (add-ugly-binding (var-decl-name elm))))
    (make-var-decl :name new-name 
		   :initializer (transform xform (var-decl-initializer elm)))))


; TODO: this currently does not throw an error when a binding is missing (ie. nil)
(defmethod transform ((xform (eql 'var-unique)) (elm identifier))
  (let ((new-name (find-binding (identifier-name elm))))
    (make-identifier :name new-name)))

(defmethod transform ((xform (eql 'var-unique)) (elm function-decl))
  (let* ((*environment* (add-environment))
	 (new-params (mapcar #'add-ugly-binding (function-decl-parameters elm))))
    (make-function-decl :name (function-decl-name elm)
			:parameters new-params
			:body (transform xform (function-decl-body elm)))))

(defmethod transform ((xform (eql 'var-unique)) (elm function-expression))
  (let* ((*environment* (add-environment))
	 (new-params (mapcar #'add-ugly-binding (function-expression-parameters elm))))
    (make-function-expression :name (function-expression-name elm)
			      :parameters new-params
			      :body (transform xform (function-expression-body elm)))))
	 

(defmethod transform ((xform (eql 'var-unique)) (elm statement-block))
  (let* ((*environment* (add-environment))
	 (new-statements (transform 'fun-unique
				    (statement-block-statements elm))))
    (make-statement-block :statements (transform xform new-statements))))


;;; ==================================================
;;;
;;; fun-unique generic functions
;;;
;;;
;;;
;;;

(defmethod transform ((xform (eql 'fun-unique)) (elm function-decl))
  (let ((new-name (add-ugly-binding (function-decl-name elm))))
    (make-function-decl :name new-name
			:parameters (function-decl-parameters elm)
			:body (function-decl-body elm))))

(defmethod transform ((xform (eql 'fun-unique)) (elm function-expression))
  (if (null (function-expression-name elm))
      elm
      (let ((new-name (add-ugly-binding (function-expression-name elm))))
	(make-function-expression :name new-name
				  :parameters (function-expression-parameters elm)
				  :body (function-expression-body elm)))))

; in fun-unique mode, do not recurse down statement-blocks 
(defmethod transform ((xform (eql 'fun-unique)) (elm statement-block))
  elm)
