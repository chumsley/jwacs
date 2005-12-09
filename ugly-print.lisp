;;;; ugly-print.lisp 
;;; 
;;; Provides functions for printing ASTs in a pareseable, but hopefully un-human readable form
;;; The key functionality provided is a source transformation to ensure ALL identifiers are made 
;;; unique
;;;
;;; Note that pretty-print was modified to output without formatting if *pretty-mode*
;;; and *opt-space* are correctly set.
;;; Essentially all we do in this is the uniquifying transformation.
;;;
;;; Unit tests in tests/ugly-print.lisp

(in-package :jwacs) 


; Our main entry point to the ugly printer
;   Please note that we're cheating somewhat-- the source
(defun ugly-print (elm stream)
  "Outputs the AST to a stream with variables and function names converted to
   unique identifiers (ie. JW0) and with all formatting removed."
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
;;; uniquify source transformation
;;;
;;; Converts all variable declarations and function names into indiscernable unique names such as JW0.
;;; 
;;; Subsequently ensures that all identifiers are unique (all scoping will have been sorted out by this point)

(defun uglify-vars (program)
  "Entry point for our source transformation. Turn all var and function declarations and their related 
   references s into ugly ones"
  (let* ((*environment* (add-environment)))
    (transform-in-scope program)))

(defun transform-in-scope (elm)
  "Transforms source elements for the current scope. Given an element, collects all
   var-decls and fun-decls and adds them to the current environment. THEN goes through
   and transforms identifiers + names in said environment. This calls into the main
   uniquify transform methods, and subsequently will recurse through the tree"
  (dolist (var-decl (collect-in-scope elm 'var-decl)) 
    (add-ugly-binding (var-decl-name var-decl)))
  (dolist (fun-decl (collect-in-scope elm 'function-decl))
    (add-ugly-binding (function-decl-name fun-decl)))
  (transform 'uniquify elm))

(defmethod transform ((xform (eql 'uniquify)) (elm identifier))
  (let ((new-name (find-binding (identifier-name elm))))
    ;; If the identifier is defined in this script, use its unique name.
    ;; Otherwise, return it unmodified (to account for system globals like document or XmlHttpRequest)
    (if new-name
      (make-identifier :name new-name)
      elm)))

(defmethod transform ((xform (eql 'uniquify)) (elm function-decl))
  (let* ((*environment* (add-environment))
         (new-params (mapcar #'add-ugly-binding (function-decl-parameters elm))))
    (make-function-decl :name (find-binding (function-decl-name elm))
                        :parameters new-params
                        :body (transform-in-scope (function-decl-body elm)))))

(defmethod transform ((xform (eql 'uniquify)) (elm function-expression))
  (let* ((*environment* (add-environment))
         (new-name (add-ugly-binding (function-expression-name elm)))
         (new-params (mapcar #'add-ugly-binding (function-expression-parameters elm))))
    (make-function-expression :name new-name
                              :parameters new-params
                              :body (transform-in-scope (function-expression-body elm)))))

(defmethod transform ((xform (eql 'uniquify)) (elm var-decl))
  (make-var-decl :name (find-binding (var-decl-name elm))
                 :initializer (transform xform (var-decl-initializer elm))))