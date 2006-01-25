;;;; source-transformations.lisp
;;;
;;; Define the base functionality for source transformations.
;;; The TRANSFORM generic function is defined, as well as some
;;; transformation-related utility functions.  The main source
;;; transformations will implement methods on TRANSFORM in their
;;; own source files.
(in-package :jwacs)

;;;; Utilities 
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

(defmacro forbid-transformation-elements (xform elm-type-list)
  "Generate DEFMETHOD forms that throw an error if the transformation
   specified in XFORM is applied to any of the element types in
   ELM-TYPE-LIST"
  `(progn
    ,@(loop for elm-type in elm-type-list
            collect `(defmethod transform ((xform (eql ',xform)) (elm ,elm-type))
                      (error "~A source-element encountered during ~A transformation!" ',elm-type ',xform)))))
                   

;;;; Collection within a single scope  
(defgeneric collect-in-scope (elm target-type)
  (:documentation
   "Finds and returns a list of all elements of TARGET-TYPE in the same scope as
    ELM.  Does not recurse into function-decl or function-expression elements.
    So for example, searching for function-decls in this code:

       var x = 10;
       function foo(x) { function bar(y) { return 10; } return bar(x); }

    FOO would be returned but BAR would not, since the decl of BAR is in
    an innermore scope (namely, FOO's body)."))

;;;; Rules about recursing into children 
(defmethod collect-in-scope (elm target-type)
  nil)

(defmethod collect-in-scope ((elm-list list) target-type)
  (loop for elm in elm-list
        append (collect-in-scope elm target-type)))

(defmethod collect-in-scope ((elm source-element) target-type)
  (loop for slot in (structure-slots elm)
        append (collect-in-scope (slot-value elm slot) target-type)))

;; Don't recurse, because the body is a new, innermore scope.
(defmethod collect-in-scope ((elm function-decl) target-type)
  nil)

;; Don't recurse, because the body is a new, innermore scope.
(defmethod collect-in-scope ((elm function-expression) target-type)
  nil)

;; Don't recurse, because the body is a new, innermore scope.
(defmethod collect-in-scope ((elm object-literal) target-type)
  nil)

;;;; Rule for returning matches. 
;; We don't recurse into matching elements
(defmethod collect-in-scope :around (elm target-type)
  (if (typep elm target-type)
    (list elm)
    (call-next-method)))

;;;; Default transformation behaviour 

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

;; Special case for object-literals to account for the fact that object-literal-properties
;; is an alist rather than a list of structures.
(defmethod transform (xform (elm object-literal))
  (make-object-literal
   :properties
   (loop for (prop-name . prop-value) in (object-literal-properties elm)
         collect (cons
                  (transform xform prop-name)
                  (transform xform prop-value)))))
