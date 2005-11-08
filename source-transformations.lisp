;; source-transformations
(in-package :jwacs)

;;;;; Utility functions
;;TODO Figure out an entry for SBCL
(defun structure-slots (object)
  #+openmcl
  (let* ((sd (gethash (class-name (class-of object)) ccl::%defstructs%))
	 (slots (if sd (ccl::sd-slots sd))))
    (mapcar #'car (if (symbolp (caar slots)) slots (cdr slots))))
  #+cmu
  (mapcar #'pcl:slot-definition-name (pcl:class-slots (class-of object)))
  #+lispworks
  (structure:structure-class-slot-names (class-of object))
  #+allegro
  (mapcar #'mop:slot-definition-name (mop:class-slots (class-of object))))

(defparameter *copy-functions* (make-hash-table :test 'eq)
  "Map from structure-type symbol to copy-function symbol")

(defun get-copier (struct-object)
  "Accept a structure object and return the (likely) name of its copy-function"
  (let* ((name (type-of struct-object))
         (copy-fn (gethash name *copy-functions*)))
    (if (null copy-fn)
      (setf (gethash name *copy-functions*) (intern (format nil "COPY-~A" name)))
      copy-fn)))

;;;;; Default transformation behaviour

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
(defmethod transform (xform (elm structure-object))
  (let ((fresh-elm (funcall (get-copier elm) elm)))
    (dolist (slot (structure-slots elm))
      (setf (slot-value fresh-elm slot)
            (transform xform (slot-value elm slot))))
    fresh-elm))        

(defmethod transform (xform (elm list))
  (mapcar (lambda (arg)
            (transform xform arg))
          elm))

;;;;; The COLLAPSE-ADJACENT-VAR-DECLS transformation
;;; As the name suggests, this transformation collapses adjacent variable declaration statements
;;; into a single statment.

;;HERE

;;;;; Unit tests
(defmethod transform ((xform (eql 'hello)) (elm string))
    "hello there!")

(defun test-transform ()
  (and
   (equal (continue-label (transform 'hello (make-continue :label "go away!")))
          "hello there!")
   (equal (transform 'hello '("string 1" symbol ("string 2")))
                     '("hello there!" symbol ("hello there!")))))