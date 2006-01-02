;;;; shift-function-decls-transformation.lisp
;;;
;;; Define the shift-function-decls transformation.
(in-package :jwacs)

;;;; shift-function-decls transformation 
;;;
;;; This transformation moves function declarations to the beginning
;;; of each scope in the provided AST.  Function decls are never moved
;;; to a different scope, and they will always appear in the same order
;;; as originally, so this transfomation is semantically neutral.
;;;
;;; NOTE: The Javascript definition of "scope" is slightly different
;;; than you might expect.  In particular, only function-decls and
;;; function-expressions create new scopes; statement blocks do not.
;;;
;;; Note also that function-expressions will /not/ be moved, since
;;; they are values (and also their identifiers do not affect their
;;; enclosing scope; see Pg 71 of ECMA-262)

(defmethod transform ((xform (eql 'shift-function-decls)) (elm-list list))
  (let ((fn-decls (collect-in-scope elm-list 'function-decl))
        (other-statements (remove-if #'function-decl-p elm-list)))
    (nconc
     (mapcar (lambda (x)
               (transform 'shift-function-decls x))
             fn-decls)
     (mapcar (lambda (x)
               (transform 'shift-function-decls x))
             other-statements))))

(defmethod transform ((xform (eql 'shift-function-decls)) (elm source-element))
  (let ((fresh-elm (funcall (get-constructor elm))))
    (dolist (slot (structure-slots elm))
      (if (function-decl-p (slot-value elm slot))
        (setf (slot-value fresh-elm slot)
              nil)
        (setf (slot-value fresh-elm slot)
              (transform 'shift-function-decls (slot-value elm slot)))))
    fresh-elm))

(defmethod transform ((xform (eql 'shift-function-decls)) (elm object-literal))
  (make-object-literal
   :properties
   (loop for (prop-name . prop-value) in (object-literal-properties elm)
         collect (cons
                  (transform 'shift-function-decls prop-name)
                  (transform 'shift-function-decls prop-value)))))
