;;;; general-utilities.lisp
;;;
;;; Handy utility functions that don't really belong anywhere else
;;; (these aren't really jwacs-specific in any way)
(in-package :jwacs)

;;;; ======= Anaphoric conditionals ================================================================
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

;;;; ======= List handling =========================================================================
(defun postpend (list-arg atom-arg)
  "Appends a list containing ATOM-ARG to LIST-ARG.
   eg: (POSTPEND '(1 2) 3) ===> '(1 2 3)"
  (append list-arg (list atom-arg)))

;;;; ======= File handling =========================================================================
(defun pathnames-equal (path1 path2)
  "Return non-NIL if PATH1 and PATH2 are equivalent.  This function avoids some of the
   complexity that pathnames can entail by comparing the namestrings of the two paths
   rather than the paths themselves.  That way we don't have to worry about spurious
   distinctions between :UNSPECIFIED and NIL, :NEWEST and NIL and some actual version number, etc."
  (equal (namestring (pathname path1))
         (namestring (pathname path2))))

(defun read-entire-file (path)
  "Reads the entire contents of the file located at PATH and returns it as a string"
  (with-open-file (in path :direction :input)
    (with-output-to-string (out)
      (loop for line = (read-line in nil :eof)
            until (eq line :eof)
            do (format out "~A~%" line)))))

(defun read-asdf-component-text (component-path)
  "Returns the contents of a file that is a component of a currently-loaded asdf system.
   COMPONENT-PATH is a path describing the location of the component to read.  It should
   have at least 2 elements.
   The first element is a symbol naming a system.
   The last element is a string naming a component.
   There may be intermediate strings naming intermediate modules.  Eg:
   
       (:JWACS-TESTS \"tests\" \"test-cps-transformation\")

   names the test-cps-transformation component, which is part of the tests module, which
   is part of the :JWACS-TESTS system."
  (let ((component (asdf:find-system (car component-path))))
    (dolist (name (cdr component-path))
      (setf component (asdf:find-component component name)))
    (read-entire-file (asdf:component-pathname component))))