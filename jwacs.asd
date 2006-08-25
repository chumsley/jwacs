;;;; jwacs.asd
;;;
;;; This is the system definition file for the jwacs project.
;;; It defines the asdf system plus any extra asdf operations
;;; (eg test-op).

(defpackage :jwacs-system
  (:use :cl :asdf)
  (:nicknames :jw-system)
  (:export
   #:*version*
   #:*executable-name*))
(in-package :jwacs-system)

;;;; ======= Build parameters ======================================================================

(defparameter *version* "alpha1"
  "The current version")

(defparameter *executable-name*
  #+win32 "jwacs.exe"
  #-win32 "jwacs"
  "The name of the executable to create when dumping a binary")

;;;; ======= Compilation configuration =============================================================
(defparameter *use-yacc* t              ; TODO When we implement semicolon insertion for the Lispworks parser, go back to using parsergen when available
;  #+lispworks nil
;  #-lispworks t
  "When t, use cl-yacc to generate a parser.  Otherwise, use the Lispworks parsergen.")

(if *use-yacc*
  (pushnew :use-yacc *features*)
  (setf *features* (remove :use-yacc *features*)))

#-use-yacc
(require "parsergen")

#+use-yacc
(defparameter *muffle-conflicts* t
  "When T, yacc warnings about Shift/Reduce and Reduce/Reduce conflicts will be muffled.
   When NIL, all such conflicts will be reported.
   When non-NIL, non-T, a single summary warning will be reported when conflicts exist.

   This value should be set to NIL or non-T during grammar
   development/debugging (so that we find out about the conflicts), but T
   at all other times (so that SBCL won't drop into the debugger when
   we're trying to load parse-javascript.lisp).")

;;;; ======= Custom ASDF file types ================================================================
(defclass js-file (static-file) ())
(defmethod source-file-type ((c js-file) (s module)) "js")
(defmethod operation-done-p ((o load-op) (c js-file))  
  t)
(defmethod operation-done-p ((o compile-op) (c js-file))
  t)

(defmethod operation-done-p ((o load-op) (c html-file))  
  t)
(defmethod operation-done-p ((o compile-op) (c html-file))
  t)

;;;; ======= System definition =====================================================================
(asdf:defsystem jwacs 
  :version *version*
  :author "James Wright <james@chumsley.org> et al"
  :licence "MIT License <http://www.opensource.org/licenses/mit-license.php>"
  :serial t
  :components ((:module "external"
                        :components
                        ((:file "yacc")))
               ;;TODO Should these three non-Lisp files go into a separate module?
               (:js-file "jw-runtime")
               (:html-file "default-template")
               (:html-file "default-iframe")
               (:file "package")
               (:file "general-utilities")
               (:file "lexer-macros")
               (:file "lex-javascript")
               (:file "js-source-model")
               #+use-yacc (:file "parse-javascript-yacc")
               (:file "parse-javascript")
               (:file "pretty-print")
               (:file "source-transformations")
               (:file "shift-decls-transformation")
               (:file "ugly-print")
               (:file "static-analysis")
               (:file "type-analysis")
               (:file "explicitize-transformation")
               (:file "shadow-values-transformation")
               (:file "cps-transformation")
               (:file "loop-transformation")
               (:file "trampoline-transformation")
               (:file "runtime-transformation")
               (:file "compiler")
               #+(or sbcl lispworks) (:file "main"))
  :depends-on (cl-ppcre))

;;;; ======= Test operation ========================================================================
(defmethod perform ((o test-op) (c (eql (find-system 'jwacs))))
  (operate 'load-op 'jwacs-tests)
  (operate 'test-op 'jwacs-tests))

(defmethod operation-done-p ((o test-op) (c (eql (find-system 'jwacs))))
  nil)