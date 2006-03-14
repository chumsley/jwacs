;;;; jwacs.asd
;;;
;;; This is the system definition file for the jwacs project.
;;; It defines the asdf system plus any extra asdf operations
;;; (eg test-op).

(defpackage :jwacs-system
  (:use :cl :asdf)
  (:nicknames :jw-system))
(in-package :jwacs-system)

;;;; Compilation configuration
(defparameter *use-yacc*
  #+lispworks nil
  #-lispworks t
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

;;;; Custom ASDF file types
(defclass js-file (static-file) ())
(defmethod source-file-type ((c js-file) (s module)) "js")
(defmethod operation-done-p ((o load-op) (c js-file))  
  t)
(defmethod operation-done-p ((o compile-op) (c js-file))
  t)

;;;; System definition
(asdf:defsystem jwacs 
  :version "0.1"
  :author "James Wright <chumsley@gmail.com>, Greg Smolyn <greg@smolyn.org>"
  :licence "BSD License <http://www.opensource.org/licenses/bsd-license.php>"
  :serial t
  :components ((:module "external"
                        :components
                        ((:file "yacc")))
               (:js-file "jw-runtime")
               (:file "package")
               (:file "general-utilities")
               (:file "lexer-macros")
               (:file "lex-javascript")
               (:file "js-source-model")
               #+use-yacc (:file "parse-javascript-yacc")
               (:file "parse-javascript")
               (:file "pretty-print")
               (:file "source-transformations")
               (:file "shift-function-decls-transformation")
               (:file "ugly-print")
               (:file "type-analysis")
               (:file "explicitize-transformation")
               (:file "cps-transformation")
               (:file "loop-transformation")
               (:file "trampoline-transformation")
               (:file "runtime-transformation")
               (:file "compiler"))
  :depends-on (cl-ppcre))

(defmethod perform ((o test-op) (c (eql (find-system 'jwacs))))
  (operate 'load-op 'jwacs-tests)
  (operate 'test-op 'jwacs-tests :force t))
