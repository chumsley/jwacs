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
(defparameter *use-yacc* t
  "When t, use cl-yacc to generate a parser.  Otherwise, use the Lispworks parsergen.")

(if *use-yacc*
  (pushnew :use-yacc *features*)
  (setf *features* (remove :use-yacc *features*)))

#-use-yacc
(require "parsergen")

(asdf:defsystem jwacs 
  :version "0.1"
  :author "James Wright <chumsley@gmail.com>"
;  :license "BSD License <http://www.opensource.org/licenses/bsd-license.php>"
  :serial t
  :components ((:module "external"
                        :components
                        ((:file "yacc")))
               (:file "package")
               (:file "lexer-macros")
               (:file "lex-javascript")
               (:file "js-source-model")
               #+use-yacc (:file "parse-javascript-yacc")
               (:file "parse-javascript")
               (:file "pretty-print")
               (:file "source-transformations"))
  :depends-on (cl-ppcre))
