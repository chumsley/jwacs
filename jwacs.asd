;;;; jwacs.asd
;;;
;;; This is the system definition file for the jwacs project.
;;; It defines the asdf system plus any extra asdf operations
;;; (eg test-op).

(defpackage :jwacs-system
  (:use :cl :asdf)
  (:nicknames :jw-system))
(in-package :jwacs-system)

;;TODO This will become more complex once Greg's cl-yacc port is done
(require "parsergen")

(asdf:defsystem jwacs 
  :version "0.1"
  :author "James Wright <chumsley@gmail.com>"
  :license "BSD License <http://www.opensource.org/licenses/bsd-license.php>"
  :serial t
  :components ((:file "package")
               (:file "lexer-macros")
               (:file "lex-javascript")
               (:file "js-source-model")
               (:file "parse-javascript")
               (:file "pretty-print")
               (:file "source-transformations"))
  :depends-on (cl-ppcre))
