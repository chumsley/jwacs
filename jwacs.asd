;;; jwacs.asd

(defpackage #:jwacs-system
  (:use :cl :asdf)
  (:nicknames :jw-system))
(in-package :jwacs-system)

(require "parsergen")                   ; TODO Use cl-yacc instead?

(asdf:defsystem jwacs 
  :version "0.1"
  :author "James Wright <chumsley@gmail.com>"
  :license "BSD License <http://www.opensource.org/licenses/bsd-license.php>"
  :serial t
  :components ((:file   "package")
               (:file   "lexer-macros")
               (:file   "lex-javascript")
               (:file   "js-source-model")
               (:file   "parse-javascript")
               (:file   "pretty-print"))
  :depends-on (cl-ppcre))
