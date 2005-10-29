;;; sugarscript.asd

(defpackage #:sugarscript-system
  (:use :cl :asdf)
  (:nicknames :sscript-system :ss-system))
(in-package :sugarscript-system)

(require "parsergen")                   ; TODO Use cl-yacc instead?

(asdf:defsystem sugarscript 
  :version "0.1"
  :author "James Wright <chumsley@gmail.com>"
  :license "BSD License <http://www.opensource.org/licenses/bsd-license.php>"
  :serial t
  :components ((:file   "package")
               (:file   "lexer-macros")
               (:file   "lex-javascript")
               (:file   "parse-javascript")
               (:file   "pretty-print"))
  :depends-on (cl-ppcre))
