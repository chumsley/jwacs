;;;; jwacs-tests.asd
;;;
;;; Defines an asdf system containing unit tests for jwacs.

(defpackage :jwacs-tests-system
  (:use :cl :asdf)
  (:nicknames :jw-tests-system))
(in-package :jwacs-tests-system)

(asdf:defsystem jwacs-tests
  :version "0.1"
  :author "James Wright <chumsley@gmail.com>"
;  :license "BSD License <http://www.opensource.org/licenses/bsd-license.php>"
  :serial t
  :components
  ((:module "external"
            :components
            ((:file "rt")))
   (:module "tests"
            :serial t
            :components
            ((:file "package")
             (:file "test-lexer")
             (:file "test-parser")
             (:file "test-pretty-print")
             (:file "test-source-transformations"))))
  :depends-on (jwacs))

