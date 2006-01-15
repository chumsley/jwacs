;;;; jwacs-tests.asd
;;;
;;; Defines an asdf system containing unit tests for jwacs.

(defpackage :jwacs-tests-system
  (:use :cl :asdf)
  (:nicknames :jw-tests-system))
(in-package :jwacs-tests-system)

(asdf:defsystem jwacs-tests
    :version "0.1"
    :author "James Wright <chumsley@gmail.com>, Greg Smolyn <greg@smolyn.org>"
    :licence "BSD License <http://www.opensource.org/licenses/bsd-license.php>"
    :serial t
    :components
    ((:module "external"
              :components
              ((:file "rt")))
     (:module "tests"
              :serial t
              :components
              ((:file "package")
               (:file "test-utils")
               (:file "test-lexer")
               (:file "test-parser")
               (:file "test-pretty-print")
               (:file "test-ugly-print")
               (:file "test-source-transformations")
               (:file "test-shift-function-decls")
               (:file "test-explicitize")
               (:file "test-cps-transformation")
               (:file "test-loop-transformation")
               (:file "test-trampoline-transformation")
               (:file "test-runtime-transformation"))))
    :depends-on (jwacs))

(defmethod perform ((o test-op) (c (eql (find-system 'jwacs-tests))))
  (operate 'load-op :jwacs)
  (funcall (intern (symbol-name '#:do-tests) (find-package :jw-tests))))
