;;;; jwacs-tests.asd
;;;
;;; Defines an asdf system containing unit tests for jwacs.

(defpackage :jwacs-tests-system
  (:use :cl :asdf :uiop)
  (:nicknames :jw-tests-system))
(in-package :jwacs-tests-system)

;;;; ======= Custom ASDF file types ================================================================
(defclass jwacs-file (static-file)
  ((type :initform "jw")))

;;;; ======= System definition =====================================================================
(defsystem "jwacs-tests"
    :author "James Wright <chumsley@gmail.com>, Greg Smolyn <greg@smolyn.org>"
    :license "MIT License <http://www.opensource.org/licenses/mit-license.php>"
    :description "Unit tests for jwacs"
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
               (:file "test-static-analysis")
               (:file "test-type-analysis")
               (:file "test-ugly-print")
               (:file "test-source-transformations")
               (:file "test-shift-decls-transformation")
               (:file "test-explicitize")
               (:file "test-shadow-values-transformation")
               (:file "test-cps-transformation")
               (:file "test-loop-transformation")
               (:file "test-trampoline-transformation")
               (:file "test-runtime-transformation")
               (:jwacs-file "lang-tests"))))
    :depends-on ("jwacs")
    :perform (test-op (o c) (symbol-call :jw-tests :do-tests)))
