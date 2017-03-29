;;;; jwacs.asd
;;;
;;; This is the system definition file for the jwacs project.
;;; It defines the asdf system plus any extra asdf operations
;;; (eg test-op).

(defpackage :jwacs-system
  (:use :cl :asdf)
  (:nicknames :jw-system))
(in-package :jwacs-system)

;;;; ======= Compilation configuration =============================================================
(defparameter *muffle-conflicts* t
  "When T, yacc warnings about Shift/Reduce and Reduce/Reduce conflicts will be muffled.
   When NIL, all such conflicts will be reported.
   When non-NIL, non-T, a single summary warning will be reported when conflicts exist.

   This value should be set to NIL or non-T during grammar
   development/debugging (so that we find out about the conflicts), but T
   at all other times (so that SBCL won't drop into the debugger when
   we're trying to load parse-javascript.lisp).")

;;;; ======= Custom ASDF file types ================================================================
(defclass js-file (static-file)
  ((type :initform "js")))

;;;; ======= System definition =====================================================================
(defsystem "jwacs"
  :version "0.3"
  :author "James Wright <james@chumsley.org> et al"
  :license "MIT License <http://www.opensource.org/licenses/mit-license.php>"
  :description "Javascript With Advanced Continuation Support"
  :serial t
  :class program-system
  :build-operation program-op
  :build-pathname "jwacs"
  :entry-point "jwacs:main"
  :components ((:module "external"
                        :components
                        ((:file "yacc")))
               ;;TODO Should these three non-Lisp files go into a separate module?
               (:js-file "jw-runtime")
               (:js-file "jw-debug-runtime")
               (:html-file "default-template")
               (:html-file "default-iframe")
               (:file "package")
               (:file "general-utilities")
               (:file "conditions")
               (:file "lexer-macros")
               (:file "lex-javascript")
               (:file "js-source-model")
               (:file "parse-javascript-yacc")
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
               (:file "main"))
  :depends-on ("cl-ppcre")
  :in-order-to ((test-op (test-op "jwacs-tests"))))
