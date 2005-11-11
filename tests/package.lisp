;;;; package.lisp
;;; Defines the package used by the unit tests

(defpackage :jwacs-tests
  (:use :cl :rtest :cl-ppcre :jwacs)
  (:nicknames :jw-tests))