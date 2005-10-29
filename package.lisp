;;; package.lisp
;;; Define the packages used by the sugarscript system

;; Eventually this may want to be several sub-packages, but let's start simple for now
(defpackage :sugarscript
    (:use :cl :parsergen :cl-ppcre)
  (:nicknames :sscript :ss)
  (:export #:parse))

