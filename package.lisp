;;;; package.lisp
;;;
;;; Define the packages used by the jwacs system.

;; Eventually this may want to be several sub-packages, but let's start simple for now
(defpackage :jwacs
  (:use :cl :cl-ppcre)
  (:nicknames :jw)
  (:export
   #:parse
   #:process
   #:build-app))
