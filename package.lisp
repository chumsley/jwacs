;;;; package.lisp
;;;
;;; Define the packages used by the jwacs system.
;;;
;;; Copyright (c) 2005 James Wright
;;; See LICENSE for full licensing details.

;; Eventually this may want to be several sub-packages, but let's start simple for now
(defpackage :jwacs
  (:use :cl :cl-ppcre)
  (:nicknames :jw)
  (:export
   #:parse
   #:process #:build-app
   #:syntax-error #:missing-import))
