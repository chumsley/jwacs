;;;; deliver.lisp
;;;
;;; Delivery script for producing a jwacs binary using SBCL.
;;;
;;; Copyright (c) 2006 James Wright
;;; See LICENSE for full licensing details.
;;;
(require "asdf")

(asdf:make "jwacs")
