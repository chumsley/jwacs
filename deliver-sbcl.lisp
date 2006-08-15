;;;; deliver.lisp
;;;
;;; Delivery script for producing a jwacs binary using SBCL.
;;;
;;; Copyright (c) 2006 James Wright
;;; See LICENSE for full licensing details.
;;;
(require :asdf)
(require :jwacs)
(in-package :jwacs)

(sb-ext:save-lisp-and-die jw-system:*executable-name*
                          :executable t
                          :toplevel #'main)
