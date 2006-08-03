;;;; deliver.lisp
;;;
;;; Delivery script for producing a jwacs binary using SBCL.
(require :asdf)
(require :jwacs)
(in-package :jwacs)

(defparameter *executable-name*
  #+win32 "jwacs.exe"
  #-win32 "jwacs"
  "The name of the executable to create")

(sb-ext:save-lisp-and-die *executable-name* :executable t
                          :toplevel #'main)
