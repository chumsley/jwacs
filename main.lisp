;;;; main.lisp
;;;
;;; Defines the MAIN function which is the entry-point for the delivered binary.
(in-package :jwacs)

(defun main ()
  "This is the main entry-point for the jwacs binary."
  (let ((target (second sb-ext:*posix-argv*)))
    (format t "~%~%~
               JWACS pre-alpha~%~
               ---------------")
    (unless target
      (format t "~&Usage: ~A <application-filename>~%" *executable-name*)
      (return-from main 255))
    (format t "~&Building application from ~S...~%" target)
    (pprint (build-app target) *standard-output*)
    0))