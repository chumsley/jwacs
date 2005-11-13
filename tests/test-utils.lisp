;;;; test-utils.lisp
;;;
;;; Some helper functions for testing in general (i.e., that are
;;; not specific to testing a certain file).

(in-package :jw-tests)

(defun flag-expected-failure (test-name)
  "Add a test to the list of expected failures"
  (pushnew test-name rtest::*expected-failures*))

