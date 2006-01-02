;;;; test-trampoline-transformation.lisp
;;;
;;; Tests for the trampoline transformation
(in-package :jwacs-tests)

(defnote trampoline "tests for the trampoline transformation")

(deftest trampoline/result-ret/1 :notes trampoline
  (transform 'trampoline
             (parse "return 50;"))
  #.(parse "return {done: true, result: 50};"))

(deftest trampoline/thunk-call/1 :notes trampoline
  (transform 'trampoline
             (parse "return fn(4);"))
  #.(parse "return {done: false,
                    thunk: function() {
                        return fn(4);
                    }};"))


    