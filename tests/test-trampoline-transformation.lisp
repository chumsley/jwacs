;;;; test-trampoline-transformation.lisp
;;;
;;; Tests for the trampoline transformation
(in-package :jwacs-tests)

(defnote trampoline "tests for the trampoline transformation")

(deftest trampoline/inlined-result/1 :notes trampoline
  (transform 'trampoline
             (parse "return 50;"))
  #.(parse "return {done: true, result: 50};"))

(deftest trampoline/inlined-result/2 :notes trampoline
  (transform 'trampoline
             (parse "return x[50];"))
  #.(parse "return {done: true, result: x[50]};"))

(deftest trampoline/inlined-thunk/1 :notes trampoline
  (let ((jw::*function-decls-in-scope* '("fn")))
    (transform 'trampoline
               (parse "return fn(4);")))
  #.(parse "return {done: false,
                    thunk: function() {
                        return fn(4);
                    }};"))

(deftest trampoline/indirected-tail-call/1 :notes trampoline
  (let ((jw::*function-decls-in-scope* '()))
    (transform 'trampoline
               (parse "return fn(4);")))
  #.(parse "return $trampolineResult(fn, this, [4]);"))

(deftest trampoline/suspend/1 :notes trampoline
  (transform 'trampoline (parse "
    if(flag)
      suspend;
    else
      return 50;"))
  #.(parse "
    if(flag)
      return {done: true};
    else
      return {done: true, result: 50};"))

;; Note that we return the result of the continuation directly without
;; wrapping it in a thunk or a result object (a "trampoline box"?)
;; since it should already be trampoline boxed by the continuation iteself.
(deftest trampoline/resume/1 :notes trampoline
  (transform 'trampoline (parse "
      resume foo[bar];"))
  #.(parse "
      return foo[bar]();"))

