;;;; test-trampoline-transformation.lisp
;;;
;;; Tests for the trampoline transformation
;;;
;;; Copyright (c) 2006 James Wright
;;; See LICENSE for full licensing details.
;;;
(in-package :jwacs-tests)

(defnote trampoline "tests for the trampoline transformation")

(deftest trampoline/inlined-result/1 :notes trampoline
  (transform 'trampoline
             (test-parse "return 50;"))
  #.(test-parse "return {done: true, result: 50};"))

(deftest trampoline/inlined-result/2 :notes trampoline
  (transform 'trampoline
             (test-parse "return x[50];"))
  #.(test-parse "return {done: true, result: x[50]};"))

(deftest trampoline/inlined-thunk/1 :notes trampoline
  (test-transform 'trampoline
             (parse "return fn(4);"))
  #.(test-parse "return {done: false,
                    thunk: function($e) {
                        return fn(4);
                    }};"))

;; Verify that the correct administrative source-element gets created
(deftest trampoline/inlined-thunk/2 :notes trampoline
  (transform 'trampoline
             (test-parse "return fn(4);"))
  (#s(return-statement
      :arg
      #S(object-literal :properties
                        ((#s(string-literal :value "done") . #S(special-value :symbol :false))
                         (#s(string-literal :value "thunk") . #S(thunk-function
                                                                 :parameters ("$e")
                                                                 :body
                                                                 (#s(return-statement
                                                                     :arg
                                                                     #S(fn-call :fn #S(identifier :name "fn")
                                                                                :args (#S(numeric-literal :value 4))))))))))))


(deftest trampoline/inlined-thunk/3 :notes trampoline
  (let ((jw::*debug-mode* t))
    (test-transform 'trampoline
                    (parse "return fn(4);")))
  #.(test-parse "return {startPos: 0, endPos: 13,
                      done: false,
                      thunk: function($e, $localEvalArg) {
                          if($localEvalArg) return $id(eval($localEvalArg));
                          return fn(4);
                      }};"))
  
(deftest trampoline/new-expr/1 :notes trampoline
  (test-transform 'trampoline (parse "
        return new Object;"))
  #.(test-parse "
        return {done: false,
                thunk: function($e) {
                        return new Object;
                       }
               };"))
        
(deftest trampoline/new-expr/2 :notes trampoline
  (test-transform 'trampoline (parse "
        return new Foo($k, 10);"))
  #.(test-parse "
        return {done: false,
                thunk: function($e) {
                        return new Foo($k, 10);
                       }
               };"))

(deftest trampoline/function-expression-recursion/1 :notes trampoline
  (test-transform 'trampoline (parse "
        return factorial(function(JW0) { return $k(n * JW0); }, n-1);"))
  #.(test-parse "
        return {done: false,
                thunk: function($e) {
                        return factorial(function(JW0) {
                                           return {done: false, thunk: function($e) { return $k(n * JW0); }};
                                         }, n - 1);
                       }};"))
    
(deftest trampoline/suspend/1 :notes trampoline
  (in-local-scope
    (test-transform 'trampoline (parse "
    if(flag)
      suspend;
    else
      return 50;")))
  #.(test-parse "
    if(flag)
      return {done: true};
    else
      return {done: true, result: 50};"))

;; Note that we return the result of the continuation directly without
;; wrapping it in a thunk or a result object (a "trampoline box"?)
;; since it should already be trampoline boxed by the continuation iteself.
(deftest trampoline/resume/1 :notes trampoline
  (in-local-scope
    (test-transform 'trampoline (parse "
      resume foo[bar];")))
  #.(test-parse "
      return {replaceHandlers: foo[bar].$exHandlers, done: false, thunk: function($e) {
        return foo[bar](); }};"))

(deftest trampoline/resume/2 :notes trampoline
  (in-local-scope
    (test-transform 'trampoline (parse "
      resume foo[bar] <- baz;")))
  #.(test-parse "
      return {replaceHandlers: foo[bar].$exHandlers, done: false, thunk: function($e) {
        return foo[bar](baz); }};"))

;; Note that the toplevel version of trampoline's tranformation of resume statements
;; isn't any different from the in-local-scope version anymore.  I'm keeping these
;; tests around in case I change my mind about where to resolve toplevel issues.
(deftest trampoline/resume/toplevel/1 :notes trampoline
  (test-transform 'trampoline (parse "
      resume foo;"))
  #.(test-parse "
      return {replaceHandlers: foo.$exHandlers, done: false, thunk: function($e) {
        return foo(); }};"))

(deftest trampoline/resume/toplevel/2 :notes trampoline
  (test-transform 'trampoline (parse "
      resume foo <- bar;"))
  #.(test-parse "
      return {replaceHandlers: foo.$exHandlers, done: false, thunk: function($e) {
        return foo(bar); }};"))

(deftest trampoline/throw/1 :notes trampoline
  (transform 'trampoline (test-parse "
      throw 100;"))
  #.(test-parse "
      throw 100;"))

(deftest trampoline/throw/2 :notes trampoline
  (let ((jw::*debug-mode* t))
    (test-transform 'trampoline (parse "throw 100;")))
  #.(test-parse "return {startPos: 0, endPos: 10,
                         done: false,  
                         thunk: function($e, $localEvalArg) {
                            if($localEvalArg) return $id(eval($localEvalArg));
                            throw 100;
                         }};"))

(deftest trampoline/throw/3 :notes trampoline
  (test-transform 'trampoline (parse "
      throw 100 -> k;"))
  #.(test-parse "
      return {replaceHandlers: k.$exHandlers, done: false, thunk: function($e) {
        throw 100;
      }};"))

;;TODO position-preservation unit tests