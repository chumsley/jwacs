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
  (test-transform 'trampoline
             (parse "return fn(4);"))
  #.(parse "return {done: false,
                    thunk: function() {
                        return fn(4);
                    }};"))

;; Verify that the correct administrative source-element gets created
(deftest trampoline/inlined-thunk/2 :notes trampoline
  (transform 'trampoline
             (parse "return fn(4);"))
  (#s(return-statement
      :arg
      #S(object-literal :properties
                        ((#s(identifier :name "done") . #S(special-value :symbol :false))
                         (#s(identifier :name "thunk") . #S(thunk-function
                                                            :body
                                                            (#s(return-statement
                                                                :arg
                                                                #S(fn-call :fn #S(identifier :name "fn")
                                                                           :args (#S(numeric-literal :value 4))))))))))))

(deftest trampoline/function-expression-recursion/1 :notes trampoline
  (test-transform 'trampoline (parse "
        return factorial(function(JW0) { return $k(n * JW0); }, n-1);"))
  #.(parse "
        return {done: false,
                thunk: function() {
                        return factorial(function(JW0) {
                                           return {done: false, thunk: function() { return $k(n * JW0); }};
                                         }, n - 1);
                       }};"))
    
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

(deftest trampoline/resume/2 :notes trampoline
  (transform 'trampoline (parse "
      resume foo[bar] <- baz;"))
  #.(parse "
      return foo[bar](baz);"))

