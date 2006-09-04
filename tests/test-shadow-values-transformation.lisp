;;;; test-shadow-values-transformation.lisp
;;;
;;; Tests for the shadow-values transformation
;;;
;;; Copyright (c) 2006 James Wright
;;; See LICENSE for full licensing details.
;;;
(in-package :jw-tests)

(defnote shadow-values "Tests for the shadow-values transformation")

(deftest shadow-values/fn-decl/arguments-referenced/1 :notes shadow-values
  (with-fresh-genvar
    (test-transform 'shadow-values (test-parse "
      function foo(a, b)
      {
        if(arguments.length < 2)
          throw 'too few args';
        else
          return arguments[0] + ',' + arguments[1];
      }")))
  #.(test-parse "
      function foo(a, b)
      {
        var arguments$0 = arguments;
        if(arguments$0.length < 2)
          throw 'too few args';
        else
          return arguments$0[0] + ',' + arguments$0[1];
      }"))

(deftest shadow-values/fn-decl/this-referenced/1 :notes shadow-values
  (with-fresh-genvar
    (test-transform 'shadow-values (test-parse "
      function foo(a, b)
      {
        this.foo = 'foo';
        if(!this.bar)
          this.bar = 'bar';
      }")))
  #.(test-parse "
      function foo(a, b)
      {
        var this$0 = this;
        this$0.foo = 'foo';
        if(!this$0.bar)
          this$0.bar = 'bar';
      }"))

(deftest shadow-values/fn-decl/both-referenced/1 :notes shadow-values
  (with-fresh-genvar
    (test-transform 'shadow-values (test-parse "
      function foo(a, b)
      {
        this.foo = arguments.length;
        if(!this.bar)
          this.bar = arguments[0];
      }")))
  #.(test-parse "
      function foo(a, b)
      {
        var arguments$0 = arguments;
        var this$1 = this;
        this$1.foo = arguments$0.length;
        if(!this$1.bar)
          this$1.bar = arguments$0[0];
      }"))

(deftest shadow-values/fn-decl/neither-referenced/1 :notes shadow-values
  (with-fresh-genvar
    (test-transform 'shadow-values (test-parse "
      function foo(a, b)
      {
        return 24;
      }")))
  #.(test-parse "
      function foo(a, b)
      {
        return 24;
      }"))

(deftest shadow-values/fn-expression/arguments-referenced/1 :notes shadow-values
  (with-fresh-genvar
    (test-transform 'shadow-values (test-parse "
      var foo = function(a, b)
      {
        if(arguments.length < 2)
          throw 'too few args';
        else
          return arguments[0] + ',' + arguments[1];
      };")))
  #.(test-parse "
      var foo = function(a, b)
      {
        var arguments$0 = arguments;
        if(arguments$0.length < 2)
          throw 'too few args';
        else
          return arguments$0[0] + ',' + arguments$0[1];
      };"))

(deftest shadow-values/fn-expression/this-referenced/1 :notes shadow-values
  (with-fresh-genvar
    (test-transform 'shadow-values (test-parse "
      var foo = function(a, b)
      {
        this.foo = 'foo';
        if(!this.bar)
          this.bar = 'bar';
      };")))
  #.(test-parse "
      var foo = function(a, b)
      {
        var this$0 = this;
        this$0.foo = 'foo';
        if(!this$0.bar)
          this$0.bar = 'bar';
      };"))

(deftest shadow-values/fn-expression/both-referenced/1 :notes shadow-values
  (with-fresh-genvar
    (test-transform 'shadow-values (test-parse "
      var foo = function(a, b)
      {
        this.foo = arguments.length;
        if(!this.bar)
          this.bar = arguments[0];
      };")))
  #.(test-parse "
      var foo = function(a, b)
      {
        var arguments$0 = arguments;
        var this$1 = this;
        this$1.foo = arguments$0.length;
        if(!this$1.bar)
          this$1.bar = arguments$0[0];
      };"))

(deftest shadow-values/fn-expression/neither-referenced/1 :notes shadow-values
  (with-fresh-genvar
    (test-transform 'shadow-values (test-parse "
      var foo = function(a, b)
      {
        return 24;
      };")))
  #.(test-parse "
      var foo = function(a, b)
      {
        return 24;
      };"))

(deftest shadow-values/position-preservation/1 :notes shadow-values
  (with-fresh-genvar
    (transform 'shadow-values (parse "function foo() { this.x = 10; return arguments; }")))
  (#s(function-decl :name "foo" :parameters nil
                    :body (#s(var-decl-statement :var-decls
                                                 (#s(var-decl :name "arguments$0" :initializer #s(special-value :symbol :arguments))))
                           #s(var-decl-statement :var-decls
                                                 (#s(var-decl :name "this$1" :initializer #s(special-value :symbol :this))))
                           #s(binary-operator :op-symbol :assign
                                              :left-arg #s(property-access :target #s(identifier :name "this$1" :start 17 :end 21)
                                                                           :field #s(string-literal :start 22 :end 23 :value "x")
                                                                           :start 17 :end 23)
                                              :right-arg #s(numeric-literal :start 26 :end 28 :value 10)
                                              :start 17 :end 28)
                           #s(return-statement :arg #s(identifier :start 37 :end 46 :name "arguments$0")
                                               :start 30 :end 47 ))
                    :start 0 :end 47)))