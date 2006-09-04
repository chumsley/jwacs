;;;; test-explicitize.lisp
;;;
;;; Tests for the explicitize transformation (and related functions)
;;;
;;; Copyright (c) 2005 James Wright
;;; See LICENSE for full licensing details.
;;;
(in-package :jwacs-tests)

;;;; Test categories 
(defnote explicitize "tests for the explicitize transformation")

;;;; Tests 
(deftest explicitize/var-decl/1 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      var x = foo(bar(baz(50 + 20)));")))
  #.(test-parse "
      var JW0 = baz(50 + 20);
      var JW1 = bar(JW0);
      var x = foo(JW1);"))

(deftest explicitize/var-decl/2 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      var x = foo(bar(baz(50 + quux(10))));")))
  #.(test-parse "
      var JW0 = quux(10);
      var JW1 = baz(50 + JW0);
      var JW2 = bar(JW1);
      var x = foo(JW2);"))

(deftest explicitize/var-decl/3 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      return foo(bar(y));")))
  #.(test-parse "
      var JW0 = bar(y);
      return foo(JW0);"))

(deftest explicitize/var-decl/4 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      var x = 10, y = 20;")))
  #.(test-parse "
      var x = 10;
      var y = 20;"))

(deftest explicitize/var-decl/5 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      var x = foo(bar()), y = baz(quux());")))
  #.(test-parse "
      var JW0 = bar();
      var x = foo(JW0);
      var JW1 = quux();
      var y = baz(JW1);"))

(deftest explicitize/unary/1 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      delete foo(bar(x));")))
  #.(test-parse "
      var JW0 = bar(x);
      var JW1 = foo(JW0);
      delete JW1;"))

(deftest explicitize/if-condition/1 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      if(foo()) { var x = bar(); return x; }")))
  #.(test-parse "
      var JW0 = foo();
      if(JW0) { var x = bar(); return x; }"))

(deftest explicitize/if-condition/2 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      if(x > 10)
      {
         var y = foo(bar(10));
      }")))
  #.(test-parse "
      if(x > 10)
      {
        var JW0 = bar(10);
        var y = foo(JW0);
      }"))

(deftest explicitize/if-condition/3 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      if(x > 10)
         var y = foo(bar(10));")))
  #.(test-parse "
      if(x > 10)
      {
        var JW0 = bar(10);
        var y = foo(JW0);
      }"))

(deftest explicitize/if-condition/4 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      if(foo(10))
        return 1;
      else if(bar(10))
        return 2;
      else
        return foo(bar(3));")))
  #.(test-parse "
      var JW0 = foo(10);
      if(JW0)
        return 1;
      else
      {
        var JW1 = bar(10);
        if(JW1)
          return 2;
        else
        {
          var JW2 = bar(3);
          return foo(JW2);
        }
      }"))

(deftest explicitize/switch/1 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      switch(foo(bar(100)))
      {
        case 8:
        case 10:
          return baz(foo(bar(20)));
        case 12:
          baz(quux(12));
          break;
        default:
          return quux(quuux(null));
      }")))
  #.(test-parse "
      var JW0 = bar(100);
      var JW1 = foo(JW0);
      switch(JW1)
      {
        case 8:
        case 10:
          var JW2 = bar(20);
          var JW3 = foo(JW2);
          return baz(JW3);
        case 12:
          var JW4 = quux(12);
          baz(JW4);
          break;
        default:
          var JW5 = quuux(null);
          return quux(JW5);
      }"))

(deftest explicitize/while/1 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      while(true)
      {
        bar(baz());
      }")))
  #.(test-parse "
      while(true)
      {
        var JW0 = baz();
        bar(JW0);
      }"))

(deftest explicitize/for-in/1 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      for(x in foo())
      {
        bar(baz());
      }")))
  #.(test-parse "
      var JW0 = foo();
      for(x in JW0)
      {
        var JW1 = baz();
        bar(JW1);
      }"))

(deftest explicitize/factorial/1 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      function factorial(n)
      {
        if(n == 0)
          return 1;
        else
          return n * factorial(n - 1);
      }")))
  #.(test-parse "
      function factorial(n)
      {
        if(n == 0)
          return 1;
        else
        {
          var JW0 = factorial(n - 1);
          return n * JW0;
        }
      }"))

(deftest explicitize/object-literal/1 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      var obj = { field: 44,
                  method: function() { return factorial(this.field) * 2; }};
      function fn()
      {
        obj.method(obj.field + foo());
      }")))
  #.(test-parse "
      var obj =
      {
        field: 44, 
        method: function()
        {
          var JW0 = factorial(this.field);
          return JW0 * 2;
        }
      };
      function fn()
      {
        var JW1 = foo();
        obj.method(obj.field + JW1);
      }"))

(deftest explicitize/conditional/1 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      var x = foo() ? bar() : baz();")))
  #.(test-parse "
      var JW0 = foo();
      if(JW0)
        var JW1 = bar();
      else
        var JW2 = baz();
      var x = JW0 ? JW1 : JW2;"))

(deftest explicitze/conditional/2 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      foo(bar() ? baz() : quux());")))
  #.(test-parse "
      var JW0 = bar();
      if(JW0)
        var JW1 = baz();
      else
        var JW2 = quux();
      foo(JW0 ? JW1 : JW2);"))

(deftest explicitize/conditional/3 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      foo(x ? y : calculateZ());")))
  #.(test-parse "
      if(x)
        ;
      else
        var JW0 = calculateZ();
      foo(x ? y : JW0);"))

(deftest explicitize/short-circuit-and/1 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      var x = foo() && bar() && baz();")))
  #.(test-parse "
      var JW0 = foo();
      if(JW0)
        var JW1 = bar();
      var JW3 = JW0 && JW1;
      if(JW3)
        var JW2 = baz();
      var x = JW3 && JW2;"))

(deftest explicitize/short-circuit-and/2 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      var w = foo() && x && bar();")))
  #.(test-parse "
      var JW0 = foo();
      var JW2 = JW0 && x;
      if(JW2)
        var JW1 = bar();
      var w = JW2 && JW1;"))

(deftest explicitize/short-circuit-or/1 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      foo(x() || y() || z());")))
  #.(test-parse "
      var JW0 = x();
      if(!JW0)
        var JW1 = y();
      var JW3 = JW0 || JW1;
      if(!JW3)
        var JW2 = z();
      foo(JW3 || JW2);"))

(deftest explicitize/short-circuit-or/2 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      var w = x || expensive() || null;")))
  #.(test-parse "
      if(!x)
        var JW0 = expensive();
      var w = x || JW0 || null;"))
      
      
(deftest explicitize/short-circuit-mixed/1 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      foo(w() && x() || y() && z());")))
  #.(test-parse "
      var JW0 = w();
      if(JW0)
        var JW1 = x();
      var JW4 = JW0 && JW1;
      if(!JW4)
      {
        var JW2 = y();
        if(JW2)
          var JW3 = z();
      }      
      foo(JW4 || JW2 && JW3);"))

(deftest explicitize/bare-fn-call/1 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      foo(50);")))
  #.(test-parse "
      foo(50);"))

(deftest explicitize/bare-fn-call/2 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      function foo()
      {
        bar(50);
      }")))
  #.(test-parse "
      function foo()
      {
        bar(50);
      }"))

(deftest explicitize/bare-fn-call/3 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      x = function()
      {
        bar(50);
      };")))
  #.(test-parse "
      x = function()
      {
        bar(50);
      };"))

(deftest explicitize/new/1 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      new Foo(bar(), baz(quux()), quuux() + 1);")))
  #.(test-parse "
      var JW0 = bar();
      var JW1 = quux();
      var JW2 = baz(JW1);
      var JW3 = quuux();
      new Foo(JW0, JW2, JW3 + 1);"))

(deftest explicitize/new/2 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      var foo = new Bar(100 + baz());")))
  #.(test-parse "
      var JW0 = baz();
      var foo = new Bar(100 + JW0);"))

(deftest explicitize/new/3 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      var foo = bar(new Baz());")))
  #.(test-parse "
      var JW0 = new Baz();
      var foo = bar(JW0);"))

(deftest explicitize/new/4 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      new (foo(bar()))(baz());"))) ; Uses constructor returned by foo
  #.(test-parse "
      var JW1 = bar();
      var JW2 = foo(JW1);
      var JW0 = baz();
      new JW2(JW0);"))
      

(deftest explicitize/comma-expr/1 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      if(x, f(g(y)), z)
        return h(j(z));")))
  #.(test-parse "
      var JW0 = g(y);
      var JW1 = f(JW0);
      if(x, JW1, z)
      {
        var JW2 = j(z);
        return h(JW2);
      }"))

(deftest explicitize/property-access/1 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      var foo = bar(20).baz;")))
  #.(test-parse "
      var JW0 = bar(20);
      var foo = JW0.baz;"))

(deftest explicitize/throw-statement/1 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      throw foo(10);")))
  #.(test-parse "
      var JW0 = foo(10);
      throw JW0;"))

(deftest explicitize/throw-statement/2 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      throw new Error -> k;")))
  #.(test-parse "
      var JW0 = new Error;
      throw JW0 -> k;"))

(deftest explicitize/throw-statement/3 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      throw foo() -> bar.fetchK();")))
  #.(test-parse "
      var JW0 = foo();
      var JW1 = bar.fetchK();
      throw JW0 -> JW1;"))

(deftest explicitize/resume-statement/1 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      resume fetchK(20);")))
  #.(test-parse "
      var JW0 = fetchK(20);
      resume JW0;"))

(deftest explicitize/resume-statement/2 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      resume k <- new Date;")))
  #.(test-parse "
      var JW0 = new Date;
      resume k <- JW0;"))

(deftest explicitize/resume-statement/3 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (test-parse "
      resume bar.fetchK() <- foo();")))
  #.(test-parse "
      var JW0 = bar.fetchK();
      var JW1 = foo();
      resume JW0 <- JW1;"))

(deftest explicitize/position-preservation/1 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (parse "resume foo();")))
  (#s(var-decl-statement :var-decls (#s(var-decl :name "JW0"
                                                 :initializer #s(fn-call :fn #s(identifier :name "foo"
                                                                                           :start 7 :end 10)
                                                                         :start 7 :end 10))))
   #s(resume-statement :target #s(identifier :name "JW0")
                       :start 0 :end 13)))