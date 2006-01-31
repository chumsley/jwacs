;;;; test-explicitize.lisp
;;;
;;; Tests for the explicitize transformation (and related functions)
(in-package :jwacs-tests)

;;;; Test categories 
(defnote explicitize "tests for the explicitize transformation")

;;;; Tests 
(deftest explicitize/var-decl/1 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (parse "
      var x = foo(bar(baz(50 + 20)));")))
  #.(parse "
      var JW0 = baz(50 + 20);
      var JW1 = bar(JW0);
      var x = foo(JW1);"))

(deftest explicitize/var-decl/2 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (parse "
      var x = foo(bar(baz(50 + quux(10))));")))
  #.(parse "
      var JW0 = quux(10);
      var JW1 = baz(50 + JW0);
      var JW2 = bar(JW1);
      var x = foo(JW2);"))

(deftest explicitize/var-decl/3 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (parse "
      return foo(bar(y));")))
  #.(parse "
      var JW0 = bar(y);
      return foo(JW0);"))

(deftest explicitize/var-decl/4 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (parse "
      var x = 10, y = 20;")))
  #.(parse "
      var x = 10;
      var y = 20;"))

(deftest explicitize/var-decl/5 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (parse "
      var x = foo(bar()), y = baz(quux());")))
  #.(parse "
      var JW0 = bar();
      var x = foo(JW0);
      var JW1 = quux();
      var y = baz(JW1);"))

(deftest explicitize/unary/1 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (parse "
      delete foo(bar(x));")))
  #.(parse "
      var JW0 = bar(x);
      var JW1 = foo(JW0);
      delete JW1;"))

(deftest explicitize/if-condition/1 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (parse "
      if(foo()) { var x = bar(); return x; }")))
  #.(parse "
      var JW0 = foo();
      if(JW0) { var x = bar(); return x; }"))

(deftest explicitize/if-condition/2 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (parse "
      if(x > 10)
      {
         var y = foo(bar(10));
      }")))
  #.(parse "
      if(x > 10)
      {
        var JW0 = bar(10);
        var y = foo(JW0);
      }"))

(deftest explicitize/if-condition/3 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (parse "
      if(x > 10)
         var y = foo(bar(10));")))
  #.(parse "
      if(x > 10)
      {
        var JW0 = bar(10);
        var y = foo(JW0);
      }"))

(deftest explicitize/if-condition/4 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (parse "
      if(foo(10))
        return 1;
      else if(bar(10))
        return 2;
      else
        return foo(bar(3));")))
  #.(parse "
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
    (transform 'explicitize (parse "
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
  #.(parse "
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
    (transform 'explicitize (parse "
      while(true)
      {
        bar(baz());
      }")))
  #.(parse "
      while(true)
      {
        var JW0 = baz();
        bar(JW0);
      }"))

(deftest explicitize/for-in/1 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (parse "
      for(x in foo())
      {
        bar(baz());
      }")))
  #.(parse "
      var JW0 = foo();
      for(x in JW0)
      {
        var JW1 = baz();
        bar(JW1);
      }"))

(deftest explicitize/factorial/1 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (parse "
      function factorial(n)
      {
        if(n == 0)
          return 1;
        else
          return n * factorial(n - 1);
      }")))
  #.(parse "
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
    (transform 'explicitize (parse "
      var obj = { field: 44,
                  method: function() { return factorial(this.field) * 2; }};
      function fn()
      {
        obj.method(obj.field + foo());
      }")))
  #.(parse "
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
    (transform 'explicitize (parse "
      var x = foo() ? bar() : baz();")))
  #.(parse "
      var JW0 = foo();
      if(JW0)
        var JW1 = bar();
      else
        var JW2 = baz();
      var x = JW0 ? JW1 : JW2;"))

(deftest explicitze/conditional/2 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (parse "
      foo(bar() ? baz() : quux());")))
  #.(parse "
      var JW0 = bar();
      if(JW0)
        var JW1 = baz();
      else
        var JW2 = quux();
      foo(JW0 ? JW1 : JW2);"))

(deftest explicitize/conditional/3 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (parse "
      foo(x ? y : calculateZ());")))
  #.(parse "
      if(x)
        ;
      else
        var JW0 = calculateZ();
      foo(x ? y : JW0);"))

(deftest explicitize/short-circuit-and/1 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (parse "
      var x = foo() && bar() && baz();")))
  #.(parse "
      var JW0 = foo();
      if(JW0)
        var JW1 = bar();
      var JW3 = JW0 && JW1;
      if(JW3)
        var JW2 = baz();
      var x = JW3 && JW2;"))

(deftest explicitize/short-circuit-and/2 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (parse "
      var w = foo() && x && bar();")))
  #.(parse "
      var JW0 = foo();
      var JW2 = JW0 && x;
      if(JW2)
        var JW1 = bar();
      var w = JW2 && JW1;"))

(deftest explicitize/short-circuit-or/1 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (parse "
      foo(x() || y() || z());")))
  #.(parse "
      var JW0 = x();
      if(!JW0)
        var JW1 = y();
      var JW3 = JW0 || JW1;
      if(!JW3)
        var JW2 = z();
      foo(JW3 || JW2);"))

(deftest explicitize/short-circuit-or/2 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (parse "
      var w = x || expensive() || null;")))
  #.(parse "
      if(!x)
        var JW0 = expensive();
      var w = x || JW0 || null;"))
      
      
(deftest explicitize/short-circuit-mixed/1 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (parse "
      foo(w() && x() || y() && z());")))
  #.(parse "
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
    (transform 'explicitize (parse "
      foo(50);")))
  #.(parse "
      foo(50);"))

(deftest explicitize/bare-fn-call/2 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (parse "
      function foo()
      {
        bar(50);
      }")))
  #.(parse "
      function foo()
      {
        bar(50);
      }"))

(deftest explicitize/bare-fn-call/3 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (parse "
      x = function()
      {
        bar(50);
      };")))
  #.(parse "
      x = function()
      {
        bar(50);
      };"))