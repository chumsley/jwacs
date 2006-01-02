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
      var JW2 = baz(50 + JW0);
      var JW3 = bar(JW2);
      var x = foo(JW3);"))

(deftest explicitize/var-decl/3 :notes explicitize
  (with-fresh-genvar
    (transform 'explicitize (parse "
      return foo(bar(y));")))
  #.(parse "
      var JW0 = bar(y);
      return foo(JW0);"))

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
      if(foo()) { var x = bar(); }")))
  #.(parse "
      var JW0 = foo();
      if(JW0) { var x = bar(); }"))

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
          var JW5 = quux(12);
          baz(JW5);
          break;
        default:
          var JW6 = quuux(null);
          return quux(JW6);
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
        var JW2 = foo();
        obj.method(obj.field + JW2);
      }"))


   