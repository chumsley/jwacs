;;;; test-shift-function-decls.lisp
;;;
;;; Tests for the shift-function-decls transformation
(in-package :jwacs-tests)

(deftest shift-function-decls/1
  (transform 'shift-function-decls (parse "
    var global1, global2 = 20;
    function foo()
    {
      var a = 10;
      function inner(h)
      {
        return h * 10;
      }
      var b = 20;
    }
    var global3 = /h/g;
    function bar() { return -88; }"))
  #.(parse "
    function foo()
    {
      function inner(h)
      {
        return h * 10;
      }
      var a = 10;
      var b = 20;
    }
    function bar() { return -88; }
    var global1, global2 = 20;
    var global3 = /h/g;"))

(deftest shift-function-decls/2
  (transform 'shift-function-decls (parse "
    function foo()
    {
      var a = 1;
      if(x)
      {
        var b;
        var fex = function functionExpression() // Doesn't move; only function decls move
        {
          return inner(2);
          function inner(arg) { return arg; } // Moves up within functionExpression's body only
        };
        var c = 100;
      }
    }"))
  #.(parse "
    function foo()
    {
      var a = 1;
      if(x)
      {
        var b;
        var fex = function functionExpression()
        {
          function inner(arg) { return arg; }
          return inner(2);
        };
        var c = 100;
      }
    }"))

(deftest shift-function-decls/3
  (transform 'shift-function-decls (parse "
      var obj = { field: 44, method: function() { return this.field * 2; }};
      function fn()
      {
        obj.method(obj.field);
      }"))
  #.(parse "
      function fn()
      {
        obj.method(obj.field);
      }
      var obj = { field: 44, method: function() { return this.field * 2; }};"))

   