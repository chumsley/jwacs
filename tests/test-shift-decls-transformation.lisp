;;;; test-shift-function-decls.lisp
;;;
;;; Tests for the shift-decls transformation
(in-package :jwacs-tests)

(deftest shift-decls/1
  (transform 'shift-decls (parse "
    var global1, global2 = 20;
    WScript.echo(global2);
    function foo()
    {
      bar();
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
    var global1;
    var global2;
    var global3;
    function foo()
    {
      function inner(h)
      {
        return h * 10;
      }
      bar();
      var a = 10;
      var b = 20;
    }
    function bar() { return -88; }
    global2 = 20;
    WScript.echo(global2);
    global3 = /h/g;"))


(deftest shift-decls/2
  (transform 'shift-decls (parse "
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

(deftest shift-decls/3
  (transform 'shift-decls (parse "
      var obj = { field: 44, method: function() { return this.field * 2; }};
      function fn()
      {
        obj.method(obj.field);
      }"))
  #.(parse "
      var obj;
      function fn()
      {
        obj.method(obj.field);
      }
      obj = { field: 44, method: function() { return this.field * 2; }};"))

(deftest shift-decls/nested-var-decls/1
  (transform 'shift-decls (parse "
      foo();
      var x = 10;
      try
      {
        var y = 20;
      }
      catch(e)
      {
        bar(e);
      }"))
  #.(parse "
      var x;
      var y;
      foo();
      x = 10;
      try
      {
        y = 20;
      }
      catch(e)
      {
        bar(e);
      }"))

(deftest shift-decls/nested-var-decls/2
  (transform 'shift-decls (parse "
      foo();
      var x = 10;
      try
      {
        var y = 20;
      }
      catch(e)
      {
        var z;
        bar(e);
      }"))
  #.(parse "
      var x;
      var y;
      var z;
      foo();
      x = 10;
      try
      {
        y = 20;
      }
      catch(e)
      {
        bar(e);
      }"))
