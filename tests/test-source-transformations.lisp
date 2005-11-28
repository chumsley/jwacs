;;;; test-source-transformation.lisp
;;;
;;; Tests for the source transformations.

(in-package :jwacs-tests)

;;;;= Helper functions =
(defmacro with-fresh-genvars ((&optional (cont-name *cont-name*)) &body body)
  "Make sure that GENVAR variable names will start from 0 and that
   continuation arguments will have a known value"
  `(let* ((*genvar-counter* 0)
          (*cont-name* ,cont-name)
          (*cont-id* (make-identifier :name *cont-name*)))
    ,@body))

;;;;= Test categories =
(defnote source-transformations "tests for the source-transformations")
(defnote cps "tests for the cps transformation")
(defnote explicitize "tests for the explicitize transformation")
    
;;;;= Tests =

;;;;== General traversal behaviour ==
(defmethod transform ((xform (eql 'hello)) (elm string))
    "hello there!")

(deftest source-transformations/general-behaviour/1 :notes source-transformations
  (transform 'hello #S(continue-statement :label "go away!"))
  #S(continue-statement :label "hello there!"))

(deftest source-transformations/general-behaviour/2 :notes source-transformations
  (transform 'hello '("string 1" symbol ("string 2")))
  ("hello there!" symbol ("hello there!")))

;;;;== Shift-function-decl transformation ==
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


;;;;== CPS transformation ==
(deftest cps/factorial/1 :notes cps
  (with-fresh-genvars ("$k")
    (transform 'cps (parse "
       function factorial1(n)
       {
         if(n == 0)
           return 1;
         else
         {
           var r1 = factorial1(n-1);
           return n * r1;
         }
       }")))
  (#s(function-decl
      :name "factorial1" :parameters ("$k" "n")
      :body (#s(if-statement
                :condition
                #s(binary-operator :op-symbol :equals
                                   :left-arg #s(identifier :name "n")
                                   :right-arg #s(numeric-literal :value 0))
                :then-statement
                #s(return-statement :arg
                              #s(fn-call :fn #s(identifier :name "$k")
                                         :args (#s(numeric-literal :value 1))))
                :else-statement
                #s(statement-block
                   :statements
                   (#s(return-statement
                       :arg
                       #s(fn-call :fn #s(identifier :name "factorial1")
                                  :args
                                  (#s(function-expression
                                      :parameters ("r1")
                                      :body (#s(return-statement
                                                :arg #s(fn-call :fn #s(identifier :name "$k")
                                                                :args (#s(binary-operator :op-symbol :multiply
                                                                                          :left-arg #s(identifier :name "n")
                                                                                          :right-arg #s(identifier :name "r1")))))))
                                     #s(binary-operator :op-symbol :subtract
                                                        :left-arg #s(identifier :name "n")
                                                        :right-arg #s(numeric-literal :value 1))))))))))))

(deftest cps/symmetric-dangling-tail/1 :notes cps
  (with-fresh-genvars ("$k")
    (transform 'cps (parse "
      function doStuff(branch)
      {
        if(branch)
          foo();
        else
          bar();
        baz();
        }")))
  (#s(function-decl
      :name "doStuff" :parameters ("$k" "branch")
      :body (#s(if-statement :condition #s(identifier :name "branch")
                             :then-statement #s(return-statement :arg #s(fn-call :fn #s(identifier :name "foo")
                                                                                :args (#s(function-expression
                                                                                          :parameters ("JW0")
                                                                                          :body (#s(return-statement :arg #s(fn-call :fn #s(identifier :name "baz")
                                                                                                                               :args (#s(identifier :name "$k")))))))))
                             :else-statement #s(return-statement :arg #s(fn-call :fn #s(identifier :name "bar")
                                                                                :args (#s(function-expression
                                                                                          :parameters ("JW1")
                                                                                          :body (#s(return-statement :arg #s(fn-call :fn #s(identifier :name "baz")
                                                                                                                                   :args (#s(identifier :name "$k"))))))))))))))
(deftest cps/asymmetric-dangling-tail/1 :notes cps
  (with-fresh-genvars ("_k")
    (transform 'cps (parse "
      function factorial2(n)
      {
        var retVal;
        if(n == 0)
          retVal = 1;
        else
        {
          var r1 = factorial2(n-1);
          var r2 = n * r1;
          retVal = r2;
        }
        return retVal;
      }")))
  #.(parse "
      function factorial2(_k, n)
      {
        var retVal;
        if(n == 0)
          retVal = 1;
        else
        {
          return factorial2(function (r1)
                            {
                              var r2 = n * r1;
                              retVal = r2;
                              return _k(retVal);
                            }, n-1);
        }

        return _k(retVal);
      }"))
  

;;;;== Explicitization transformation ==
(deftest explicitize/var-decl/1 :notes explicitize
  (with-fresh-genvars ()
    (transform 'explicitize (parse "
      var x = foo(bar(baz(50 + 20)));")))
  #.(parse "
      var JW0 = baz(50 + 20);
      var JW1 = bar(JW0);
      var x = foo(JW1);"))

(deftest explicitize/var-decl/2 :notes explicitize
  (with-fresh-genvars ()
    (transform 'explicitize (parse "
      var x = foo(bar(baz(50 + quux(10))));")))
  #.(parse "
      var JW0 = quux(10);
      var JW2 = baz(50 + JW0);
      var JW3 = bar(JW2);
      var x = foo(JW3);"))

(deftest explicitize/var-decl/3 :notes explicitize
  (with-fresh-genvars ()
    (transform 'explicitize (parse "
      return foo(bar(y));")))
  #.(parse "
      var JW0 = bar(y);
      return foo(JW0);"))

(deftest explicitize/unary/1 :notes explicitize
  (with-fresh-genvars ()
    (transform 'explicitize (parse "
      delete foo(bar(x));")))
  #.(parse "
      var JW0 = bar(x);
      var JW1 = foo(JW0);
      delete JW1;"))

(deftest explicitize/if-condition/1 :notes explicitize
  (with-fresh-genvars ()
    (transform 'explicitize (parse "
      if(foo()) { var x = bar(); }")))
  #.(parse "
      var JW0 = foo();
      if(JW0) { var x = bar(); }"))

(deftest explicitize/if-condition/2 :notes explicitize
  (with-fresh-genvars ()
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
  (with-fresh-genvars ()
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
  (with-fresh-genvars ()
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
  (with-fresh-genvars ()
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