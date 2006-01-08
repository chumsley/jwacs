;;;; test-cps-transformation.lisp
;;;
;;; Tests for the cps transformation
(in-package :jwacs-tests)

;;;; Test categories 
(defnote cps "tests for the cps transformation")

;;;; Tests 
(deftest cps/factorial/1 :notes cps
  (with-fresh-genvar
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
                                                        :right-arg #s(numeric-literal :value 1))))))))))
     #s(binary-operator :op-symbol :assign
                        :left-arg #s(property-access :field #s(string-literal :value "$callStyle")
                                                     :target #s(identifier :name "factorial1"))
                        :right-arg #s(string-literal :value "cps"))))

(deftest cps/symmetric-dangling-tail/1 :notes cps
  (with-fresh-genvar
    (transform 'cps (parse "
      function doStuff(branch)
      {
        if(branch)
          foo();
        else
          bar();
        baz();
        }")))
  #.(parse "
      function doStuff($k, branch)
      {
        if(branch)
          return $call(foo, function(dummy$0) { return $call(baz, function(dummy$1) { return $k(); }, this, []); }, this, []);
        else
          return $call(bar, function(dummy$2) { return $call(baz, function(dummy$3) { return $k(); }, this, []); }, this, []);
      }
      doStuff.$callStyle='cps';"))

(deftest cps/asymmetric-dangling-tail/1 :notes cps
  (with-fresh-genvar
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
      function factorial2($k, n)
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
                              return $k(retVal);
                            }, n-1);
        }

        return $k(retVal);
      }
      factorial2.$callStyle = 'cps';"))
  
(deftest cps/post-function-dangling-tail/1 :notes cps
  (with-fresh-genvar
    (transform 'cps (parse "
      function foo(branch)
      {
        if(branch)
          return foo(false);
        else
        {
          WScript.echo('hi');
          return foo(true);
        }
      }
      foo(false);")))
  #.(parse "
      function foo($k, branch)
      {
        if(branch)
          return foo($k, false);
        else
        {
          return $call(WScript.echo, function(dummy$0) { return foo($k, true); }, this, ['hi']);
        }
      }
      foo.$callStyle='cps';
      foo(function(dummy$1) { return $k(); }, false);"))

(deftest cps/post-function-dangling-tail/2 :notes cps
  (with-fresh-genvar
    (transform 'cps (parse "
      function foo()
      {
        onEvent = function(e) { process(e); };
        bar();
      }")))
  #.(parse "
      function foo($k)
      {
        onEvent = $cpsLambda(function JW0($k, e)
                             {
                               if(typeof $k != 'function')
                                 return JW0($id, $k, e);
                               return $call(process, function(dummy$1) { return $k(); }, this, [e]);
                             });
        return $call(bar, function(dummy$2) { return $k(); }, this, []);
      }
      foo.$callStyle = 'cps';"))

(deftest cps/tail-fn-call/1 :notes cps
  (with-fresh-genvar
    (in-local-scope
      (transform 'cps (parse "
        return factorial(JW0);"))))
  #.(parse "
      return $call(factorial, $k, this, [JW0]);"))

(deftest cps/inline-call-with-tail/1 :notes cps
  (with-fresh-genvar
    (transform 'cps (parse "
      function foo() {}
      function bar()
      {
        foo();
        x += 10;
      }")))
  #.(parse "
      function foo($k) { return $k(); }
      foo.$callStyle = 'cps';
      function bar($k)
      {
        return foo(function (dummy$0) {
                     x += 10;
                     return $k();
                   });
      }
      bar.$callStyle = 'cps';"))

(deftest cps/indirected-tailless-call/1 :notes cps
  (with-fresh-genvar
    (in-local-scope
      (transform 'cps (parse "
        foo();"))))
  #.(parse "
        return $call(foo, function (dummy$0) { return $k(); }, this, []);"))

(deftest cps/object-literal/1 :notes cps
  (with-fresh-genvar
    (transform 'cps (parse "
      var obj =
      {
        field: 44, 
        method: function()
        {
          var x = factorial(this.field);
          return x * 2;
        }
      };
      function fn()
      {
        var y = obj.field + 1;
        obj.method(y);
      }")))
  #.(parse "
      var obj =
      {
        field: 44,
        method: $cpsLambda(function JW0($k)
        {
          if(typeof $k != 'function')
            return JW0($id, $k);
          return factorial(function(x) {
                             return $k(x * 2);
                           }, this.field);
        })
      };
      function fn($k)
      {
        var y = obj.field + 1;
        return $call(obj.method, function(dummy$1) { return $k(); }, this, [y]);
      }
      fn.$callStyle = 'cps';"))

(deftest cps/implicit-return/1 :notes cps
  (transform 'cps (parse "
    function foo()
    {
    }"))
  #.(parse "
    function foo($k)
    {
      return $k();
    }
    foo.$callStyle = 'cps';"))

(deftest cps/implicit-return/2 :notes cps
  (transform 'cps (parse "
    function foo()
    {
      x = 10;
    }"))
  #.(parse "
    function foo($k)
    {
      x = 10;
      return $k();
    }
    foo.$callStyle = 'cps';"))

(deftest cps/implicit-return/3 :notes cps
  (with-fresh-genvar
    (transform 'cps (parse "
    function foo()
    {
      if(x)
        bar();
      else
        y = 10;
      z = 20;
    }")))
  #.(parse "
    function foo($k)
    {
      if(x)
        return $call(bar, function(dummy$0) { z = 20; return $k(); }, this, []);
      else
        y = 10;
      
      z = 20;
      return $k();
    }
    foo.$callStyle = 'cps';"))

(deftest cps/suspend-transformation/1 :notes cps
  (with-fresh-genvar
    (transform 'cps (parse "
      suspend foo.bar;
      var baz = 10 + 12;")))
    #.(parse "
    {
      var JW0 = function() {var baz = 10 + 12;};
      foo.bar = JW0;
      JW0();
    }"))

(deftest cps/resume-transformation/1 :notes cps
  (transform 'cps (parse "
      resume foo[bar];"))
  #.(parse "
      return foo[bar]();"))

