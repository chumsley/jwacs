;;;; test-cps-transformation.lisp
;;;
;;; Tests for the cps transformation
(in-package :jwacs-tests)

;;;; Test categories 
(defnote cps "tests for the cps transformation")

;;;; Tests 
(deftest cps/factorial/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
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
  (with-fresh-genvar
    (test-transform 'cps (parse "
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
        var ifK$0 = function()
        {
          return baz(function(dummy$1) { return $k(); });
        };
        if(branch)
          return foo(function(dummy$2) { resume ifK$0; });
        else
          return bar(function(dummy$3) { resume ifK$0; });
      }"))

(deftest cps/asymmetric-dangling-tail/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
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
        var ifK$0 = function()
        {
          return $k(retVal);
        };
        if(n == 0)
        {
          retVal = 1;
          resume ifK$0;
        }
        else
          return factorial2(function (r1)
                            {
                              var r2 = n * r1;
                              retVal = r2;
                              resume ifK$0;
                            }, n-1);
      }"))
  
(deftest cps/if/unreachable-tail/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo(userK)
      {
        if(x)
          return 40;
        else
          resume userK;
        output('this will never be seen');
      }")))
  #.(parse "
      function foo($k, userK)
      {
        if(x)
          return $k(40);
        else
          resume userK;
        return output(function(dummy$0) { return $k(); }, 'this will never be seen');
      }"))

(deftest cps/if/terminated-then-null-else/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo(userK)
      {
        if(x)
        {
          if(y)
            return 80;
          else
            resume userK <- 80;
        }
        output('might see this');
      }")))
  #.(parse "
      function foo($k, userK)
      {
        if(x)
          if(y)
            return $k(80);
          else
            resume userK <- 80;
        return output(function(dummy$0) { return $k(); }, 'might see this');
      }"))

(deftest cps/if/terminated-then-null-else/2 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo(x)
      {
        if(x)
        {
          output('yes');
          return true;
        }
        output('no');
      }")))
  #.(parse "
      function foo($k, x)
      {
        if(x)
          return output(function(dummy$0) { return $k(true); }, 'yes');
        return output(function(dummy$1) { return $k(); }, 'no');
      }"))


(deftest cps/if/terminated-then-unterminated-else/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo(userK)
      {
        if(x)
        {
          if(y)
            return 80;
          else
            resume userK <- 80;
        }
        else
          output('also this');
        output('might see this');
      }")))
  #.(parse "
      function foo($k, userK)
      {
        if(x)
          if(y)
            return $k(80);
          else
            resume userK <- 80;
        else
          return output(function(dummy$0) {
                                return output(function(dummy$1) { return $k(); },
                                              'might see this');
                        }, 'also this');
      }"))
      
(deftest cps/if/unterminated-then-terminated-else/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo(userK)
      {
        if(x)
          output('also this');
        else
        {
          if(y)
            return 80;
          else
            resume userK <- 80;
        }
          
        output('might see this');
      }")))
  #.(parse "
      function foo($k, userK)
      {
        if(x)
          return output(function(dummy$0) {
                                return output(function(dummy$1) { return $k(); },
                                              'might see this');
                        }, 'also this');
        else
          if(y)
            return $k(80);
          else
            resume userK <- 80;
      }"))     

(deftest cps/post-function-dangling-tail/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
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
          return WScript.echo(function(dummy$0) { return foo($k, true); }, 'hi');
        }
      }
      foo(function(dummy$1) { return $k(); }, false);"))

(deftest cps/post-function-dangling-tail/2 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo()
      {
        onEvent = function(e) { process(e); };
        bar();
      }")))
  #.(parse "
      function foo($k)
      {
        onEvent = function($k, e)
                  {
                    return process(function(dummy$0) { return $k(); }, e);
                  };
        return bar(function(dummy$1) { return $k(); });
      }"))

(deftest cps/no-tail-after-if/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo(x)
      {
        if(x)
          return x * 10;
        else
          return 20;
      }")))
  #.(parse "
      function foo($k, x)
      {
        if(x)
          return $k(x * 10);
        else
          return $k(20);
      }"))
        
(deftest cps/switch-statement/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo(x)
      {
        switch(x)
        {
          case 1:
            bar();
            break;

          default:
            return 88;
        }

        return baz();
      }")))
  #.(parse "
      function foo($k, x)
      {
        var switchK$0 = function()
        {
          return baz($k);
        };
        switch(x)
        {
          case 1:
            return bar(function(dummy$1) { resume switchK$0; });
          default:
            return $k(88);
        }
      }"))

(deftest cps/switch-statement/2 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo(x)
      {
        switch(x)
        {
          case 10:
            bar();
            break;
        }
        return baz(x + 2);
      }")))
  #.(parse "
      function foo($k, x)
      {
        var switchK$0 = function()
        {
          return baz($k, x + 2);
        };
        switch(x)
        {
          case 10:
            return bar(function(dummy$1) { resume switchK$0; });
          default:
            resume switchK$0;
        }
      }"))

(deftest cps/switch-statement/3 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo(x)
      {
        switch(x)
        {
          case 10:
            bar();
        }
        return baz(x + 2);
      }")))
  #.(parse "
      function foo($k, x)
      {
        var switchK$0 = function()
        {
          return baz($k, x + 2);
        };
        switch(x)
        {
          case 10:
            return bar(function(dummy$1) { resume switchK$0; });
          default:
            resume switchK$0;
        }
      }"))

(deftest cps/switch-statement/4 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo(x)
      {
        switch(x)
        {
          case 10:
            foo();
          case 15:
          case 20:
            bar();
        }
        return baz(x - 1);
      }")))
  #.(parse "
      function foo($k, x)
      {
        var switchK$0 = function()
        {
          return baz($k, x - 1);
        };
        switch(x)
        {
          case 10:
            return foo(function(dummy$1) { return bar(function(dummy$2) { resume switchK$0; }); });
          case 15:
          case 20:
            return bar(function(dummy$3) { resume switchK$0; });
          default:
            resume switchK$0;
        }
      }"))
        
(deftest cps/labelled-switch/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo(x, y)
      {
        outer:
        switch(x)
        {
          case 10:
          inner:
          switch(y)
          {
            case 20:
            doSomething();
            break inner;
          }
          doSomethingElse();
          break;
        }
        return doAlways();
      }")))
  #.(parse "
      function foo($k, x, y)
      {
        var switchK$0 = function()
        {
          return doAlways($k);
        };
        switch(x)
        {
          case 10:
            var switchK$1 = function()
            {
              return doSomethingElse(function(dummy$2) { resume switchK$0; });
            };
            switch(y)
            {
              case 20:
                return doSomething(function(dummy$3) { resume switchK$1; });
              default:
                resume switchK$1;
            }
          default:
            resume switchK$0;
        }
      }"))

(deftest cps/labelled-switch/2 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo(x, y)
      {
        outer:
        switch(x)
        {
          case 10:
          inner:
          switch(y)
          {
            case 20:
            doSomething();
            break outer;
          }
          doSomethingElse();
          break;
        }
        return doAlways();
      }")))
  #.(parse "
      function foo($k, x, y)
      {
        var switchK$0 = function()
        {
          return doAlways($k);
        };
        switch(x)
        {
          case 10:
            var switchK$1 = function()
            {
              return doSomethingElse(function(dummy$2) { resume switchK$0; });
            };
            switch(y)
            {
              case 20:
                return doSomething(function(dummy$3) { resume switchK$0; });
              default:
                resume switchK$1;
            }
          default:
            resume switchK$0;
        }
      }"))
  
(deftest cps/simple-loop/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      while(true)
      {
        if(x++ > 10)
          break;
        output(x);
        continue;
      }
      return 10;")))
  #.(parse "
      var break$0 = function()
      {
        return $k(10);
      };
      var continue$1 = function()
      {
        if(x++ > 10)
          resume break$0;
        return output(function(dummy$2) { resume continue$1; }, x);
      };
      resume continue$1;"))

(deftest cps/nested-loop/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      outer:
      while(true)
      {
        if(x++ > 10)
          break;
        inner:
        while(true)
        {
          if(y++ > 10)
            break;
          continue;
        }
        continue;
      }
      return 10;")))
  #.(parse "
      var break$0 = function()
      {
        return $k(10);
      };
      var continue$1 = function()
      {
        if(x++ > 10)
          resume break$0;
        var break$2 = function() { resume continue$1; };
        var continue$3 = function()
        {
          if(y++ > 10)
            resume break$2;
          resume continue$3;
        };
        resume continue$3;
      };
      resume continue$1;"))
     
(deftest cps/nested-loop/2 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      outer:
      while(true)
      {
        if(x++ > 10)
          break outer;
        inner:
        while(true)
        {
          if(y++ > 10)
            break inner;
          if(y % 2 == 0)
            break outer;
          continue inner;
        }
        continue outer;
      }
      return 10;")))
  #.(parse "
      var break$0 = function()
      {
        return $k(10);
      };
      var continue$1 = function()
      {
        if(x++ > 10)
          resume break$0;
        var break$2 = function() { resume continue$1; };
        var continue$3 = function()
        {
          if(y++ > 10)
            resume break$2;
          if(y % 2 == 0)
            resume break$0;
          resume continue$3;
        };
        resume continue$3;
      };
      resume continue$1;"))      

(deftest cps/nested-loop/3 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      outer:
      while(true)
      {
        if(x++ > 10)
          break outer;
        inner:
        while(true)
        {
          if(y++ > 10)
            break inner;
          if(y % 2 == 0)
            continue outer;
          continue inner;
        }
        continue outer;
      }
      return 10;")))
  #.(parse "
      var break$0 = function()
      {
        return $k(10);
      };
      var continue$1 = function()
      {
        if(x++ > 10)
          resume break$0;
        var break$2 = function() { resume continue$1; };
        var continue$3 = function()
        {
          if(y++ > 10)
            resume break$2;
          if(y % 2 == 0)
            resume continue$1;
          resume continue$3;
        };
        resume continue$3;
      };
      resume continue$1;"))

(deftest cps/tail-fn-call/1 :notes cps
  (with-fresh-genvar
    (in-local-scope
      (transform 'cps (parse "
        return factorial(JW0);"))))
  #.(parse "
      return factorial($k, JW0);"))

(deftest cps/inline-call-with-tail/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo() {}
      function bar()
      {
        foo();
        x += 10;
      }")))
  #.(parse "
      function foo($k) { return $k(); }
      function bar($k)
      {
        return foo(function (dummy$0) {
                     x += 10;
                     return $k();
                   });
      }"))

(deftest cps/object-literal/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
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
        method: function ($k)
        {
          return factorial(function(x) {
                             return $k(x * 2);
                           }, this.field);
        }
      };
      function fn($k)
      {
        var y = obj.field + 1;
        return obj.method(function(dummy$0) { return $k(); }, y);
      }"))

(deftest cps/implicit-return/1 :notes cps
  (transform 'cps (parse "
    function foo()
    {
    }"))
  #.(parse "
    function foo($k)
    {
      return $k();
    }"))

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
    }"))

(deftest cps/implicit-return/3 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
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
      var ifK$0 = function()
      {
        z = 20;
        return $k();
      };
      if(x)
        return bar(function(dummy$1) { resume ifK$0; });
      else
      {
        y = 10;
        resume ifK$0;
      }
    }"))

;; `suspend` and `resume` are handled by the TRAMPOLINE transformation.
;; Capturing the current function's current continuation is handled by
;; the CPS transformation.
(deftest cps/function_continuation/1 :notes cps
  (transform 'cps
             (parse "x = function_continuation;"))
  #.(parse "x = $k;"))
