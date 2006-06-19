;;;; test-runtime-transformation.lisp
;;;
;;; Unit tests for the runtime transformation
(in-package :jwacs-tests)

(defnote runtime "tests for the runtime transformation")

(deftest runtime/continuation/1 :notes runtime
  (with-fresh-genvar
    (let ((jw::*function-decls-in-scope* '("bar" "baz")))
      (transform 'runtime
                 (transform 'cps (parse "
        function foo()
        {
            bar();
            return baz();
        }")))))
  #.(parse "
        function foo($k)
        {
            if(!$k || !$k.$isK)
              return $callFromDirect(foo, this, arguments);
            return bar($makeK(function () { return baz($k); }));
        }
        foo.$jw = true;"))

(deftest runtime/function-expression/1 :notes runtime
  (with-fresh-genvar
    (transform 'runtime (parse "
      var x = function($k) { return $k(21); };")))
  #.(parse "
      var x = $lambda(function lambda$0($k) {
                        if(!$k || !$k.$isK)
                          return $callFromDirect(lambda$0, this, arguments);
                        return $k(21); });"))

(deftest runtime/trampoline-return/1 :notes runtime
  (with-fresh-genvar
    (transform 'runtime
               (transform 'trampoline (parse "
      function fact($k, x)
      {
        if(x == 0)
          return $k(1);
        else
          return fact($k, x - 1);
      }"))))
  #.(parse "
      function fact($k, x)
      {
        if(!$k || !$k.$isK)
          return $callFromDirect(fact, this, arguments);
        if(x == 0)
          return {done:false, thunk: function() { return $k(1); }};
        else
          return {done:false, thunk: function() { return fact($k, x - 1); }};
      }
      fact.$jw = true;"))

(deftest runtime/indirected-call/1 :notes runtime
  (with-fresh-genvar
    (transform 'runtime
               (transform 'cps (parse "
        function bar(x) { return x; }
        function foo()
        {
          bar(50);
          return baz(100);
        }"))))
  #.(parse "
        function bar($k, x)
        {
          if(!$k || !$k.$isK)
            return $callFromDirect(bar, this, arguments);
          return $k(x);
        }
        bar.$jw = true;

        function foo($k)
        {
          if(!$k || !$k.$isK)
            return $callFromDirect(foo, this, arguments);
          return bar($makeK(function() {
                              return $call0(baz, $k, null, 100);
                              }), 50);
        }
        foo.$jw = true;"))

(deftest runtime/indirected-call/2 :notes runtime
  (with-fresh-genvar
    (transform 'runtime
               (transform 'cps (parse "
        function bar(x) { return x; }
        function foo()
        {
          bar(50);
          return Foo.Bar.Baz.quux(100, 101, 102, 103, 104, 105, 106, 107, 108, 109);
        }"))))
  #.(parse "
        function bar($k, x)
        {
          if(!$k || !$k.$isK)
            return $callFromDirect(bar, this, arguments);
          return $k(x);
        }
        bar.$jw = true;

        function foo($k)
        {
          if(!$k || !$k.$isK)
            return $callFromDirect(foo, this, arguments);
          return bar($makeK(function() {
                              return $call(Foo.Bar.Baz.quux, $k, Foo.Bar.Baz, [100, 101, 102, 103, 104, 105, 106, 107, 108, 109]); 
                              }), 50);
        }
        foo.$jw = true;"))

(deftest runtime/indirected-call/3 :notes runtime
  (with-fresh-genvar
    (transform 'runtime
               (transform 'cps (parse "
        function foo()
        {
          return Foo.Bar.Baz(10, 20);
        }"))))
  #.(parse "
        function foo($k)
        {
          if(!$k || !$k.$isK)
            return $callFromDirect(foo, this, arguments);
          return $call0('Baz', $k, Foo.Bar, 10, 20);
        }
        foo.$jw = true;"))

(deftest runtime/new-expr/1 :notes runtime
  (with-fresh-genvar
    (in-local-scope
      (test-transform 'runtime
                      (transform 'cps (parse "
        function bar(z) { return z; }
        var x = new Foo(50, 55);
        return bar(x);")))))
  #.(parse "
        function bar($k, z)
        {
          if(!$k || !$k.$isK)
            return $callFromDirect(bar, this, arguments);
          return $k(z);
        }
        bar.$jw = true;

        return $new0(Foo, $makeK(function(x) { return bar($k, x); }), 50, 55);"))

(deftest runtime/new-expr/2 :notes runtime
  (with-fresh-genvar
    (in-local-scope
      (test-transform 'runtime
                      (transform 'cps (parse "
        function bar(z) { return z; }
        var x = new Foo(1,2,3,4,5,6,7,8,9,10,11,12);
        return bar(x);")))))
  #.(parse "
        function bar($k, z)
        {
          if(!$k || !$k.$isK)
            return $callFromDirect(bar, this, arguments);
          return $k(z);
        }
        bar.$jw = true;

        return $new(Foo, $makeK(function(x) { return bar($k, x); }), [1,2,3,4,5,6,7,8,9,10,11,12]);"))

(deftest runtime/arguments/1 :notes runtime
  (with-fresh-genvar
    (test-transform 'runtime
                    (transform 'shadow-values
                               (transform 'cps (parse "
      function foo()
      {
        return arguments;
      }")))))
  #.(parse "
     function foo($k)
     {
        if(!$k || !$k.$isK)
          return $callFromDirect(foo, this, arguments);
        var arguments$0 = $makeArguments(arguments);
        return $k(arguments$0);
     }
     foo.$jw = true;"))

(deftest runtime/arguments/2 :notes runtime
  (test-transform 'runtime
                  (transform 'shadow-values
                             (transform 'cps (parse "
      function foo()
      {
        var arguments = 99;
        return arguments;
      }"))))
  #.(parse "
     function foo($k)
     {
        if(!$k || !$k.$isK)
          return $callFromDirect(foo, this, arguments);
        var arguments = 99;
        return $k(arguments);
     }
     foo.$jw = true;"))

(deftest runtime/arguments/3 :notes runtime
  (test-transform 'runtime
                  (transform 'shadow-values
                             (transform 'cps (parse "
      var x = arguments;"))))
  #.(parse "var x = arguments;"))

(deftest runtime/toplevel/resume/1 :notes runtime
  (test-transform 'runtime
                  (list
                   (jw::make-continuation-call :fn (make-identifier :name "foo"))))
  #.(parse "
      $trampoline(function() { return foo(); });"))

(deftest runtime/toplevel/indirect-call/1 :notes runtime
  (test-transform 'runtime
                  (transform 'trampoline
                             (transform 'cps (parse "
      foo(10);
      bar(20);"))))
  #.(parse "
      $trampoline(function() {
        return $call0(foo, $makeK(function() {
          return {done: false, thunk: function() {
            return $call0(bar, $makeK(function() {
              return {done: false, thunk: function() {
                return $k();
              }};
            }), null, 20);
          }};
        }), null, 10);
      });"))

(deftest runtime/toplevel/indirect-new/1 :notes runtime
  (test-transform 'runtime
                  (transform 'trampoline
                             (transform 'cps (parse "
      new foo(10);
      new bar(20);"))))
  #.(parse "
      $trampoline(function() {
        return $new0(foo, $makeK(function() {
          return {done: false, thunk: function() {
            return $new0(bar, $makeK(function() {
              return {done: false, thunk: function() {
                return $k();
              }};
            }), 20);
          }};
        }), 10);
      });"))