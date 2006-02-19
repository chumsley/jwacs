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
            return bar($makeK(function (dummy$0) { return baz($k); }));
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
          return bar($makeK(function(dummy$0) {
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
          return bar($makeK(function(dummy$0) {
                              return $call(Foo.Bar.Baz.quux, $k, Foo.Bar.Baz, [100, 101, 102, 103, 104, 105, 106, 107, 108, 109]); 
                              }), 50);
        }
        foo.$jw = true;"))