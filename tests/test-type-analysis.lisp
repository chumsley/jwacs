;;;; test-type-analysis.lisp
;;;
;;; Unit tests and benchmarks for type analysis functionality
(in-package :jw-tests)

(defnote type-analysis "tests for the type-analysis functionality")

(defun type-names (value-node-list)
  (sort (copy-list 
         (mapcar 'jw::value-node-constructor-name value-node-list))
        'string<))

(deftest type-analysis/simple-assignment/1 :notes type-analysis
  (type-names
   (compute-types #s(identifier :name "x")
                  (type-analyze (parse "x = 5 / '2.5'; y = 'str'; y = x;"))))
  ("Number"))

(deftest type-analysis/simple-assignment/2 :notes type-analysis
  (type-names
   (compute-types #s(identifier :name "y")
                  (type-analyze (parse "x = 5 / '2.5'; y = 'str'; y = x;"))))
   ("Number" "String"))

(deftest type-analysis/simple-assignment/3 :note type-analysis
  (type-names
   (compute-types #s(identifier :name "non-existo")
                  (type-analyze (parse "var x = 10; var y = str; y = x;"))))
  ("undefined"))

(deftest type-analysis/var-decl/1 :notes type-analysis
    (type-names
     (compute-types #s(identifier :name "y")
                    (type-analyze (parse "var x; x = 5 / '2.5'; y = 'str'; y = x;"))))
    ("Number" "String" "undefined"))

(deftest type-analysis/var-decl/2 :notes type-analysis
  (type-names
   (compute-types #s(identifier :name "y")
                  (type-analyze (parse "var x = 5 / '2.5', y = 'str'; y = x;"))))
  ("Number" "String"))

(deftest type-analysis/function-parameters/1 :notes type-analysis
  (type-names
   (compute-types
    #s(identifier :name "a")
    (type-analyze (parse "
     function foo()
     {
       var x = 20;
       var y = 'str';
       return bar(x, y);
     }

     function bar(a, b)
     {
       return a + b;
     }"))))
  ("Number"))

(deftest type-analysis/function-parameters/2 :notes type-analysis
  (type-names
   (compute-types
    #s(identifier :name "b")
    (type-analyze (parse "
     function foo()
     {
       var x = 20;
       var y = 'str';
       bar(x);
       return bar(x, y);
     }

     function bar(a, b)
     {
       return a + b;
     }"))))
  ("String" "undefined"))

(deftest type-analysis/function-return/1 :notes type-analysis
  (type-names
   (compute-types
    #s(identifier :name "x")
    (type-analyze (parse "
     function foo()
     {
       var x = bar(10);
     }

     function bar(a)
     {
       return a;
     }"))))
  ("Number"))

(deftest type-analysis/function-return/2 :notes type-analysis
  (type-names
   (compute-types
    #s(identifier :name "x")
    (type-analyze (parse "
     function foo()
     {
       var x = bar(10);
       var y = bar('str');
       bar();
     }

     function bar(a)
     {
       return a;
     }"))))
  ("Number" "String" "undefined"))

(deftest type-analysis/property-access/1 :notes type-analysis
  (type-names
   (compute-types
    #s(property-access :target #s(identifier :name "x") :field #s(string-literal :value "foo"))
    (type-analyze (parse "
     var x = new Object;
     x.foo = 20;"))))
  ("Number"))

(deftest type-analysis/property-access/2 :notes type-analysis
  (type-names
   (compute-types
    #s(property-access :target #s(identifier :name "x") :field #s(string-literal :value "foo"))
    (type-analyze (parse "
     var x = new Object;
     x.foo = 20;
     var y = x;
     y.foo = 'str';"))))
  ("Number" "String"))

(deftest type-analysis/property-access/3 :notes type-analysis
  (type-names
   (compute-types
    #s(property-access :target #s(identifier :name "x") :field #s(string-literal :value "foo"))
    (type-analyze (parse "
     function Foo()
     {
        this.foo = 55;
     }
     function Bar()
     {
        this.bar = 'str';
     }
     var x = new Foo;
     var y = new Bar;
     x = y;"))))
  ("Number" "undefined"))
  
(deftest type-analysis/new-expr/1 :notes type-analysis
  (type-names
   (compute-types
    #s(identifier :name "x")
    (type-analyze (parse "
     var ctors = new Object;
     ctors.num = Number;
     ctors.str = String;
     ctors.foo = Foo;
     var x = new ctors.str;"))))
  ("String"))

(deftest type-analysis/new-expr/2 :notes type-analysis
  (type-names
   (compute-types
    #s(identifier :name "x")
    (type-analyze (parse "
     function randCtor()
     {
        if(rand() % 2)
          return String;
        else
          return Number;
     }
     var x = new (randCtor());"))))
  ("Number" "String"))

(deftest type-analysis/new-expr/3 :notes type-analysis
  (type-names
   (compute-types
    #s(identifier :name "x")
    (type-analyze (parse "
     function id(a)
     {
        return a;
     }
     var x = new (id(String))();
     var y = new (id(RegExp))();"))))
  ("RegExp" "String"))
  
(deftest type-analysis/new-expr/arguments/1 :notes type-analysis
  (type-names
   (compute-types
    #s(property-access :target #s(identifier :name "x")
                       :field #s(string-literal :value "foo"))
    (type-analyze (parse "
     var x = new FooObj(55);
     var y = new FooObj('str');
     function FooObj(arg)
     {
        this.foo = arg;
     }"))))
  ("Number" "String"))

(deftest type-analysis/new-expr/arguments/2 :notes type-analysis
  (type-names
   (compute-types
    #s(property-access :target #s(identifier :name "x")
                       :field #s(string-literal :value "foo"))
    (type-analyze (parse "
     var x = new (FooFactory(80))('str');
     function FooFactory(ctor)
     {
        if(ctor == 1)
          return FooObj;
        else
          return BarObj;
     }
     function FooObj(a)
     {
        this.foo = a;
     }
     function BarObj(b)
     {
        this.foo = null;
        this.bar = b;
     }"))))
  ("String" "null"))
  
(deftest type-analysis/property-access/object-literals/1 :notes type-analysis
  (type-names
   (compute-types
    #s(property-access :target #s(property-access :target #s(identifier :name "x")
                                                  :field #s(string-literal :value "foo"))
                       :field #s(string-literal :value "a"))
    (type-analyze (parse "
     var x = new Object;
     x.foo = {a: null, b: 20};"))))
  ("null"))

(deftest type-analysis/property-access/object-literals/2 :notes type-analysis
  (type-names
   (compute-types
    #s(property-access :target #s(property-access :target #s(identifier :name "x")
                                                  :field #s(string-literal :value "foo"))
                       :field #s(string-literal :value "b"))
    (type-analyze (parse "
     var x = new Object;
     x.foo = {a: null, b: 20};"))))
  ("Number"))

(deftest type-analysis/property-access/function-calls/1 :notes type-analysis
  (type-names
   (compute-types
    #s(property-access :target #s(identifier :name "y")
                       :field #s(string-literal :value "a"))
    (type-analyze (parse "
     x.foo = function() { return {a: null, b: 20}; };
     y = x.foo();"))))
  ("null"))

(deftest type-analysis/property-access/function-calls/2 :notes type-analysis
  (type-names
   (compute-types
    #s(property-access :target #s(identifier :name "y")
                       :field #s(string-literal :value "a"))
    (type-analyze (parse "
     x.foo = function() { return {a: null, b: 20}; };
     y = x.foo();"))))
  ("null"))

(deftest type-analysis/property-access/function-expressions/1 :notes type-analysis
  (type-names
   (compute-types
    #s(property-access :target  #s(fn-call :fn #s(property-access :target #s(identifier :name "x")
                                                                  :field #s(string-literal :value "foo"))
                                           :args nil)
                       :field #s(string-literal :value "a"))
    (type-analyze (parse "
     var x = new Object;
     x.foo = function() { return {a: null, b: 20}; };"))))
  ("null"))

(deftest type-analysis/property-access/function-expressions/2 :notes type-analysis
  (type-names
   (compute-types
    #s(property-access :target  #s(fn-call :fn #s(property-access :target #s(identifier :name "x")
                                                                  :field #s(string-literal :value "foo"))
                                           :args nil)
                       :field #s(string-literal :value "non-existo"))
    (type-analyze (parse "
     var x = new Object;
     x.foo = function() { return {a: null, b: 20}; };"))))
  ("undefined"))

(deftest type-analysis/cycle/1 :notes type-analysis
  (type-names
   (compute-types
    #s(identifier :name "w")
    (type-analyze (parse "
     x = 50;
     x = y;
     y = 'str';
     y = z;
     z = new Array;
     z = w;
     w = new Object;
     w = x;"))))
  ("Array" "Number" "Object" "String"))

(deftest type-analysis/cycle/2 :notes type-analysis
  (length
   (jw::location-node-assignments
    (gethash "x" (type-analyze (parse "
                     x = 50;
                     x = y;
                     y = 'str';
                     y = z;
                     z = new Array;
                     z = w;
                     w = new Object;
                     w = x;")))))
  4)

(deftest type-analysis/simple-binary/1 :notes type-analysis
  (type-names
   (compute-types #s(binary-operator :left-arg #s(numeric-literal :value 3)
                                     :right-arg #s(numeric-literal :value 10)
                                     :op-symbol :add)
                  (type-analyze nil)))
  ("Number"))

(deftest type-analysis/simple-binary/2 :notes type-analysis
  (type-names
   (compute-types #s(binary-operator :left-arg #s(string-literal :value "ten")
                                     :right-arg #s(numeric-literal :value 10)
                                     :op-symbol :add)
                  (type-analyze nil)))
  ("Number" "String"))

(deftest type-analysis/simple-binary/3 :notes type-analysis
  (type-names
   (compute-types #s(binary-operator :left-arg #s(identifier :name "x")
                                     :right-arg #s(numeric-literal :value 10)
                                     :op-symbol :add)
                  (type-analyze (parse "var x = 10;"))))
  ("Number"))

(deftest type-analysis/simple-binary/4 :notes type-analysis
  (type-names
   (compute-types #s(binary-operator :left-arg #s(identifier :name "x")
                                     :right-arg #s(numeric-literal :value 10)
                                     :op-symbol :add)
                  (type-analyze (parse "var x = 'ten';"))))
  ("Number" "String"))

(deftest type-analysis/simple-binary/5 :notes type-analysis
  (type-names
   (compute-types #s(binary-operator :left-arg #s(identifier :name "x")
                                     :right-arg #s(string-literal :value "fifteen")
                                     :op-symbol :add)
                  (type-analyze (parse "var x = 'ten';"))))
  ("String"))

(deftest type-analysis/simple-unary/1 :notes type-analysis
  (type-names
   (compute-types #s(unary-operator :arg #s(identifier :name "nonExisto")
                                    :op-symbol :logical-not)
                  (type-analyze nil)))
  ("Boolean"))

(deftest type-analysis/simple-unary/2 :notes type-analysis
  (type-names
   (compute-types #s(unary-operator :arg #s(identifier :name "nonExisto")
                                    :op-symbol :bitwise-not)
                  (type-analyze nil)))
  ("Number"))

(deftest type-analysis/simple-prototype/1 :notes type-analysis
  (type-names
   (compute-types #s(property-access :target #s(identifier :name "x")
                                     :field #s(string-literal :value "foo"))
                  (type-analyze (parse "
        function MyType() {}
        MyType.prototype.foo = 100;
        var x = new MyType();"))))
  ("Number"))

(deftest type-analysis/separate-object-values/1 :notes type-analysis
  (type-names
   (compute-types #s(property-access :target #s(identifier :name "y")
                                     :field #s(string-literal :value "foo"))
                  (type-analyze (parse "
        var x = new Object;
        var y = new Object;
        x.foo = 'str';
        y.foo = 42;"))))
  ("Number"))

(deftest type-analysis/this-context/1 :notes type-analysis
  (type-names
   (compute-types #s(property-access :target #s(identifier :name "x")
                                     :field #s(string-literal :value "bar"))
                  (type-analyze (parse "
        function MyType()
        {
          this.bar = /bar/gi;
        }
        var x = new MyType();"))))
  ("RegExp"))

(deftest type-analysis/this-context/2 :notes type-analysis
  (type-names
   (compute-types #s(property-access :target #s(identifier :name "x")
                                     :field #s(string-literal :value "bar"))
                  (type-analyze (parse "
        function MyType()
        {
          this.bar = /bar/gi;
        }
        function AnotherType() {}
        AnotherType.prototype.baz = MyType;
        var x = new AnotherType();
        x.baz();"))))
  ("RegExp"))

(deftest type-analysis/this-context/3 :notes type-analysis
  (type-names
   (let ((graph (type-analyze (parse "
        function foo()
        {  }
        var x = new Object;
        x.mtd = foo;
        this.bar = 100;
        x.bar = 'shfifty';
        x.mtd();
        "))))
     (compute-types #s(property-access :target #s(special-value :symbol :this)
                                       :field #s(string-literal :value "bar"))
                    graph)))
  ("Number"))

(deftest type-analysis/this-context/4 :notes type-analysis
  (type-names
   (let ((graph (type-analyze (parse "
        function foo()
        {  }
        var x = new Object;
        x.mtd = foo;
        this.bar = 100;
        x.bar = 'shfifty';
        x.mtd();
        "))))
     (compute-types #s(property-access :target #s(special-value :symbol :this)
                                       :field #s(string-literal :value "bar"))
                    graph
                    (jw::find-value-node graph "foo"))))
  ("String"))

(deftest type-analysis/this-context/5 :notes type-analysis
  (type-names
   (let ((graph (type-analyze (parse "
        function foo()
        {  }
        var x = new Object;
        x.mtd = foo;
        this.bar = 100;
        x.bar = 'shfifty';
        x.mtd();

        foo = function() { this.bar = /shmee/ig; };
        "))))
     (compute-types #s(property-access :target #s(special-value :symbol :this)
                                       :field #s(string-literal :value "bar"))
                    graph
                    "foo")))
  ("RegExp" "String"))
                  
(deftest type-analysis/switch-statement/1 :notes type-analysis
  (type-names
   (compute-types #s(identifier :name "x")
                  (type-analyze (parse "
        var x = 20;
        switch(y)
        {
          case 10:
            x = 'a';
            break;
          case 20:
            x = null;
            break;
          default:
            y = 10;
            break;
        }"))))
  ("Number" "String" "null"))

(deftest type-analysis/switch-statement/2 :notes type-analysis
  (type-names
   (compute-types #s(identifier :name "y")
                  (type-analyze (parse "
        function top()
        {
          var y = null;
          switch(y = 'c')
          {
            case 'c':
            return true;
          }
        }"))))
  ("String" "null"))

(deftest type-analysis/function_continuation/1 :notes type-analysis
  (type-names
   (compute-types #s(identifier :name "x")
                  (type-analyze (parse "
        var b = null;
        function id(a)
        {
          b = function_continuation;
          return a;
        }
        var x = id('str');
        if(x != 100)
          resume b <- 100;"))))
  ("Number" "String"))

(deftest type-analysis/function_continuation/2 :notes type-analysis
  (type-names
   (compute-types #s(identifier :name "b")
                  (type-analyze (parse "
        var b = null;
        function id(a)
        {
          b = function_continuation;
          return a;
        }
        var x = id('str');
        if(x != 100)
          resume b <- 100;"))))
  ("$continuation" "null"))
        
(deftest type-analysis/function_continuation/3 :notes type-analysis
  (let ((graph (type-analyze (parse "
        function foo()
        {
          return 30;
        }"))))
    (type-names
     (compute-types #s(special-value :symbol :function_continuation)
                    graph
                    (jw::find-value-node graph "foo"))))
  ("$continuation"))

(deftest type-analysis/fn-call/too-few-args/1 :notes type-analysis
  (type-names
   (compute-types #s(identifier :name "x")
                  (type-analyze (parse "
        function foo(x)
        {
          return x + 2;
        }
        foo(10);
        foo();"))))
  ("Number" "undefined"))

(deftest type-analysis/fn-call/too-many-args/1 :notes type-analysis
  (type-names
   (compute-types #s(identifier :name "x")
                  (type-analyze (parse "
        function foo(x)
        {
          return x + 2;
        }
        foo(10);
        foo(88, 'str');"))))
  ("Number"))

(deftest type-analysis/with-statement/1 :notes type-analysis
  (type-names
   (compute-types #s(identifier :name "x")
                  (type-analyze (parse "
        var bar = null;
        function extract(obj)
        {
          with(obj)
          {
            return bar;
          }
        }

        var x = extract({bar: 10});
        x = extract({baz: 'str'});"))))
  ("Number" "null"))
(flag-expected-failure 'type-analysis/with-statement/1)

(deftest type-analysis/try-catch/1 :notes type-analysis
  (type-names
   (compute-types #s(identifier :name "x")
                  (type-analyze (parse "
        function foo(num)
        {
          if(num)
            throw 100;
          else
            return 'str';
        }

        var x;
        try
        {
          x = foo(true);
        }
        catch(e)
        {
          x = e;
        }"))))
  ("Number" "String" "undefined"))
(flag-expected-failure 'type-analysis/try-catch/1)
    

(defun %make-property-cycle (&optional (n 1000))
  (append (parse "x0.foo = x1; x0 = x1;")
                      (loop for idx from 1 upto (1- n)
                            append (parse (format nil "x~D=new Object;x~D.foo~D = x~D; x~D = x~D;" idx idx idx (1+ idx) idx (1+ idx))))
                      (parse (format nil "x~D = x0; x~D.foo = x0;" n n))))
