;;;; test-cps-transformation.lisp
;;;
;;; Tests for the cps transformation
(in-package :jwacs-tests)

;;;;= Test categories =
(defnote cps "tests for the cps transformation")

;;;;= Tests =
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
                                                        :right-arg #s(numeric-literal :value 1))))))))))))

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
      }"))
  
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
