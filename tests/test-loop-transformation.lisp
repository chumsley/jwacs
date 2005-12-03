(in-package :jwacs-tests)

(defnote loop-to-function "tests for loop-to-function source transformation")

(deftest loop-to-function/while/1 :notes loop-to-function
  (with-fresh-genvar
    (transform 'loop-to-function
               (parse "x=0; while(x<4) { foo(); x++; }")))
  #.(parse "x=0; 
            {
            var JW0 = function JW1() {
              if(x<4) {
                foo();
                x++;
                JW1();
              }
            };         
           JW0();
           }"))

(deftest loop-to-function/break/1 :notes loop-to-function
  (with-fresh-genvar
    (transform 'loop-to-function
               (parse "x=0;
                       while(x<4)
                       {
                         var y = foo(x++);
                         if(y == null)
                           break;
                       }")))
  #.(parse "x=0;
            {
              var JW0 = function JW1() {
                if(x < 4)
                {
                  var y = foo(x++);
                  if(y == null)
                    return;
                }
              };
            }"))
                  
(deftest loop-to-function/do-while/1 :notes loop-to-function
 (with-fresh-genvar
   (transform 'loop-to-function
	      (parse "x=0; do { foo(); x++; } while(x<4);")))
 #.(parse "x=0;
           {
           var JW0 = function JW1() {
             foo();
             x++;
             if(x<4) {
               JW1();
             }
           };
           JW0();
           }"))

(deftest loop-to-function/for/1 :notes loop-to-function
  (with-fresh-genvar
    (transform 'loop-to-function
               (parse "for(x=0; x<4; x++) { foo(); }")))
  #.(parse "{
             x=0;           
           var JW0 = function JW1() {
              if(x<4) {
               foo();
               x++;
               JW1();
              }
           };
           JW0();           
         }"))

(deftest loop-to-function/for-in/1 :notes loop-to-function
 (with-fresh-genvar
   (transform 'loop-to-function
	      (parse "for(var x in obj) 
                      {
                        foo();
                      }")))
 #.(parse " {
  var JW0 = new Array;
  var JW1 = 0;
  for(var JW2 in obj)
  {
    JW0[JW1++] = JW2;
  }
  var JW3 = function JW4(JW5) { if(JW5 < JW0.length)
  {
    var x = JW0[JW5++];
    
    foo();
    
    JW4(JW5);
  } };
  JW3(0);
}"))

(deftest loop-to-function/break/1 :notes loop-to-function
  (with-fresh-genvar
    (transform 'loop-to-function
               (parse "x=0;
                       while(x<4)
                       {
                         var y = foo(x++);
                         if(y == null)
                           break;
                       }")))
  #.(parse "x=0;
            {
              var JW0 = function JW1() {
                if(x < 4)
                {
                  var y = foo(x++);
                  if(y == null)
                    return;
                }
              };
            }"))
                  
(deftest loop-to-function/continue/1 :notes loop-to-function
  (with-fresh-genvar
    (transform 'loop-to-function
               (parse "x=0;
                       while(x<4)
                       {
                         var y = foo(x++);
                         if(y == null)
                           continue;
                       }")))
  #.(parse "x=0;
            {
              var JW0 = function JW1() {
                if(x < 4)
                {
                  var y = foo(x++);
                  if(y == null)
                    JW1();
                }
              };
            }"))

(deftest loop-to-function/continue/2 :notes loop-to-function
  (with-fresh-genvar
    (transform 'loop-to-function
               (parse "for(x=0; x<4; x++) { if(bar()) continue; foo(); }")))
  #.(parse "{
              x=0;           
              var JW0 = function JW1() {
                if(x<4) {
                  if(bar()) {
                    x++;
                    JW1();
                  }
                  foo();
                  x++;
                  JW1();
                }
              };
              JW0();           
            }"))

(deftest loop-to-function/continue/3 :notes loop-to-function
  (with-fresh-genvar
    (transform 'loop-to-function
               (parse "x=0; do { if(foo()) continue; x++; } while(x<4);")))
  #.(parse "x=0;
            {
              var JW0 = function JW1() {
                if(foo())
                  if(x<4)
                    JW1();
                  else
                    return;
                x++;
                if(x<4) {
                  JW1();
                }
              };
              JW0();
            }"))