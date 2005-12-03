(in-package :jwacs-tests)

(defnote loop-to-function "tests for loop-to-function source transformation")

(deftest while/1 :notes loop-to-function
  (with-fresh-genvars ()
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



(deftest do-while/1 :notes loop-to-function
 (with-fresh-genvars ()
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

(deftest for/1 :notes loop-to-function
 (with-fresh-genvars ()
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


(deftest for-in/1 :notes loop-to-function
 (with-fresh-genvars ()
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