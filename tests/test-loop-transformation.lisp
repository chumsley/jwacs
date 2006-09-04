;;;
;;; Copyright (c) 2005 Greg Smolyn
;;; See LICENSE for full licensing details.
;;;
(in-package :jwacs-tests)

;; canonicalization tests

(defnote loop-canonicalize "tests for canonicalize source transformation")

;; ====================================
;; WHILE LOOPS


(deftest canonicalize/while/basic :notes  loop-canonicalize
  (transform 'loop-canonicalize
               (test-parse "x=0; while(x<4) { foo(); x++; }"))
  #.(test-parse "x=0; while(true) { if(!(x<4)) break; foo(); x++; continue; }"))

(deftest canonicalize/while/var-decl-in-body :notes  loop-canonicalize
    (transform 'loop-canonicalize
               (test-parse "x=0; while(x<4) { var y=0; foo(); x++; }"))
  #.(test-parse "x=0; var y; while(true) { if(!(x<4)) break; y=0; foo(); x++; continue; }"))

(deftest canonicalize/while/nested-while :notes  loop-canonicalize
    (transform 'loop-canonicalize
               (test-parse "while(x<4) { var y=0; foo(); while(y<5) { var z=0; bar(); } }"))
    #.(test-parse "var y,z; while(true) { if(!(x<4)) break; y=0; foo(); while(true) { if(!(y<5)) break; z=0; bar(); continue; } continue; }"))

(deftest canonicalize/while/labelled :notes  loop-canonicalize
    (transform 'loop-canonicalize
               (test-parse "x=0; labelled: while(x<4) { foo(); x++; }"))
  #.(test-parse "x=0; labelled: while(true) { if(!(x<4)) break; foo(); x++; continue; }"))




;; ====================================
;; FOR LOOPS


(deftest canonicalize/for/basic :notes loop-canonicalize
    (transform 'loop-canonicalize
               (test-parse "for(var x=0; x<10; x++) { foo(); }"))
    #.(test-parse "var x=0; while(true) { if(!(x<10)) break; foo(); x++; continue; }"))

(deftest canonicalize/for/labelled :notes loop-canonicalize
    (transform 'loop-canonicalize
               (test-parse "yar: for(var x=0; x<10; x++) { foo(); }"))
    #.(test-parse "var x=0; yar: while(true) { if(!(x<10)) break; foo(); x++; continue; }"))


(deftest canonicalize/for/var-decl-in-body :notes loop-canonicalize
    (transform 'loop-canonicalize 
               (test-parse "for(var x=0; x<10; x++) { var y=0; foo();}"))
    #.(test-parse "var y; var x=0; while(true) { if(!(x<10)) break; y=0; foo(); x++; continue; }"))

(deftest canonicalize/for/single-statement :notes loop-canonicalize
  (with-fresh-genvar
    (transform 'loop-canonicalize
               (test-parse "
        for(var x = 0; x < 10; x++)
          output(x);")))
  #.(test-parse "
        var x = 0;
        while(true)
        {
          if(!(x < 10))
            break;
          output(x);
          x++;
          continue;
        }"))
            
;; ====================================
;; DO-WHILE LOOPS

(deftest canonicalize/do-while/var-decl-in-body :notes loop-canonicalize
  (with-fresh-genvar
    (transform 'loop-canonicalize
               (test-parse "do { var x = rval; foo(); } while(test);")))
    #.(test-parse "
  var x, JW0 = true;
  while(true)
  {
    if(!JW0)
    {
      if(!test)
        break;
    }
    else
      JW0 = false;
    x = rval;
    foo();
    continue;
  }"))

(deftest canonicalize/do-while/labelled :notes loop-canonicalize
  (with-fresh-genvar
    (transform 'loop-canonicalize
               (test-parse "yar: do { var x = rval; foo(); } while(test);")))
    #.(test-parse "
  var x, JW0 = true;
  yar:
  while(true)
  {
    if(!JW0)
    {
      if(!test)
        break;
    }
    else
      JW0 = false;
    x = rval;
    foo();
    continue;
  }"))


;; ====================================
;; FOR-IN LOOPS

(deftest canonicalize/for-in/basic :notes loop-canonicalize
  (with-fresh-genvar
    (transform 'loop-canonicalize
               (test-parse "for(var_x in some_collection) { foo(); }")))
  #.(test-parse "
  var JW0 = [], JW1 = 0, JW3 = 0;
  for(var JW2 in some_collection)
  {
    JW0[JW1++] = JW2;
  }
  while(true)
  {
    if(!(JW3 < JW0.length))
      break;
    var_x = JW0[JW3++];
    foo();
    continue;
  }"))

(deftest canonicalize/for-in/labelled :notes loop-canonicalize
  (with-fresh-genvar
    (transform 'loop-canonicalize
               (test-parse "yar: for(var_x in some_collection) { foo(); }")))
  #.(test-parse "
  var JW0 = [], JW1 = 0, JW3 = 0;
  for(var JW2 in some_collection)
  {
    JW0[JW1++] = JW2;
  }
  yar:
  while(true)
  {
    if(!(JW3 < JW0.length))
      break;
    var_x = JW0[JW3++];
    foo();
    continue;
  }"))

(deftest canonicalize/while/position-preservation/1 :notes  loop-canonicalize
  (let* ((xformed (transform 'loop-canonicalize
                            (parse "x=0; while(x<4) { foo(); x++; }")))
         (foo-call (second (jw::statement-block-statements (jw::while-body (second xformed))))))
    (values (source-element-start foo-call)
            (source-element-end foo-call)))
  18 21)

;; TODO at some point we will want to look very carefully at how the positions are calculated
;; for the condition test and the generated break and continue statements.