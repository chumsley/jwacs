;;;; loop-transformation.lisp
;;; 
;;
;; LOOP-CANONICALIZE: turn loops into a form more easily EXPLICITIZE'd and CPS'ed
;;
;;  Basically we remove the test into an if statement in the loop, remove any variable declarations inside the loop
;;  and annotate at the correct places with "break" and "continue"
;;
;;
;; We'll also turn everything into a while loop, so we only have one loop type to deal with later on.
;;;
;;; Copyright (c) 2005 Greg Smolyn and James Wright
;;; See LICENSE for full licensing details.
;;;
(in-package :jwacs)

;; ===================================
;; WHILE LOOP

;; This is the most basic loop, and is going to be our base case.
;; Here we move all variable declarations OUT of the while's body (keeping assignment inside).
;; We move the test into an if-break statement inside the body, and we add explicit calls 
;; to break and continue (which makes life easier for the rest of our transformations).


;; while(test) {
;;    var x = rval;
;;    foo();
;; }
;;
;; BECOMES ==>
;;
;; var x;
;; while(true) {
;;   if(!test)
;;     break;
;;   x = rval;
;;   foo();
;;   continue;
;; }


(defmethod transform ((xform (eql 'loop-canonicalize)) (elm while))
  (let ((new-decls (mapcar (lambda (decl)  
                             (make-var-decl :name (var-decl-name decl) :initializer nil))
                           (collect-in-scope (while-body elm) 'var-decl)))
        (new-while (make-while 
                    :label (source-element-label elm)
                    :condition (make-special-value :symbol :true)
                    :body (single-statement
                           (make-if-statement 
                            :condition (make-unary-operator :op-symbol :logical-not :arg (while-condition elm)
                                                            :start (source-element-start (while-condition elm))
                                                            :end (source-element-end (while-condition elm)))
                            :then-statement (make-break-statement :target-label nil))
                           (transform 'loop-canonicalize 
                                      (transform 'loop-canonicalize-in-body 
                                                 (while-body elm)))
                           (make-continue-statement :target-label nil))
                    :start (source-element-start elm)
                    :end (source-element-end elm))))
    (if new-decls
      (combine-statements (make-var-decl-statement :var-decls new-decls) new-while)
      new-while)))


;; ===================================
;; DO-WHILE LOOP
 
;; DO-WHILE loops are converted into straight WHILE loops.
;; An if statement is added to skip the test when this is the first time through the loop.
;; This approach was chosen over using the while test around the continue (ie "if(test) continue;" instead of "if(!test) break;")
;; even though it is uglier, because if there was a label on this loop and a nested loop used a labelled continue, 
;; that labelled continue would also have to have the test around it.
;;
;; In short, it's uglier but it keeps the test in one place.


;; do {
;;  var x = rval;
;;  foo();
;; } while(test);
;;
;; BECOMES ==>
;;
;; var first = true;
;; while(true) {
;;   if(!first) {
;;     if(!test) 
;;      break;
;;     } 
;;   }
;;   else {
;;     first = false;
;;   } 
;;   x=rval;
;;   foo();
;;   continue;
;; }


(defmethod transform ((xform (eql 'loop-canonicalize)) (elm do-statement))
  (let* ((firstp (genvar)) ; firstp is the variable we use to determine if this is the first iteration through the loop
         (new-decls (append (mapcar (lambda (decl)  (make-var-decl :name (var-decl-name decl) :initializer nil))
                                    (collect-in-scope (do-statement-body elm) 'var-decl))
                            (list (make-var-decl :name firstp :initializer (make-special-value :symbol :true)))))
         (new-while (make-while
                     :label (source-element-label elm)
                     :condition (make-special-value :symbol :true)
                     :body (single-statement
                            (make-if-statement
                             :condition (make-unary-operator :op-symbol :logical-not :arg (make-identifier :name firstp))
                             :then-statement (make-statement-block 
                                              :statements (list (make-if-statement
                                                                 :condition (make-unary-operator 
                                                                             :op-symbol :logical-not 
                                                                             :arg (do-statement-condition elm))
                                                                 :then-statement (make-break-statement :label nil))))
                             :else-statement (make-binary-operator
                                              :op-symbol :assign
                                              :left-arg (make-identifier :name firstp)
                                              :right-arg (make-special-value :symbol :false)))

                            (transform 'loop-canonicalize (transform 'loop-canonicalize-in-body (do-statement-body elm)))
                            (make-continue-statement :label nil))
                     :start (source-element-start elm)
                     :end (source-element-end elm))))
    (combine-statements
     (make-var-decl-statement :var-decls new-decls)
     new-while)))

;; ===================================
;; FOR LOOP

;; For loops are converted to while loops as well.
;; This is fairly straight forward, we move the initializer to above the while and the 
;; loop statement to just before the continue;



;; for(var x=0; x<10; x++) {
;;   foo();
;; }
;;
;; ==>
;;
;; var x=0;
;; while(true) {
;;  if(!(x<10)) 
;;   break;
;;  foo();
;;  x++;
;;  continue;
;; }


(defmethod transform ((xform (eql 'loop-canonicalize)) (elm for))
 (let* ((new-decls (mapcar (lambda (decl)  (make-var-decl :name (var-decl-name decl) :initializer nil))
                           (collect-in-scope (for-body elm) 'var-decl)))
        (new-header-statements (cond
                                 ((and (for-initializer elm)
                                       new-decls)
                                  (list (make-var-decl-statement :var-decls new-decls)
                                       (for-initializer elm)))
                                  ((for-initializer elm)
                                   (for-initializer elm))
                                  (t
                                   nil)))
        (new-loop (make-while :label (source-element-label elm)
                              :condition (make-special-value :symbol :true)
                              :body (single-statement
                                     (make-if-statement
                                      :condition (make-unary-operator :op-symbol :logical-not :arg (or (for-condition elm) (make-special-value :symbol :true)))
                                      :then-statement (make-break-statement :target-label nil))
                                     (transform 'loop-canonicalize (transform 'loop-canonicalize-in-body (for-body elm)))
                                     (for-step elm)
                                     (make-continue-statement :target-label nil))
                              :start (source-element-start elm)
                              :end (source-element-end elm))))
   (combine-statements
    new-header-statements
    new-loop))) 



;; =========================
;; Helper transform

;; This transformation is for use in the body of while loops-- it converts all var-decls into
;; regular assignments. 
;; This needs to happen after you've collected any var-decls to be placed before their appropriate looping statements

(defmethod transform ((xform (eql 'loop-canonicalize-in-body)) (elm var-decl-statement))
  (let ((assignments (mapcar (lambda (var-decl) (awhen (var-decl-initializer var-decl)
                                                  (make-binary-operator :op-symbol :assign
                                                                        :left-arg (make-identifier :name (var-decl-name var-decl))
                                                                        :right-arg it
                                                                        :start (source-element-start var-decl)
                                                                        :end (source-element-end var-decl))))
                             (var-decl-statement-var-decls elm))))
    (if (= 1 (length assignments))
      (first assignments)
      (combine-statements assignments))))


;; ===================
;;  FOR-IN LOOP 

;; For-in loops are rather a strange beast.  We can't reproduce the for-in functionality
;; easily (or at all) without using for-in. Ugh.
;;
;; So we keep the for-in, but we remove the body and generate an array in that for-in
;; over which we will apply the body.  Because the for-in is just generating a temporary
;; array, we know this is safe for the CPS transform.
;;
;; Here I'm going to cheat a little and convert the for-in to a for-in and while loop, and then 
;; run the loop-canonicalize on the while loop to get a properly transformed while loop.


;; for(var_x in var_y)
;; {
;;   body;
;; }
;;
;; BECOMES ==>
;;
;; new_array = new Array();
;; new_count=0;
;; for(new_prop in var_y)
;; {
;;   new_array[new_count++]=new_prop;
;; }
;; while(new_count_rec<new_array.length) {
;;   var_x = new_array[new_count_rec++];
;;   body;
;; }
;;
;; BECOMES ==> something that has a loop-canonicalized while. :) 

(defmethod transform ((xform (eql 'loop-canonicalize)) (elm for-in))
  (let* ((new-array (genvar))     ; the array to which we dump the collected data
         (new-count (genvar))     ; a counter for our array-- basically we're just adding a property with a number as its key
         (new-prop (genvar))      ; the iterator var
         (new-count-rec (genvar)) ; the counter for our while loop (ya, ya, could reuse new-count I guess)
         (new-while (make-while :label (source-element-label elm)
                                :condition (make-binary-operator 
                                            :op-symbol :less-than
                                            :left-arg (make-identifier :name new-count-rec)
                                            :right-arg (make-property-access 
                                                        :target (make-identifier :name new-array)
                                                        :field (make-string-literal :value "length")))
                                :body (single-statement
                                       (make-forin-assign elm new-array new-count-rec)
                                       (for-in-body elm))
                                :start (source-element-start elm)
                                :end (source-element-end elm))))
                                                
    (combine-statements
     (make-var-decl-statement 
      :var-decls (list (make-var-decl :name new-array :initializer (make-array-literal :elements nil))
                       (make-var-decl :name new-count :initializer (make-numeric-literal :value 0))
                       (make-var-decl :name new-count-rec :initializer (make-numeric-literal :value 0))))
     (make-for-in 
      :binding (make-var-decl-statement
                :var-decls (list 
                            (make-var-decl :name new-prop :initializer nil)))
      :collection (for-in-collection elm)
      :body (make-statement-block 
             :statements (list (make-binary-operator 
                                :op-symbol :assign
                                :left-arg (make-property-access 
                                           :target (make-identifier :name new-array)
                                           :field (make-unary-operator 
                                                   :op-symbol :post-incr
                                                   :arg (make-identifier :name new-count)))
                                :right-arg (make-identifier :name new-prop)))))
     (transform xform new-while))))
											  

(defun make-forin-assign (elm new-array new-count-rec)
  "Creates the assignment that spoofs our iterating through the for-in collection. Just assigns the orignal iterator var
   from the for-in a value from our new array of values, which we are iterating through using a while loop and the variable
   new-count-rec"
  (let ((prop-access
         (make-property-access :target (make-identifier :name new-array)
                               :field (make-unary-operator :op-symbol :post-incr
                                                           :arg (make-identifier :name new-count-rec))))
        (binding (for-in-binding elm)))
    (cond ((identifier-p binding)
           (make-binary-operator :op-symbol :assign
                                 :left-arg binding
                                 :right-arg prop-access))
          ((var-decl-statement-p binding)
           (let ((vardecl (car (var-decl-statement-var-decls binding))))
             (make-var-init (var-decl-name vardecl) prop-access))))))
