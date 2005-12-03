;;;; loop-transformation.lisp
;;; 
;;;  Implements the transformation of looping constructs (while, do-while, for, for-in) into
;;;  their recursive-function calling equivalents.

(in-package :jwacs)

;; =====================
;; === WHILE LOOP ===
;; * Original
;;
;; while(test)
;; {
;;   body;
;; }
;;
;; * Transformed
;;
;; var while_var = function whileloop()
;; {
;;   if(test)
;;     {
;;       body();
;;       whileloop();
;;     }
;; }
;; while_var();

(defmethod transform ((xform (eql 'loop-to-function)) (elm while))
  (let ((function-var (genvar))
	(function-name (genvar)))
    (make-statement-block 
     :statements
     (list 
      (make-var-decl-statement 
       :var-decls 
       (list
	(make-var-decl 
	 :name function-var
	 :initializer (make-function-expression 
		       :name function-name
		       :parameters nil
		       :body (list (make-if-statement 
				    :condition (while-condition elm)
				    :then-statement (make-statement-block 
						     :statements (append (transform-body xform #'while-body elm)
									 (list (make-fn-call 
										:fn (make-identifier :name function-name)
										:args nil))))
				    :else-statement nil))))))
      (make-fn-call :fn (make-identifier :name function-var) :args nil)))))

		      

(defun transform-body (xform body-fn elm)
  (let* ((transformed-body (transform xform (funcall body-fn elm)))
	 (transformed-stmts (if (statement-block-p transformed-body)
				(statement-block-statements transformed-body)
				transformed-body)))
    (if (listp transformed-stmts)
	transformed-stmts
	(list transformed-stmts))))

;; =====================
;; === DO-WHILE LOOP ===
;; * Original
;;
;; do {
;;   body;
;; } while(test);
;;
;; * Transformed 
;;
;; var dowhile_var = function dowhileloop()
;; {
;;   body();
;;   if(test) {
;;     dowhileloop();
;;   }
;; }
;; dowhile_var();


(defmethod transform ((xform (eql 'loop-to-function)) (elm do-statement))
  (let ((function-var (genvar))
	(function-name (genvar)))
    (make-statement-block 
     :statements 
     (list 
      (make-var-decl-statement 
       :var-decls (list 
		   (make-var-decl 
		    :name function-var
		    :initializer (make-function-expression 
				 :name function-name
				 :parameters nil			       
				 :body  (append (transform-body xform #'do-statement-body elm)
						(list (make-if-statement :condition (do-statement-condition elm)
									 :then-statement (make-statement-block
											  :statements (list 
												       (make-fn-call 
													:fn (make-identifier :name function-name) 
													:args nil)))
									 :else-statement nil)))))))
      (make-fn-call :fn (make-identifier :name function-var) :args nil)))))

							


;; =====================
;; ===   FOR LOOP    ===
;; * Original
;;
;; for(expressions; test; loop_expr) 
;; {
;;   body();
;; }
;;
;; * Transformed
;;
;; expressions;
;; var function_var = function forloop()
;; {
;;   if(test) {
;;     body
;;     loop_expr;
;;     forloop();
;;   }
;; }
;; function_var();

(defmethod transform ((xform (eql 'loop-to-function)) (elm for))
  (let ((function-var (genvar))
	(function-name (genvar)))
    (make-statement-block
     :statements
     (list (for-initializer elm)
	   (make-var-decl-statement 
	    :var-decls (list
			(make-var-decl 
			 :name function-var
			 :initializer (make-function-expression 
				       :name function-name 
				       :parameters nil
				       :body (list (make-if-statement 
						    :condition (for-condition elm)
						    :then-statement (make-statement-block 
								     :statements
								     (append (transform-body xform #'for-body elm)
									     (list (for-step elm))
									     (list (make-fn-call 
										    :fn (make-identifier :name function-name) 
										    :args nil))))))))))
	   (make-fn-call :fn (make-identifier :name function-var) :args nil)))))




;; ===================
;; === FOR-IN LOOP ===
;; * Original
;;
;; for(var_x in var_y)
;; {
;;   body;
;; }
;;
;; * Transformed
;;
;; new_array = new Array();
;; new_count=0;
;; for(new_prop in var_y)
;; {
;;   new_array[new_count++]=new_prop;
;; }
;; function forinloop(new_count_rec)
;; {
;;   if(new_count_rec<new_array.length)
;;     {
;;       var_x = new_array[new_count_rec++];
;;       body;
;;       forinloop(new_count_rec);
;;     }
;; }
;; forinloop(0);

(defmethod transform ((xform (eql 'loop-to-function)) (elm for-in))
  (let ((new-array (genvar))
	(new-count (genvar))
	(new-prop (genvar))
	(function-var (genvar))
	(function-name (genvar))
	(new-count-rec (genvar)))
    (make-statement-block
     :statements
     (list (make-var-decl-statement 
	    :var-decls (list 
			(make-var-decl 
			 :name new-array
			 :initializer (make-new-expr 
				       :object-name (make-identifier :name "Array") 
				       :args nil))))
	   (make-var-decl-statement
	    :var-decls (list 
			(make-var-decl 
			 :name new-count
			 :initializer (make-numeric-literal :value 0))))
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
	   (make-var-decl-statement
	    :var-decls (list 
			(make-var-decl 
			 :name function-var 
			 :initializer (make-function-expression
				       :name function-name
				       :parameters (list new-count-rec)
				       :body (list 
					      (make-if-statement 
					       :condition 
					       (make-binary-operator 
						:op-symbol :less-than
						:left-arg (make-identifier :name new-count-rec)
						:right-arg (make-property-access 
							    :target (make-identifier :name new-array)
							    :field (make-string-literal :value "length")))
					       :then-statement 
					       (make-statement-block 
						:statements (append  
							     (list (make-forin-assign elm new-array new-count-rec))
							     (transform-body xform #'for-in-body elm)
							     (list (make-fn-call :fn (make-identifier :name function-name)
										 :args (list (make-identifier :name new-count-rec))))))))))))
	   (make-fn-call :fn (make-identifier :name function-var)
			 :args (list (make-numeric-literal :value 0)))))))


											  

(defun make-forin-assign (elm new-array new-count-rec)
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
	     (make-var-decl-statement :var-decls (list (make-var-decl :name (var-decl-name vardecl)
								:initializer prop-access)))))
	  (t nil))))
