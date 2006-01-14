;;;; runtime-transformation.lisp
;;;
;;; Define the transformation that adds in support for and calls into
;;; the dynamic runtime.
(in-package :jwacs)




;;----------------------------------------------------------------------------------------------------
;; XXX For now it's a bit of a dumping ground; I'm just pasting old code into here
;; without any sense for making it actually work.


(defun make-call-style-guard (fn-name parameters)
  "Builds an if statement that checks whether the incoming continuation argument is a function value;
   if it isn't, then (on the assumption that a direct-style call has been mistakenly made) re-calls
   this function (whose name is FN-NAME) with a default continuation parameter (`$id`) followed by
   the original incoming parameters."
  (make-if-statement
   :condition
   (make-binary-operator :op-symbol :not-equals
                         :left-arg (make-unary-operator :op-symbol :typeof
                                                        :arg *cont-id*)
                         :right-arg (make-string-literal :value "function"))
   :then-statement
   (make-return-statement :arg (make-fn-call :fn (make-identifier :name fn-name)
                                             :args (mapcar (lambda (p) (make-identifier :name p))
                                                           (cons *id-name* parameters))))))

(defparameter *function-decls-in-scope* nil
  "A list of names of currently-visible function-decls.  We can use this to
   determine which calls need to be indirected through $call, and which can
   be 'inlined' as unchecked CPS calls.")

(defun function-in-scope-p (name)
  "Return non-NIL if a function-decl with name NAME is currently visible in scope"
  (member name *function-decls-in-scope* :test 'equal))


(defun make-indirected-call (fn-call-elm)
  "Wraps FN-CALL-ELM (which should be a function call) in a call to the runtime
   function `$trampolineResult`, which will determine at runtime whether to generate
   a thunk that makes the call or to make the call directly and return a result."
  (assert (fn-call-p fn-call-elm))
  (make-fn-call :fn *trampolineResult-id*
                :args (list (fn-call-fn fn-call-elm)
                            (make-special-value :symbol :this)
                            (make-array-literal :elements (fn-call-args fn-call-elm)))))
                
