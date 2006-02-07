;;;; type-analysis.lisp
;;;
;;; Defines functions and data structures for static type analysis
;;; on jwacs source code.
(in-package :jwacs)

;;; ======================================================================
;;;; Graph data types and utilities

(defparameter *type-graph* nil
  "Hash table from value-key to value-node.  A value-node is a node
   of the type graph, with edges to all the value-nodes that it is
   assigned to.")
;;TODO Define a type-key more completely

(defstruct type-graph-node
  "A node in the type graph"
  name
  properties      ; Assoc list of (string . node), with '* as a special case
  return-node)    ; Value-node

(defstruct (value-node (:include type-graph-node))
  "A type graph node that represents a value (eg variable, return
   value, parameter, intermediate value)"
  assignments     ; List of type-graph-nodes
  arguments       ; Assoc list of (index . node)
  min-call-arity) ; The smallest numbers of arguments that this function has ever been called with
  
(defstruct (type-node (:include type-graph-node))
  "A node in the type graph that represents a type (eg, FunctionFoo, number)"
  parameters)     ; List of value-nodes

(defun get-value-node (name)
  "Return the value node named NAME from *TYPE-GRAPH*, creating it
   if necessary"
  (multiple-value-bind (node found-p)
      (gethash name *type-graph*)
    (if found-p
        node
        (setf (gethash name *type-graph*)
              (make-value-node :name name)))))

(defun get-node-property (node name)
  "Return the value node pointed to by NODE's NAME property, creating
   it if necessary"
  (aif (assoc name (type-graph-node-properties node) :test 'equal)
    (cdr it)
    (let ((new-cell (cons name (get-value-node (gensym (format nil "prop$~A" name))))))
      (push new-cell (type-graph-node-properties node))
      (cdr new-cell))))

(defun get-node-argument (node index)
  "Return the value node pointed to by NODE's INDEXth argument, creating it
   if necessary"
  (aif (assoc index (value-node-arguments node) :test 'eql)
    (cdr it)
    (let ((new-cell (cons index (get-value-node (gensym (format nil "~A$~A$arg"
                                                                (type-graph-node-name node) index))))))
      (push new-cell (value-node-arguments node))
      (cdr new-cell))))

(defun get-return-node (node)
  "Returns the return-node of type-graph-node NODE, creating it if necessary"
  (aif (type-graph-node-return-node node)
    it
    (let ((new-return-node (get-value-node (gensym (format nil "~A$ret" (type-graph-node-name node))))))
      (setf (type-graph-node-return-node node) new-return-node)
      new-return-node)))

(defun find-type-node-named (name node-list)
  "If NODE-LIST contains a type-node named NAME, returns it."
  (find name node-list
            :key (lambda (node)
                   (when (type-node-p node)
                     (type-node-name node)))
            :test 'equal))

(defun add-assignment-edge (left-node right-node)
  "Add an assignment edge from LEFT-NODE to RIGHT-NODE in *TYPE-GRAPH*
   if no such edge already exists"
  (assert (value-node-p left-node))
  (pushnew right-node (value-node-assignments left-node)))

(defun get-type-node (name)
  "Return the type node named NAME from *TYPE-GRAPH*, creating it
   if necessary."
  (let ((value-node (get-value-node name)))
    (aif (find-type-node-named name (value-node-assignments value-node))
      it
      (let ((new-type-node (make-type-node :name name)))
        (add-assignment-edge value-node new-type-node)
        new-type-node))))

(defun min* (left right)
  "Returns the minimum of LEFT and RIGHT.  Either or both arguments may
   be NIL.  NIL is treated as being larger than any other value."
  (cond
    ((null left)
     right)
    ((null right)
     left)
    (t
     (min left right))))

;;; ======================================================================
;;;; INTERNAL-ANALYZE generic function

(defgeneric internal-analyze (elm)
  (:documentation
   "Performs an analysis of the source element ELM and makes appropriate
    updates to *TYPE-GRAPH*.  Returns a value-node that represents the type
    of ELM when possible."))

(defparameter *innermost-function-node* nil
  "The type-node of the innermost function decl, if any")

(defmethod internal-analyze ((elm-list list))
  (loop for elm in elm-list
        do (internal-analyze elm)))

(defmethod internal-analyze ((elm source-element))
  (loop for slot in (structure-slots elm)
        do (internal-analyze (slot-value elm slot))))

(defmethod internal-analyze (elm)
  nil)

(defmethod internal-analyze ((elm identifier))
  (get-value-node (identifier-name elm)))

(defmethod internal-analyze ((elm string-literal))
  (get-type-node "String"))

(defmethod internal-analyze ((elm re-literal))
  (get-type-node "RegExp"))

(defmethod internal-analyze ((elm numeric-literal))
  (get-type-node "Number"))

(defmethod internal-analyze ((elm special-value))
  (ecase (special-value-symbol elm)
    (:this ;TODO
     nil)
    ((:false :true)
     (get-type-node "Boolean"))
    (:null
     (get-type-node "null"))
    (:undefined
     (get-type-node "undefined"))))

(defmethod internal-analyze ((elm binary-operator))
  (let ((left-node (internal-analyze (binary-operator-left-arg elm)))
        (right-node (internal-analyze (binary-operator-right-arg elm))))
    (case (binary-operator-op-symbol elm)
      ((:assign :plus-equals
        :and-equals :xor-equals :or-equals)
       (add-assignment-edge left-node right-node)
       left-node)

      ((:times-equals :divide-equals :mod-equals :minus-equals
        :lshift-equals :rshift-equals :urshift-equals)
       (add-assignment-edge left-node (get-type-node "Number"))
       left-node)

      ((:multiply :divide :modulo :subtract)
       (get-type-node "Number"))

      ((:equals :strict-equals :not-equals :strict-not-equals)
       (get-type-node "Boolean"))

      (otherwise
       (let ((expr-node (get-value-node (gensym "expr"))))
         (add-assignment-edge expr-node left-node)
         (add-assignment-edge expr-node right-node)
         expr-node)))))

(defmethod internal-analyze ((elm var-decl))
  (let ((left-node (get-value-node (var-decl-name elm))))
    (if (var-decl-initializer elm)
      (add-assignment-edge left-node (internal-analyze (var-decl-initializer elm)))
      (add-assignment-edge left-node (get-type-node "undefined")))))

(defmethod internal-analyze ((elm fn-call))
  (let* ((target-node (internal-analyze (fn-call-fn elm)))
         (ret-node (get-return-node target-node)))

    (setf (value-node-min-call-arity target-node)
          (min* (value-node-min-call-arity target-node)
                (length (fn-call-args elm))))

    (loop for arg in (fn-call-args elm)
          for idx upfrom 0
          do (add-assignment-edge (get-node-argument target-node idx)
                                  (internal-analyze arg)))

    ret-node))

(defmethod internal-analyze ((elm function-decl))
  (let ((*innermost-function-node* (get-type-node (function-decl-name elm))))
    ;; Redefining functions is legal, but probably not what we wanted
    (unless (null (type-node-parameters *innermost-function-node*))
      (warn "Type-analysis encountered function ~A multiple times" (function-decl-name elm)))

    (loop for param in (function-decl-parameters elm)
          collect (get-value-node param) into param-list
          finally (setf (type-node-parameters *innermost-function-node*)
                        (nconc param-list (type-node-parameters *innermost-function-node*))))
    
    (internal-analyze (function-decl-body elm))))

(defmethod internal-analyze ((elm function-expression))
  (let ((*innermost-function-node* (aif (function-expression-name elm)
                                     (get-type-node it)
                                     (get-type-node (gensym "function")))))
    (loop for param in (function-expression-parameters elm)
          collect (get-value-node param) into param-list
          finally (setf (type-node-parameters *innermost-function-node*)
                        param-list))

    (internal-analyze (function-expression-body elm))
    *innermost-function-node*))

(defmethod internal-analyze ((elm return-statement))
  (if *innermost-function-node*
    (add-assignment-edge (get-return-node *innermost-function-node*)
                         (internal-analyze (return-statement-arg elm)))
    (error "Type-analysis found a return statement at topmost scope")))
   
;;HERE              
;;TODO all the other source-element types (primarily the expressions)

;;; ======================================================================
;;;; Interface functions

(defun type-analyze (elm)
  "Perform type analysis on ELM and return the corresponding type-map."
  (let ((*type-graph* (make-hash-table :test 'equal)))
    (internal-analyze elm)
    (collapse-function-calls)
    *type-graph*))

(defun find-node (name type-map)
  "Find the value node named NAME in TYPE-MAP."
  (gethash name type-map))

(defun find-type (name type-map)
  "Return the type named NAME from TYPE-MAP"
  (awhen (find-node name type-map)
    (find-type-node-named name (value-node-assignments it))))

(defgeneric compute-types (expression-elm type-map)
  (:documentation
   "Return all possible types for the expression represented by EXPRESSION-ELM
    based upon the analysis recorded in TYPE-MAP"))

(defmethod compute-types ((elm identifier) type-map)
  (compute-node-types (find-node (identifier-name elm) type-map) type-map))

;;; ======================================================================
;;;; COMPUTE-NODE-TYPES generic function

(defstruct node-history
  "A container for a list of already-visited nodes"
  node-list)

(defgeneric compute-node-types (node type-map &optional node-history)
  (:documentation
   "Return all the possible types for NODE based upon TYPE-MAP"))

(defmethod compute-node-types :around (node type-map &optional (node-history (make-node-history)))
  (unless (member node (node-history-node-list node-history))
    (push node (node-history-node-list node-history))
    (call-next-method)))

(defmethod compute-node-types ((node value-node) type-map &optional (node-history (make-node-history)))
  (mapcan (lambda (node)
            (compute-node-types node type-map node-history))
          (value-node-assignments node)))

(defmethod compute-node-types ((node type-node) type-map &optional (node-history (make-node-history)))
  (list node))

(defmethod compute-node-types ((node null) type-map &optional (node-history (make-node-history)))
  (declare (ignore type-map))
  nil)

;;; ======================================================================
;;;; COLLAPSE-FUNCTION-CALLS function and supporting generic function

(defun collapse-function-calls ()
  "Add extra edges between return nodes and argument/parameter nodes in *TYPE-GRAPH*"
  (let ((history (make-node-history)))
    (loop for node being each hash-value of *type-graph*
          do (collapse-function-calls-walk node history nil nil nil))))

(defgeneric collapse-function-calls-walk (node node-history
                                               env-rets env-args env-min)
  (:documentation
   "Visits type-graph-node NODE and its assignment-descendents and adds links between
    return nodes of caller and callees, and between corresponding argument/parameter nodes.
    ENV-RETS is a list of return nodes of ancestors.
    ENV-ARGS is a list of assoc-cells (INDEX . NODE) of ancestor arguments nodes.  Note that
    there may be more than one entry per index in ENV-ARGS, so don't actually use ASSOC on it.
    ENV-MIN is the minimum of all ancestor nodes' MIN-CALL-ARITY."))

(defmethod collapse-function-calls-walk :around (node node-history env-rets env-args env-min)
  (unless (member node (node-history-node-list node-history))
    (push node (node-history-node-list node-history))
    (call-next-method)))

(defmethod collapse-function-calls-walk ((node value-node) node-history env-rets env-args env-min)
  (let ((own-rets (aif (value-node-return-node node)
                    (cons it env-rets)
                    env-rets))
        (own-args (append (value-node-arguments node)
                          env-args))
        (own-min (min* (value-node-min-call-arity node) env-min)))
    (loop for descendent in (value-node-assignments node)
          do (collapse-function-calls-walk descendent node-history own-rets own-args own-min))))

(defmethod collapse-function-calls-walk ((node type-node) node-history env-rets env-args env-min)
  (when-let (own-ret (type-node-return-node node))
    (loop for caller-ret in env-rets
          do (add-assignment-edge caller-ret own-ret)))

  ;; TODO Currently O(n^2) in the number of env-args.
  (loop for param in (type-node-parameters node)
        for idx upfrom 0
        do (when (and env-min (>= idx env-min))
             (add-assignment-edge param (get-type-node "undefined")))
           (loop for cell in env-args
                 when (= idx (car cell))
                 do (add-assignment-edge param (cdr cell)))))
                      