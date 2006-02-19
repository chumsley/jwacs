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
;;TODO Clarify (in the graph) the difference between types and constructor functions

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

(defun find-node-property (node name)
  "Return the value node pointed to by NODE's NAME property if
   one already exists, or NIL otherwise"
  (cdr (assoc name (type-graph-node-properties node) :test 'equal)))

(defun get-node-property (node name)
  "Return the value node pointed to by NODE's NAME property, creating
   it if necessary"
  (aif (find-node-property node name)
    it
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
;        (add-assignment-edge (get-node-property new-type-node "prototype") new-type-node)
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

;;TODO Rename to BUILD-TYPE-GRAPH
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
    (:this ;TODO - this deals properly with "declared inside function" but not "set to prototype fields" methods
     *innermost-function-node*)
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
    (unless (or (null (type-node-return-node *innermost-function-node*))
                (null (type-node-parameters *innermost-function-node*)))
      (warn "Type-analysis encountered function ~A multiple times" (function-decl-name elm)))

    (loop for param in (function-decl-parameters elm)
          collect (get-value-node param) into param-list
          finally (setf (type-node-parameters *innermost-function-node*)
                        (nconc param-list (type-node-parameters *innermost-function-node*))))
    
    (internal-analyze (function-decl-body elm))))

(defmethod internal-analyze ((elm function-expression))
  (let ((*innermost-function-node* (get-type-node (aif (function-expression-name elm)
                                                    it
                                                    (gensym "function-expression")))))
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
   
(defun compute-property-name (field-elm)
  (if (or (string-literal-p field-elm)
          (numeric-literal-p field-elm))
    (slot-value field-elm 'value)
    'any))

(defmethod internal-analyze ((elm property-access))
  (let ((target-node (internal-analyze (property-access-target elm)))
        (field-elm (property-access-field elm)))

    (internal-analyze field-elm)
    (get-node-property target-node (compute-property-name field-elm))))
    
(defmethod internal-analyze ((elm new-expr))
  (when (identifier-p (new-expr-object-name elm))
    (get-type-node (identifier-name (new-expr-object-name elm))))
  (internal-analyze (new-expr-object-name elm)))

(defmethod internal-analyze ((elm object-literal))
  (let ((prop-cells (mapcar (lambda (cell)
                              (cons (if (identifier-p (car cell))
                                      (identifier-name (car cell)) ;TODO Temporary HACK to get around the fact that we use identifiers instead of strings as the keys for object literals
                                      (compute-property-name (car cell)))
                                    (internal-analyze (cdr cell))))
                            (object-literal-properties elm)))
        (literal-node (get-type-node (gensym "object-literal"))))

    (setf (type-node-properties literal-node)
          prop-cells)
    literal-node))

;;TODO all the other source-element types (primarily the expressions)

;;; ======================================================================
;;;; Interface functions

(defun type-analyze (elm)
  "Perform type analysis on ELM and return the corresponding type-map."
  (let ((*type-graph* (make-hash-table :test 'equal)))
    (internal-analyze elm)

    (loop with node-history = (make-node-history)
          for node being each hash-value of *type-graph*
          do (collapse-type-graph node (make-node-history) nil nil nil nil))
    
    *type-graph*))

(defun find-node (name type-map)
  "Find the value node named NAME in TYPE-MAP."
  (gethash name type-map))

(defun find-type (name type-map)
  "Return the type named NAME from TYPE-MAP"
  (awhen (find-node name type-map)
    (find-type-node-named name (value-node-assignments it))))

(defun compute-types (expression-elm type-map)
  "Return all possible types for the expression represented by EXPRESSION-ELM
   based upon the analysis recorded in TYPE-MAP"
  (multiple-value-bind (root-node prop-stack)
      (find-root-for-expression expression-elm nil type-map)
    (compute-node-types root-node prop-stack type-map (make-node-history))))

;; TODO maybe this wants to take arguments indicating how to find nodes, one of which
;; adds to the hash and one of which does not.  That way we can combine the duplicate
;; functionality between here and INTERNAL-ANALYZE.
(defgeneric find-root-for-expression (expression-elm prop-stack type-map)
  (:documentation
   "Find (or construct) an appropriate root node for determining the possible types
    of the expression represented by EXPRESSION-ELM by walking TYPE-MAP.
    First value is root nodes, second value is a stack of property names."))

(defmethod find-root-for-expression ((elm identifier) prop-stack type-map)
  (values (find-node (identifier-name elm) type-map)
          prop-stack))

(defmethod find-root-for-expression ((elm property-access) prop-stack type-map)
  (with-slots (target field) elm
    (let ((prop-name (compute-property-name field)))
      (multiple-value-bind (target-node own-prop-stack)
          (find-root-for-expression (property-access-target elm) (cons prop-name prop-stack) type-map)
        (values target-node own-prop-stack)))))

;;; ======================================================================
;;;; COMPUTE-NODE-TYPES generic function

(defstruct node-history
  "A container for a list of already-visited nodes"
  node-list)

(defgeneric compute-node-types (node prop-stack type-map node-history)
  (:documentation
   "Return all the possible types for NODE based upon TYPE-MAP.
    PROP-STACK contains a stack of properties to follow as well, if any."))

(defmethod compute-node-types :around (node prop-stack type-map node-history)
  (unless (member node (node-history-node-list node-history))
    (push node (node-history-node-list node-history))
    (call-next-method)))

(defmethod compute-node-types ((node value-node) prop-stack type-map node-history)
  (let ((simple-recursion (remove-duplicates
                           (mapcan (lambda (n)
                                    (compute-node-types n prop-stack type-map node-history))
                                  (value-node-assignments node)))))

    (aif (and (consp prop-stack)
              (find-node-property node (car prop-stack)))
      (union (compute-node-types it (cdr prop-stack) type-map (make-node-history))
              simple-recursion)
      simple-recursion)))

(defmethod compute-node-types ((node type-node) prop-stack type-map node-history)
  (aif (and (consp prop-stack)
            (find-node-property node (car prop-stack)))
    (compute-node-types it (cdr prop-stack) type-map (make-node-history))
    (list node)))

(defmethod compute-node-types ((node null) prop-stack type-map node-history)
  (declare (ignore prop-stack type-map node-history))
  nil)

;;; ======================================================================
;;;; COLLAPSE-TYPE-GRAPH generic function

(defgeneric collapse-type-graph (node node-history
                                      env-rets env-args env-min
                                      env-props)
  (:documentation
   "Informally, this function collects argument, property, and return nodes down assignment
    paths and deposits them at each type-node that it encounters.

    ENV-RETS is a list of return nodes of ancestors.
    ENV-ARGS is a list of assoc-cells (INDEX . NODE) of ancestor arguments nodes.  Note that
    there may be more than one entry per index in ENV-ARGS, so don't actually use ASSOC on it.
    ENV-MIN is the minimum of all ancestor nodes' MIN-CALL-ARITY.

    ENV-PROPS is a list of assoc-cells (PROP-NAME . NODE).  Note that there
    may be more than one cell for a given property name, so it's not safe
    to use ASSOC.

    The return value is a list of all the type-nodes that NODE has an assignment-path to."))

(defmethod collapse-type-graph :around ((node value-node) node-history
                                        env-rets env-args env-min
                                        env-props)
  (cond
    ((member node (node-history-node-list node-history))
     (if (value-node-p node)
       (value-node-assignments node)
       (list node)))
    (t
     (push node (node-history-node-list node-history))
     (call-next-method))))
                                      
(defmethod collapse-type-graph ((node value-node) node-history
                                      env-rets env-args env-min
                                      env-props)
  (let ((own-rets (aif (value-node-return-node node)
                    (cons it env-rets)
                    env-rets))
        (own-args (append (value-node-arguments node)
                          env-args))
        (own-min (min* (value-node-min-call-arity node)
                       env-min))
        (own-props (append (value-node-properties node)
                           env-props)))
    (loop for child in (value-node-assignments node)
          do (collapse-type-graph child node-history
                                   own-rets own-args own-min
                                   own-props))))

(defmethod collapse-type-graph ((node type-node) node-history
                                      env-rets env-args env-min
                                      env-props)

  ;; Return edges
  (let ((own-ret (get-return-node node)))
    (loop for caller-ret in env-rets
          do (add-assignment-edge caller-ret own-ret)))

  ;; Undefined argument handling
  (loop for param in (type-node-parameters node)
        for idx upfrom 0
        when (and (numberp env-min)
                  (>= idx env-min))
        do (add-assignment-edge param (get-type-node "undefined")))

  ;; Link corresponding arguments and parameters
  ;; TODO Deal with worse-than-quadratic nature of this operation, perhaps
  ;; by using an array for parameters instead of a list.
  (loop for (arg-idx . arg-node) in env-args
        do (add-assignment-edge (nth arg-idx (type-node-parameters node)) arg-node))

  ;; Link corresponding properties
  ;; TODO Currently O(n^2); fix by using hash-table for properties
  (loop for (prop-name . prop-node) in env-props
        for own-node = (get-node-property node prop-name)
        do (add-assignment-edge own-node prop-node))

  ;; Process nodes connected by "dotted" (ie, non-assignment) edges
  (loop for prop-cell in (type-node-properties node)
        do (collapse-type-graph (cdr prop-cell) node-history
                                nil nil nil nil))

  (loop for param in (type-node-parameters node)
        do (collapse-type-graph param node-history
                                nil nil nil nil))

  (collapse-type-graph (get-return-node node) node-history
                       nil nil nil nil))

  
;;; ======================================================================
;;;; COLLAPSE-FUNCTION-CALLS generic function

(defgeneric collapse-function-calls (node node-history
                                          env-rets env-args env-min)
  (:documentation
   "Adds extra edges between return nodes an argument/parameter nodes among all of the
    descendant nodes of NODE.
    ENV-RETS is a list of return nodes of ancestors.
    ENV-ARGS is a list of assoc-cells (INDEX . NODE) of ancestor arguments nodes.  Note that
    there may be more than one entry per index in ENV-ARGS, so don't actually use ASSOC on it.
    ENV-MIN is the minimum of all ancestor nodes' MIN-CALL-ARITY."))

(defmethod collapse-function-calls :around (node node-history env-rets env-args env-min)
  (unless (member node (node-history-node-list node-history))
    (push node (node-history-node-list node-history))
    (call-next-method)))

(defmethod collapse-function-calls ((node value-node) node-history env-rets env-args env-min)
  (let ((own-rets (aif (value-node-return-node node)
                    (cons it env-rets)
                    env-rets))
        (own-args (append (value-node-arguments node)
                          env-args))
        (own-min (min* (value-node-min-call-arity node) env-min)))
    (loop for descendent in (value-node-assignments node)
          do (collapse-function-calls descendent node-history own-rets own-args own-min))))

(defmethod collapse-function-calls ((node type-node) node-history env-rets env-args env-min)
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

;;; ======================================================================
;;;; COLLAPSE-PROPERTY-ACCESSES generic function

(defgeneric collapse-property-accesses (node node-history env-props)
  (:documentation
   "Adds edges to ensure that property-nodes of value-nodes always have a
    path to property-nodes of descendant types and vice versa amongst the
    descendant nodes of NODE.
    ENV-PROPS is a list of assoc-cells (PROP-NAME . NODE).  Note that there
    may be more than one cell for a given property name, so it's not safe
    to use ASSOC."))

(defmethod collapse-property-accesses :around (node node-history env-props)
  (unless (member node (node-history-node-list node-history))
    (push node (node-history-node-list node-history))
    (call-next-method)))

(defmethod collapse-property-accesses ((node value-node) node-history env-props)
  (let ((own-props (append (value-node-properties node) env-props)))
    (loop for descendant in (value-node-assignments node)
          do (collapse-property-accesses descendant node-history own-props))))

(defmethod collapse-property-accesses ((node type-node) node-history env-props)
  ;; TODO Currently O(n^2)
  (loop for env-cell in env-props
        for own-node = (get-node-property node (car env-cell))
        do (add-assignment-edge own-node (cdr env-cell))))


;;;; Debugging helper
;;TODO Move this somewhere else
(defun make-dot-graph (type-graph &optional (fname "c:/temp/types.dot"))
  (with-open-file (s fname :direction :output :if-exists :supersede)
    (let ((node-history nil)
          (node-queue (make-array (hash-table-count type-graph) :fill-pointer 0 :adjustable t)))
      (labels ((get-name (node)
                 (substitute #\_ #\-
                             (substitute #\_ #\$
                                         (string
                                          (if (type-node-p node)
                                            (format nil "type_~A" (type-node-name node))
                                            (type-graph-node-name node))))))

               (queue-node (node)
                 (unless (or (null node)
                             (find node node-history)
                             (find node node-queue))
                   (vector-push-extend node node-queue)))
                 
               (print-edge (from-node to-node label)
                 (format s "  ~A -> ~A" (get-name from-node) (get-name to-node))
                 (when label
                   (format s " [style=dashed, label=\"~A\"]" label))
                 (format s ";~%"))

               (print-collection-edges (from-node to-collection-accessor)
                 (loop for cell-or-node in (funcall to-collection-accessor from-node)
                       for to-node = (if (consp cell-or-node)
                                       (cdr cell-or-node)
                                       cell-or-node)
                       for label = (if (consp cell-or-node)
                                     (car cell-or-node)
                                     nil)
                       do
                       (print-edge from-node to-node label)
                       (queue-node to-node)))

               (print-node-edges (node)
                 (unless (find node node-history)
                   (push node node-history)

                   (print-collection-edges node 'type-graph-node-properties)
                   
                   (when (type-node-p node)
                     (print-collection-edges node 'type-node-parameters))

                   (when (value-node-p node)
                     (print-collection-edges node 'value-node-arguments)
                     (print-collection-edges node 'value-node-assignments))
                     
                   (when-let (ret-node (type-graph-node-return-node node))
                     (print-edge node
                                 ret-node
                                 "$ret")
                     (queue-node ret-node)))))

        (format s "digraph {~%  node [shape=box];~%")
        
        (loop for node being each hash-value in type-graph
              do
              (format s "  ~A [shape=ellipse];~%" (get-name node))
              (queue-node node))

        (loop for idx upfrom 0
              while (< idx (fill-pointer node-queue))
              do (print-node-edges (aref node-queue idx)))
      
      (format s "}")))))