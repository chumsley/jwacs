;;;; type-analysis.lisp
;;;
;;; Defines functions and data structures for static type analysis
;;; on jwacs source code.
;;;
;;; Copyright (c) 2006 James Wright
;;; See LICENSE for full licensing details.
;;;
(in-package :jwacs)

;;; ======================================================================
;;;; Graph data types and utilities

(defstruct type-graph-node
  "A node in the type graph"
  name
  properties           ; Assoc list of (string . node), with 'ANY as a special case
  assignment-backlinks ; List of location-nodes
  return-node)         ; location-node

(defstruct (location-node (:include type-graph-node))
  "A type graph node that represents a location (eg variable, return
   value, parameter, intermediate value)"
  assignments          ; List of type-graph-nodes
  arguments            ; Assoc list of (index . node)
  this-bindings        ; List of type-graph-nodes
  min-call-arity)      ; The smallest numbers of arguments that this function has ever been called with
  
(defstruct (value-node (:include type-graph-node))
  "A node in the type graph that represents a value (object, function, number, etc.)"
  constructor-name     ; Name of the constructor for this type, if any
  parameters           ; List of location-nodes
  this-context-node    ; location-node
  prototype-node       ; location-node
  continuation-node)   ; location-node

(defun find-location-node (graph name)
  "Return the location node named NAME from GRAPH if one
   already exists, or NIL otherwise"
  (gethash name graph))

(defun get-location-node (graph name &optional queue)
  "Return the location node named NAME from GRAPH, creating it
   if necessary.  If the node is created and QUEUE is non-NIL,
   then the new node will be queued for processing."
  (flet ((maybe-setup-builtin (node)
           ;;TODO This might be the sort of thing that gets declared in a "prelude" of
           ;; type-declarations, but for now we'll ensure that built-in types have their
           ;; constructors set up on demand.
           (when (find name '("Array" "Boolean" "Function" "Number" "Object" "RegExp" "String")
                       :test #'equal)
             (setup-function-node graph node))

           (when (find name '("null" "undefined" global-context) :test #'equal)
             (unless (find-value-node-named name (location-node-assignments node))
               (add-assignment-edge node (make-value-node :name name :constructor-name name) queue)))
           node))
    
    (multiple-value-bind (node found-p)
        (gethash name graph)
      (if found-p
        (maybe-setup-builtin node)
        (let ((node (make-location-node :name name)))
          (when queue
            (enqueue-node queue node))
          (setf (gethash name graph) node)
          (maybe-setup-builtin node))))))

(defun find-node-property (node name)
  "Return the location node pointed to by NODE's NAME property if
   one already exists, or NIL otherwise"
  (cdr (assoc name (type-graph-node-properties node) :test 'equal)))

(defun get-node-property (graph node name &optional queue)
  "Return the location node pointed to by NODE's NAME property, creating
   it in GRAPH if necessary.  If the property node is created, then it
   will be queued for processing if QUEUE is non-NIL."
  (aif (find-node-property node name)
    it
    (let ((new-cell (cons name (get-location-node graph (gensym (format nil "prop$~A" name)) queue))))
      (push new-cell (type-graph-node-properties node))
      (cdr new-cell))))

(defun get-node-argument (graph node index)
  "Return the location node pointed to by NODE's INDEXth argument, creating it
   in GRAPH if necessary"
  (aif (assoc index (location-node-arguments node) :test 'eql)
    (cdr it)
    (let ((new-cell (cons index (get-location-node graph (gensym (format nil "~A$~A$arg"
                                                                      (type-graph-node-name node) index))))))
      (push new-cell (location-node-arguments node))
      (cdr new-cell))))

(defun get-return-node (graph node &optional queue)
  "Returns the return-node of type-graph-node NODE, creating it in GRAPH if necessary.
   If NODE is created then it will be added to QUEUE if QUEUE is non-NIL."
  (aif (type-graph-node-return-node node)
    it
    (let ((new-return-node (get-location-node graph (gensym (format nil "~A$ret" (type-graph-node-name node))) queue)))
      (setf (type-graph-node-return-node node) new-return-node)
      new-return-node)))

(defun get-this-context-node (graph node &optional queue)
  "Returns the this-context-node of value-node NODE, creating it if necessary.
   If a new node is created then it will be added to QUEUE if QUEUE is non-NIL."
  (aif (value-node-this-context-node node)
    it
    (let ((new-this-context-node (get-location-node graph (gensym (format nil "~A$this" (value-node-name node))) queue)))
      (setf (value-node-this-context-node node) new-this-context-node)
      new-this-context-node)))

(defun get-continuation-node (graph node &optional queue)
  "Returns the continuation value-node for value-node NODE, creating it if necessary.
   Creating a continuation node may cause a return-node to be created and queued as
   a side-effect."
  (aif (value-node-continuation-node node)
    it
    (let* ((new-param (get-location-node graph (gensym "k_arg")))
           (new-k-value (make-value-node :name (gensym (format nil "~A_continuation" (value-node-name node)))
                                         :constructor-name "$continuation"
                                         :parameters (list new-param)))
           (new-k-location (make-location-node :name (gensym (format nil "~A_continuation" (value-node-name node))))))
      
      (add-assignment-edge new-k-location
                           new-k-value
                           queue)

      (add-assignment-edge (get-return-node graph node queue)
                           new-param
                           queue)
      (setf (value-node-continuation-node node)
            new-k-location))))

(defun find-value-node-named (name node-list)
  "If NODE-LIST contains a value-node named NAME, returns it."
  (find name node-list
            :key (lambda (node)
                   (when (value-node-p node)
                     (value-node-name node)))
            :test 'equal))

(defun add-assignment-edge (left-node right-node &optional queue)
  "Add an assignment edge from LEFT-NODE to RIGHT-NODE if no such edge already exists.
   If QUEUE is non-NIL, queues LEFT-NODE for further processing."
  (assert (location-node-p left-node))
  (unless (find right-node (location-node-assignments left-node))
    (push right-node (location-node-assignments left-node))
    (push left-node (type-graph-node-assignment-backlinks right-node))
    (when queue
      (enqueue-ancestors queue left-node nil))))

(defun enqueue-ancestors (queue node path)
  "Adds NODE and all of the assignment-ancestors of NODE to QUEUE.
   PATH contains a list of nodes processed so far (used for cycle-detection)."
  (enqueue-node queue node)
  (loop with own-path = (cons node path)
        for parent in (type-graph-node-assignment-backlinks node)
        unless (find parent path)
        do (enqueue-ancestors queue parent own-path)))

(defun find-value-node (graph name)
  "Return the value named NAME from GRAPH"
  (awhen (find-location-node graph name)
    (find-value-node-named name (location-node-assignments it))))

(defun setup-function-node (graph node)
  "Sets up a location-node to point to a value-node of the same name.
   The existence of three nodes will be guaranteed:
     (1) A value-node with an assignment edge from NODE and the same name as NODE
     (2) A location node pointed to by the 'prototype' property of (1)
     (3) A value-node named #:|NAME-prototype| with an assignment edge from (2).

   Returns the function value-node (1)"
  (let* ((name (location-node-name node))
         (function-value (aif (find-value-node-named name (location-node-assignments node))
                           it
                           (make-value-node :name name :constructor-name "Function"))) ;(1)
         (prototype-node (get-node-property graph function-value "prototype"))) ;(2)

    ;; Ensure edge from NODE to (1)
    (add-assignment-edge node function-value)
    
    ;; Ensure (3)
    (when (null (remove-if-not 'value-node-p (location-node-assignments prototype-node)))
      (add-assignment-edge prototype-node
                           (make-value-node :name (gensym (format nil "~A_proto" name))
                                            :constructor-name name)))
    function-value))

(defun get-exemplar-value-nodes (graph constructor-name)
  "Returns a list containing a value-node for an exemplar value
   of a type created by constructor CONSTRUCTOR-NAME (which will
   usually be the name of a built-in type such as Number)"
  (let ((location-node (get-location-node graph constructor-name)))
    (setup-function-node graph location-node)
    (remove-duplicates
     (loop for type-node in (location-node-assignments location-node)
           for prototype-node = (find-node-property type-node "prototype")
           when prototype-node
           append (location-node-assignments prototype-node)))))

(defun get-instance-node (graph constructor &optional queue)
  "Returns a node representing an instance of the type(s) constructed by
   CONSTRUCTOR.  CONSTRUCTOR is either the name of a location-node to
   retrieve from GRAPH, or a node in GRAPH representing the constructor.

   For Object and Array types, a new instance node is
   created.  For all other types, the same 'exemplar node' is used for
   all instances.

   If QUEUE is non-NIL and the constructor node had to be created, then
   the new node will be queued for processing."
  (let* ((ctor-node (if (type-graph-node-p constructor)
                            constructor
                            (get-location-node graph constructor queue)))
         (ctor-name (type-graph-node-name ctor-node)))

    (cond
      ((or (equal "Object" ctor-name)
           (equal "Array" ctor-name))
       
       (make-value-node :name (gensym ctor-name)
                        :constructor-name ctor-name
                        :prototype-node (car (get-exemplar-value-nodes graph ctor-name))))
      (t
       (get-node-property graph ctor-node "prototype")))))

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
;;;; POPULATE phase

(defun populate-type-graph (elm)
  "Populate a type-graph with nodes and initial edges based on source-element ELM"
  (let ((graph (make-hash-table :test 'equal)))
    (populate-nodes graph elm)
    graph))

(defgeneric populate-nodes (graph elm)
  (:documentation
   "Analyzes source element ELM and adds nodes and edges to GRAPH
    based upon that analysis.  No transitive analysis is performed
    (that's for the CONNECT and COLLAPSE phases).  Returns a location-node
    that represents ELM's value for expression elements."))

(defparameter *innermost-function-node* nil
  "The value-node of the innermost function decl, if any")

(defmethod populate-nodes (graph (elm-list list))
  (loop for elm in elm-list
        do (populate-nodes graph elm)))

(defmethod populate-nodes (graph (elm source-element))
  (loop for slot in (structure-slots elm)
        do (populate-nodes graph (slot-value elm slot))))

(defmethod populate-nodes (graph elm)
  nil)

(defmethod populate-nodes (graph (elm identifier))
  (get-location-node graph (identifier-name elm)))

(defmethod populate-nodes (graph (elm string-literal))
  (get-instance-node graph "String"))

(defmethod populate-nodes (graph (elm re-literal))
  (get-instance-node graph "RegExp"))

(defmethod populate-nodes (graph (elm numeric-literal))
  (get-instance-node graph "Number"))

(defmethod populate-nodes (graph (elm special-value))
  (ecase (special-value-symbol elm)
    (:this
     (if (null *innermost-function-node*)
       (get-location-node graph 'global-context)
       (get-this-context-node graph *innermost-function-node*)))
    ((:false :true)
     (get-instance-node graph "Boolean"))
    (:null
     (get-location-node graph "null"))
    (:undefined
     (get-location-node graph "undefined"))
    (:function_continuation
     (if (null *innermost-function-node*)
       (error "type-analysis encountered function_continuation at toplevel")
       (get-continuation-node graph *innermost-function-node*)))))

(defmethod populate-nodes (graph (elm binary-operator))
  (let ((left-node (populate-nodes graph (binary-operator-left-arg elm)))
        (right-node (populate-nodes graph (binary-operator-right-arg elm))))
    (case (binary-operator-op-symbol elm)
      ((:assign :plus-equals
        :and-equals :xor-equals :or-equals)
       (add-assignment-edge left-node right-node)
       left-node)

      ((:times-equals :divide-equals :mod-equals :minus-equals
        :lshift-equals :rshift-equals :urshift-equals)
       (add-assignment-edge left-node (get-instance-node graph "Number"))
       left-node)

      ((:multiply :divide :modulo :subtract)
       (get-instance-node graph "Number"))

      ((:equals :strict-equals :not-equals :strict-not-equals)
       (get-instance-node graph "Boolean"))

      (otherwise
       (let ((expr-node (get-location-node graph (gensym "expr"))))
         (add-assignment-edge expr-node left-node)
         (add-assignment-edge expr-node right-node)
         expr-node)))))

(defmethod populate-nodes (graph (elm unary-operator))
  (populate-nodes graph (unary-operator-arg elm))
  (case (unary-operator-op-symbol elm)
    ((:pre-incr :post-incr :pre-decr :post-decr :unary-plus :unary-minus :bitwise-not)
     (get-instance-node graph "Number"))
    ((:logical-not :delete)
     (get-instance-node graph "Boolean"))
    (:typeof
     (get-instance-node graph "String"))
    (:void
     (get-location-node graph "undefined"))
    (otherwise
     (error "unrecognized unary operation ~A" (unary-operator-op-symbol elm)))))
    
(defmethod populate-nodes (graph (elm var-decl))
  (let ((left-node (get-location-node graph (var-decl-name elm))))
    (if (var-decl-initializer elm)
      (add-assignment-edge left-node (populate-nodes graph (var-decl-initializer elm)))
      (add-assignment-edge left-node (get-location-node graph "undefined")))))

(defmethod populate-nodes (graph (elm fn-call))
  (multiple-value-bind (fn-node prop-target-node)
      (populate-nodes graph (fn-call-fn elm))
    (let ((ret-node (get-return-node graph fn-node)))

      (if (property-access-p (fn-call-fn elm))
        (pushnew prop-target-node
                 (location-node-this-bindings fn-node))
        (pushnew (get-location-node graph 'global-context)
                 (location-node-this-bindings fn-node)))

      (setf (location-node-min-call-arity fn-node)
            (min* (location-node-min-call-arity fn-node)
                  (length (fn-call-args elm))))

      (loop for arg in (fn-call-args elm)
            for idx upfrom 0
            do (add-assignment-edge (get-node-argument graph fn-node idx)
                                    (populate-nodes graph arg)))

      ret-node)))

(defmethod populate-nodes (graph (elm resume-statement))
  (let ((target-node (populate-nodes graph (resume-statement-target elm)))
        (arg-node (populate-nodes graph (resume-statement-arg elm))))
    (unless (null arg-node)
      (add-assignment-edge (get-node-argument graph target-node 0)
                           arg-node))))
  
(defmethod populate-nodes (graph (elm function-decl))
  (let ((*innermost-function-node* (setup-function-node
                                    graph
                                    (get-location-node graph (function-decl-name elm)))))

    ;; Redefining functions is legal, but probably not what we wanted
    (unless (or (null (value-node-return-node *innermost-function-node*))
                (null (value-node-parameters *innermost-function-node*)))
      (warn "Type-analysis encountered function ~A multiple times" (function-decl-name elm)))

    (loop for param in (function-decl-parameters elm)
          collect (get-location-node graph param) into param-list
          finally (setf (value-node-parameters *innermost-function-node*)
                        (nconc param-list (value-node-parameters *innermost-function-node*))))
    
    (populate-nodes graph (function-decl-body elm))

    ;; Handling for functions with no explicit return value
    (unless (explicit-return-p (function-decl-body elm))
      (add-assignment-edge (get-return-node graph *innermost-function-node*)
                           (get-location-node graph "undefined")))))

(defmethod populate-nodes (graph (elm function-expression))
  (let* ((function-name (aif (function-expression-name elm)
                          it
                          (gensym "function-expression")))
         (fn-location-node (get-location-node graph function-name))
         (*innermost-function-node* (setup-function-node
                                     graph
                                     fn-location-node)))
    (loop for param in (function-expression-parameters elm)
          collect (get-location-node graph param) into param-list
          finally (setf (value-node-parameters *innermost-function-node*)
                        param-list))

    (populate-nodes graph (function-expression-body elm))

    ;; Handling for functions with no explicit return value
    (unless (explicit-return-p (function-expression-body elm))
      (add-assignment-edge (get-return-node graph *innermost-function-node*)
                           (get-location-node graph "undefined")))

    ;; Function-expressions are expressions (duh) so return the location-node for this value
    fn-location-node))

(defmethod populate-nodes (graph (elm return-statement))
  (if *innermost-function-node*
    (add-assignment-edge (get-return-node graph *innermost-function-node*)
                         (populate-nodes graph (return-statement-arg elm)))
    (error "type-analysis encountered a return statement at topmost scope")))

(defun compute-field-name (field-elm)
  "Return the name of a property.  For properties specified by literals, this is the
   name of the property; for all other field names (ie, for identifiers or other expressions)
   this will be the special field-name 'ANY."
  (if (or (string-literal-p field-elm)
          (numeric-literal-p field-elm))
    (slot-value field-elm 'value)
    'any))

(defmethod populate-nodes (graph (elm property-access))
  (let ((target-node (populate-nodes graph (property-access-target elm)))
        (field-elm (property-access-field elm)))

    (populate-nodes graph field-elm)
    (values
     (get-node-property graph target-node (compute-field-name field-elm))
     target-node))) ; We return a second value to permit the FN-CALL mtd to avoid repopulating the target node
    
(defmethod populate-nodes (graph (elm new-expr))
  (let ((ctor-node (populate-nodes graph (new-expr-constructor elm))))

    (setf (location-node-min-call-arity ctor-node)
          (min* (location-node-min-call-arity ctor-node)
                (length (new-expr-args elm))))

    (pushnew (get-node-property graph ctor-node "prototype")
             (location-node-this-bindings ctor-node))
    
    (loop for arg in (new-expr-args elm)
          for idx upfrom 0
          do (add-assignment-edge (get-node-argument graph ctor-node idx)
                                  (populate-nodes graph arg)))

    (get-instance-node graph ctor-node)))

(defmethod populate-nodes (graph (elm object-literal))
  (let ((literal-node (get-instance-node graph "Object")))
    (loop for (prop-name . prop-elm) in (object-literal-properties elm)
          for field-name = (compute-field-name prop-name)
          do (add-assignment-edge
              (get-node-property graph literal-node field-name)
              (populate-nodes graph prop-elm)))
    literal-node))

(defmethod populate-nodes (graph (elm array-literal))
  (let ((literal-node (get-instance-node graph "Array")))
    (loop for index upfrom 0
          for value-elm in (array-literal-elements elm)
          do (add-assignment-edge
              (get-node-property graph literal-node index)
              (populate-nodes graph value-elm)))
    literal-node))

;;; ======================================================================
;;;; The NODE-QUEUE data-type (TODO move to general-utilities as editable-queue)
(defstruct node-queue-entry
  prev
  next
  item)

(defstruct node-queue-container
  root-entry
  lookup)

(defun make-node-queue (&key (test 'equal))
  "Create an empty NODE-QUEUE"
  (let ((container (make-node-queue-container :lookup (make-hash-table :test test)
                                              :root-entry (make-node-queue-entry))))
    (setf (node-queue-entry-prev (node-queue-container-root-entry container))
          (node-queue-container-root-entry container))
    (setf (node-queue-entry-next (node-queue-container-root-entry container))
          (node-queue-container-root-entry container))
    container))
   
(defun enqueue-node (queue node)
  "Add NODE to the end of QUEUE"
  (let* ((right (node-queue-container-root-entry queue))
         (left (node-queue-entry-prev right))
         (mid (make-node-queue-entry :prev left :next right :item node)))
    (unless (gethash node (node-queue-container-lookup queue))
      (setf (node-queue-entry-next left)
            mid)
      (setf (node-queue-entry-prev right)
            mid)
      (setf (gethash node (node-queue-container-lookup queue))
            mid))
    queue))

(defun dequeue-node (queue)
  "Remove and return a node from the front of QUEUE"
  (let* ((left (node-queue-container-root-entry queue))
         (mid (node-queue-entry-next left))
         (right (node-queue-entry-next mid)))
    (setf (node-queue-entry-next left) right)
    (setf (node-queue-entry-prev right) left)
    (remhash (node-queue-entry-item mid) (node-queue-container-lookup queue))
    (node-queue-entry-item mid)))

(defun remove-queued-node (queue node)
  "Removes the specified NODE from QUEUE"
  (when (gethash node (node-queue-container-lookup queue))
    (let* ((mid (gethash node (node-queue-container-lookup queue)))
           (left (node-queue-entry-prev mid))
           (right (node-queue-entry-next mid)))
      (setf (node-queue-entry-next left) right)
      (setf (node-queue-entry-prev right) left)
      (remhash (node-queue-entry-item mid) (node-queue-container-lookup queue))
      (node-queue-entry-item mid))))

(defun node-queue-size (queue)
  "Return the number of nodes stored in QUEUE"
  (hash-table-count (node-queue-container-lookup queue)))

;;; ======================================================================
;;;; CONNECT phase

(defun connect-type-graph (graph)
  (let ((queue (make-node-queue)))

    ;; Add all the location nodes to the processing queue
    (loop for node being each hash-value of graph
          do (enqueue-node queue node))

    ;; Process the queue
    (loop while (> (node-queue-size queue) 0)
          for node = (dequeue-node queue)
          do (connect-nodes node graph queue nil
                            nil nil nil nil nil))
    graph))
          
          
(defgeneric connect-nodes (node graph queue path
                                env-this env-rets env-args env-min
                                env-props)
  (:documentation
   "Adds extra connections NODE and its descendants to account for
    function calls and property-accesses.

    QUEUE is the queue of nodes to process; CONNECT-NODES may mutate its value.

    ENV-THIS is a list of this-bindings encountered so far.  Every value-node that
    is encountered will have an edge added from its this-context-node to these nodes.

    ENV-RET is a list of RET nodes encountered so far; Every value-node that is
    encountered will have an edge added from its ret-node to each of these nodes.

    ENV-ARGS is a list of (ARG-INDEX . LOCATION-NODE) cells of arg-bindings encountered
    so far; value-nodes that are encountered will add edges from their parameter nodes
    to each of these nodes.
    
    ENV-MIN is the minimum of all ancestor nodes' MIN-CALL-ARITY.

    ENV-PROPS is a list of assoc-cells (PROP-NAME . NODE).  Note that there
    may be more than one cell for a given property name, so it's not safe
    to use ASSOC."))

(defmethod connect-nodes :around ((node location-node) graph queue path
                                    env-this env-rets env-args env-min
                                    env-props)
  (unless (member node path)
    (call-next-method)))

(defmethod connect-nodes ((node location-node) graph queue path
                            env-this env-rets env-args env-min
                            env-props)
  (let ((own-this (union (location-node-this-bindings node) env-this))
        (own-rets (aif (location-node-return-node node)
                    (adjoin it env-rets)
                    env-rets))
        (own-args (append (location-node-arguments node)
                          env-args))
        (own-min (min* (location-node-min-call-arity node)
                       env-min))
        (own-props (append (location-node-properties node)
                           env-props))
        (own-path (cons node path)))

    ;; GAH filthy bastard line
    ;; This is not actually safe in the presence of cycles; need to do some additional
    ;; checking as in COLLAPSE-TYPE-GRAPH
;    (remove-queued-node queue node)

    (dolist (child (location-node-assignments node))
      (connect-nodes child graph queue own-path
                     own-this own-rets own-args own-min
                     own-props))))

(defmethod connect-nodes ((node value-node) graph queue path
                            env-this env-rets env-args env-min
                            env-props)
  ;; This-context edges
  (loop for caller-this in env-this
        do (add-assignment-edge (get-this-context-node graph node queue) caller-this queue))
      
  ;; Return edges
  (loop for caller-ret in env-rets
        do (add-assignment-edge caller-ret (get-return-node graph node queue) queue))

  ;; Undefined argument handling
  (loop for param in (value-node-parameters node)
        for idx upfrom 0
        when (and (numberp env-min)
                  (>= idx env-min))
        do (add-assignment-edge param (get-location-node graph "undefined" queue) queue))

  ;; Link corresponding arguments and parameters
  ;; TODO Deal with worse-than-quadratic nature of this operation, perhaps
  ;; by using an array for parameters instead of a list.
  (loop for (arg-idx . arg-node) in env-args
        for param-node = (nth arg-idx (value-node-parameters node))
        unless (null param-node)
        do (add-assignment-edge param-node arg-node queue))

  ;; Link corresponding properties
  ;; TODO Currently O(n^2); fix by using hash-table for properties
  (loop for (prop-name . prop-node) in env-props
        for own-node = (get-node-property graph node prop-name queue)
        do
        (add-assignment-edge own-node prop-node queue)
        (add-assignment-edge prop-node own-node queue)))

;;; ======================================================================
;;;; COLLAPSE phase

(defparameter *cycle-free-collapse-pass* t
  "T if no cycles were encountered on this pass of COLLAPSE-NODES")

(defun collapse-type-graph (graph)
  "Adds an edge from each location-node in GRAPH to each value-node that it has a
   path to, and removes all other assignment edges.  Removes all 'dotted' edges
   (ie, args, ret, and props) from location-nodes; only value-nodes will have dotted
   edges after this processing is done."

  ;; Process each node in the graph, and then remove anonymous location-nodes
  (maphash (lambda (name node)
             (declare (ignore value))
             (let ((*cycle-free-collapse-pass* t))
               (collapse-nodes node nil)
               (when (anonymous-node-p node)
                 (remhash name graph))))
           graph)
  graph)

(defun anonymous-node-p (node)
  "Return T if NODE is an 'anonymous' node (ie, one whose name is an uninterned
   symbol)."
  (and (symbolp (type-graph-node-name node))
       (null (symbol-package (type-graph-node-name node)))))
    

(defgeneric collapse-nodes (node path)
  (:documentation
  "Adds an edge from NODE to each value-node that it has a
   path to, and removes all other assignment edges.  Removes all 'dotted' edges
   (ie, args, ret, and props) from location-nodes; only value-nodes will have dotted
   edges after this processing is done.  Recursively processes all assignment-children.
   PATH is a list of nodes representing the path taken to get to this node.
   Returns all value-nodes encountered so far."))

(defmethod collapse-nodes ((node location-node) path)
  (if (member node path)
    (setf *cycle-free-collapse-pass* nil)
    (let* ((own-path (cons node path))
           (new-assignments (remove-duplicates
                             (loop for child in (location-node-assignments node)
                                   append (collapse-nodes child own-path)))))
      (when (or *cycle-free-collapse-pass*
                (null path))
        (setf (location-node-properties node) nil)
        (setf (location-node-arguments node) nil)
        (setf (location-node-return-node node) nil)
        (setf (location-node-this-bindings node) nil)
        (setf (type-graph-node-assignment-backlinks node) nil)        
        (setf (location-node-assignments node) new-assignments))
      new-assignments)))

(defmethod collapse-nodes ((node value-node) path)
  (setf (type-graph-node-assignment-backlinks node) nil)
  (list node))
    
;;; ======================================================================
;;;; Interface functions

(defgeneric compute-types (expression-elm graph &optional execution-context)
  (:documentation
  "Returns a list of value-nodes representing a set of possible types for
   the expression represented by EXPRESSION-ELM based on the type-graph
   GRAPH.

   EXPRESSION-ELM is evaluated in the global execution context unless EXECUTION-CONTEXT
   is specified.  EXECUTION-CONTEXT can be either NIL (for global context), the name of a function,
   or a value-node that represents a function."))

;;TODO When the expression type is added to js-source-model
;(defmethod compute-types (graph (elm expression))
;  nil)

(defmethod compute-types ((elm numeric-literal) graph &optional execution-context)
  (get-exemplar-value-nodes graph "Number"))

(defmethod compute-types ((elm string-literal) graph &optional execution-context)
  (get-exemplar-value-nodes graph "String"))

(defmethod compute-types ((elm re-literal) graph &optional execution-context)
  (get-exemplar-value-nodes graph "RegExp"))

(defmethod compute-types ((elm identifier) graph &optional execution-context)
  (let ((node (find-location-node graph (identifier-name elm))))
    (if node
      (location-node-assignments node)
      (compute-types #s(special-value :symbol :undefined) graph execution-context))))

(defmethod compute-types ((elm special-value) graph &optional execution-context)
  (ecase (special-value-symbol elm)
    (:this
     (cond
       ((value-node-p execution-context)
        (location-node-assignments (get-this-context-node graph execution-context)))
       ((null execution-context)
        (location-node-assignments (get-location-node graph 'global-context)))
       (t
        (mapcan (lambda (v-node)
                  (location-node-assignments (get-this-context-node graph v-node)))
                (location-node-assignments (get-location-node graph execution-context))))))
    ((:false :true)
     (get-exemplar-value-nodes graph "Boolean"))
    (:null
     (location-node-assignments (get-location-node graph "null")))
    (:undefined
     (location-node-assignments (get-location-node graph "undefined")))
    (:function_continuation
     (cond
       ((value-node-p execution-context)
        (location-node-assignments (get-continuation-node graph execution-context)))
       ((null execution-context)
        (error "function_continuation cannot be typed without an execution context"))
       (t
        (mapcan (lambda (v-node)
                  (location-node-assignments (get-continuation-node graph v-node)))
                (location-node-assignments (get-location-node graph execution-context))))))))

(defmethod compute-types ((elm property-access) graph &optional execution-context)
  (let ((target-types (compute-types (property-access-target elm) graph execution-context))
        (field-name (compute-field-name (property-access-field elm))))
    (labels ((get-prop-values (value-node field &optional default-value)
               (let ((prop-node (find-node-property value-node field)))
                 (if prop-node
                   (location-node-assignments prop-node)
                   default-value)))
             (process-value-node (value-node)
               (if (eq field-name 'any)
                 (loop for prop-cell in (value-node-properties value-node)
                       append (location-node-assignments (cdr prop-cell)))
                 (union
                  (get-prop-values value-node field-name (compute-types #s(special-value :symbol :undefined) graph execution-context))
                  (get-prop-values value-node 'any nil)))))
      (remove-duplicates
       (loop for value-node in target-types
             append (process-value-node value-node))))))

(defmethod compute-types ((elm fn-call) graph &optional execution-context)
  (let ((fn-types (compute-types (fn-call-fn elm) graph execution-context)))
    (remove-duplicates
     (loop for value-node in fn-types
           append (location-node-assignments (value-node-return-node value-node))))))

(defmethod compute-types ((elm binary-operator) graph &optional execution-context)
  (let ((left-types (compute-types (binary-operator-left-arg elm) graph execution-context))
        (right-types (compute-types (binary-operator-right-arg elm) graph execution-context)))
    (case (binary-operator-op-symbol elm)
      ((:assign :plus-equals
        :and-equals :xor-equals :or-equals)
       (union left-types right-types))

      ((:times-equals :divide-equals :mod-equals :minus-equals
        :lshift-equals :rshift-equals :urshift-equals)
       (union (get-exemplar-value-nodes graph "Number") left-types))

      ((:multiply :divide :modulo :subtract)
       (get-exemplar-value-nodes graph "Number"))

      ((:equals :strict-equals :not-equals :strict-not-equals)
       (get-exemplar-value-nodes graph "Boolean"))

      (otherwise
       (union left-types right-types)))))

(defmethod compute-types ((elm unary-operator) graph &optional execution-context)
  (case (unary-operator-op-symbol elm)
    ((:pre-incr :post-incr :pre-decr :post-decr :unary-plus :unary-minus :bitwise-not)
     (get-exemplar-value-nodes graph "Number"))
    ((:logical-not :delete)
     (get-exemplar-value-nodes graph "Boolean"))
    (:typeof
     (get-exemplar-value-nodes graph "String"))
    (:void
     (compute-types #s(special-value :symbol :undefined) graph execution-context))
    (otherwise
     (error "unrecognized unary operation ~A" (unary-operator-op-symbol elm)))))

(defmethod compute-types ((elm object-literal) graph &optional execution-context)
  (loop with value-node = (make-value-node :name (gensym "computed-object-literal"))
        for (prop-name . prop-elm) in (object-literal-properties elm)
        for field-name = (compute-field-name prop-name)
        for prop-node = (make-location-node :name (gensym (format nil "prop$~A" field-name))
                                            :assignments (compute-types prop-elm graph execution-context))
        do (push (cons field-name prop-node)
                 (value-node-properties value-node))
        finally (return (list value-node))))

(defmethod compute-types ((elm array-literal) graph &optional execution-context)
  (loop with value-node = (make-value-node :name (gensym "computed-array-literal"))
        for index upfrom 0
        for prop-elm in (array-literal-elements elm)
        for prop-node = (make-location-node :name (gensym (format nil "prop$~A" index))
                                            :assignments (compute-types prop-elm graph execution-context))
        do (push (cons index prop-node)
                 (value-node-properties value-node))
        finally (return (list value-node))))
      
(defmethod compute-types ((elm function-expression) graph &optional execution-context)
  (with-slots (name) elm
    (aif (and name
              (find-location-node graph name))
      (location-node-assignments it)
      (let ((fn-node (populate-nodes graph elm)))
        (remhash (location-node-name fn-node) graph) ; Don't add to the graph permanently XXX is this right?
        (location-node-assignments fn-node)))))

;;;================================================================================
;;;; type-analyze function
;;;
;;; This is the main external interface for analyzing a group of source elements.

(defun type-analyze (elm)
  "Perform type analysis on ELM and return the corresponding type-map."
  (let ((graph (populate-type-graph elm)))
    (connect-type-graph graph)
    (collapse-type-graph graph)
    graph))

;;;================================================================================
;;;; Debugging helpers

(defun type-graph-node-unique-name (node)
  "Return a name for NODE that is likely to be unique"
  (assert (type-graph-node-p node))
  (if (value-node-p node)
    (format nil "VALUE$~A" (type-graph-node-name node))
    (format nil "LOC$~A" (type-graph-node-name node))))

(defgeneric copy-type-graph-nodes (node old-hash new-hash)
  (:documentation
   "If NODE is not a member of old-hash, then add NODE to OLD-HASH and
    an identically-named fresh node to NEW-HASH and then recursively
    visit all of its descendants."))

(defmethod copy-type-graph-nodes ((node location-node) old-hash new-hash)
  (with-slots (name) node
    (let ((unique-name (type-graph-node-unique-name node)))
      (unless (gethash unique-name old-hash)
        (setf (gethash unique-name old-hash)
              node)
        (setf (gethash unique-name new-hash)
              (make-location-node :name name))
        (dolist (child (location-node-assignments node))
          (copy-type-graph-nodes child old-hash new-hash))))))

(defmethod copy-type-graph-nodes ((node value-node) old-hash new-hash)
  (with-slots (name) node
    (let ((unique-name (type-graph-node-unique-name node)))
      (unless (gethash unique-name old-hash)
        (setf (gethash unique-name old-hash)
              node)
        (setf (gethash unique-name new-hash)
              (make-value-node :name name))))))

(defgeneric copy-type-graph-edges (node old-hash new-hash)
  (:documentation
   "copy all of the edges of node to edges between the corresponding nodes
    in new-hash."))

(defmethod copy-type-graph-edges ((old-node type-graph-node) old-hash new-hash)
  (let ((new-node (gethash (type-graph-node-unique-name old-node) new-hash)))

    ;; Copy properties
    (loop for (prop-name . old-prop) in (type-graph-node-properties old-node)
          for new-prop = (gethash (type-graph-node-unique-name old-prop) new-hash)
          do (push (cons prop-name new-prop)
                   (type-graph-node-properties new-node)))
    (setf (type-graph-node-properties new-node)
          (reverse (type-graph-node-properties new-node)))

    ;; Copy backlinks
    (dolist (parent (type-graph-node-assignment-backlinks old-node))
      (push (gethash (type-graph-node-unique-name parent) new-hash)
            (type-graph-node-assignment-backlinks new-node)))
    (setf (type-graph-node-assignment-backlinks new-node)
          (reverse (type-graph-node-assignment-backlinks new-node)))

    ;; Copy ret-node
    (when-let (old-ret (type-graph-node-return-node old-node))
      (setf (type-graph-node-return-node new-node)
            (gethash (type-graph-node-unique-name old-ret) new-hash)))))
      

(defmethod copy-type-graph-edges ((old-node location-node) old-hash new-hash)
  (let ((new-node (gethash (type-graph-node-unique-name old-node) new-hash)))

    ;; Copy assignments
    (dolist (child (location-node-assignments old-node))
      (push (gethash (type-graph-node-unique-name child) new-hash)
            (location-node-assignments new-node)))
    (setf (location-node-assignments new-node)
          (reverse (location-node-assignments new-node)))

    ;; Copy arguments
    (loop for (index . old-arg) in (location-node-arguments old-node)
          for new-arg = (gethash (type-graph-node-unique-name old-arg) new-hash)
          do (push (cons index new-arg)
                   (location-node-arguments new-node)))
    (setf (location-node-arguments new-node)
          (reverse (location-node-arguments new-node)))

    ;; Copy this-bindings
    (dolist (child (location-node-this-bindings old-node))
      (push (gethash (type-graph-node-unique-name child) new-hash)
            (location-node-this-bindings new-node)))
    (setf (location-node-this-bindings new-node)
          (reverse (location-node-this-bindings new-node)))

    (setf (location-node-min-call-arity new-node)
          (location-node-min-call-arity old-node))

    ;; Copy common fields
    (call-next-method)))

(defmethod copy-type-graph-edges ((old-node value-node) old-hash new-hash)
  (let ((new-node (gethash (type-graph-node-unique-name old-node) new-hash)))

    ;; Copy parameters
    (loop for old-param in (value-node-parameters old-node)
          for new-param = (gethash (type-graph-node-unique-name old-param) new-hash)
          do (push new-param (value-node-parameters new-node)))
    (setf (value-node-parameters new-node)
          (reverse (value-node-parameters new-node)))

    ;; Copy singletons
    (when-let (old-proto (value-node-prototype-node old-node))
      (setf (value-node-prototype-node new-node)
            (gethash (type-graph-node-unique-name old-proto) new-hash)))

    (when-let (old-this (value-node-this-context-node old-node))
      (setf (value-node-this-context-node new-node)
            (gethash (type-graph-node-unique-name old-this) new-hash)))

    ;; Copy constructor name
    (setf (value-node-constructor-name new-node)
          (value-node-constructor-name old-node))

    ;; Common fields
    (call-next-method)))

(defun copy-type-graph (graph)
  "Returns a newly-created equivalent GRAPH"
  (let ((old-hash (make-hash-table :test 'equal))
        (new-hash (make-hash-table :test 'equal))
        (ret-graph (make-hash-table :test 'equal)))

    ;; Fill the lookups with all the nodes
    (loop for node being each hash-value of graph
          do (copy-type-graph-nodes node old-hash new-hash))

    ;; Copy the edges of each graph
    (loop for node being each hash-value of old-hash
          do (copy-type-graph-edges node old-hash new-hash))

    ;; Build a graph lookup containing only the nodes in GRAPH
    (loop for node being each hash-value of graph
          do (setf (gethash (type-graph-node-name node) ret-graph)
                   (gethash (type-graph-node-unique-name node) new-hash)))

    (values ret-graph new-hash)))

;;TODO Move this somewhere else
(defun make-dot-graph (type-graph &optional (fname "c:/temp/types.dot"))
  (let* ((path (if (pathname-type (pathname fname))
                 (pathname fname)
                 (merge-pathnames (pathname fname) (make-pathname :type "dot")))))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (let ((node-history nil)
            (node-queue (make-array (hash-table-count type-graph) :fill-pointer 0 :adjustable t)))
        (labels ((get-name (node)
                   (substitute #\_ #\-
                               (substitute #\_ #\$
                                           (string
                                            (if (value-node-p node)
                                              (format nil "value_~A" (value-node-name node))
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

                 (print-collection-edges (from-node to-collection-accessor &optional user-label)
                   (loop for cell-or-node in (funcall to-collection-accessor from-node)
                         for to-node = (if (consp cell-or-node)
                                         (cdr cell-or-node)
                                         cell-or-node)
                         for label = (if (consp cell-or-node)
                                       (car cell-or-node)
                                       user-label)
                         do
                         (print-edge from-node to-node label)
                         (queue-node to-node)))

                 (print-node-edges (node)
                   (unless (find node node-history)
                     (push node node-history)

                     (if (location-node-p node)
                       (format s "  ~A [shape=ellipse];~%" (get-name node))
                       (format s "  ~A [shape=box];~%" (get-name node)))
                   
                     (print-collection-edges node 'type-graph-node-properties)
                   
;                     (when (type-graph-node-assignment-backlinks node)
;                       (print-collection-edges node 'type-graph-node-assignment-backlinks "<<"))
                       
                     (when (value-node-p node)
                       (print-collection-edges node 'value-node-parameters "")
                       (when-let (this-node (value-node-this-context-node node))
                         (print-edge node
                                     this-node
                                     "$thisContext")
                         (queue-node this-node))
                       (when-let (k-node (value-node-continuation-node node))
                         (print-edge node
                                     k-node
                                     "$k")))

                     (when (location-node-p node)
                       (print-collection-edges node 'location-node-arguments)
                       (print-collection-edges node 'location-node-assignments)
                       (print-collection-edges node 'location-node-this-bindings "$this"))
                     
                     (when-let (ret-node (type-graph-node-return-node node))
                       (print-edge node
                                   ret-node
                                   "$ret")
                       (queue-node ret-node)))))

          (format s "digraph {~%  ~%")
        
          (loop for node being each hash-value in type-graph
                do (queue-node node))

          (loop for idx upfrom 0
                while (< idx (fill-pointer node-queue))
                do (print-node-edges (aref node-queue idx)))
      
          (format s "}"))))))

