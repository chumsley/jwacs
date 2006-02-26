;;;; type-analysis.lisp
;;;
;;; Defines functions and data structures for static type analysis
;;; on jwacs source code.
(in-package :jwacs)

;;; ======================================================================
;;;; Graph data types and utilities

(defstruct type-graph-node
  "A node in the type graph"
  name
  properties       ; Assoc list of (string . node), with 'ANY as a special case
  return-node)     ; location-node

(defstruct (location-node (:include type-graph-node))
  "A type graph node that represents a location (eg variable, return
   value, parameter, intermediate value)"
  assignments      ; List of type-graph-nodes
  arguments        ; Assoc list of (index . node)
  min-call-arity)  ; The smallest numbers of arguments that this function has ever been called with
  
(defstruct (value-node (:include type-graph-node))
  "A node in the type graph that represents a value (object, function, number, etc.)"
  constructor-name ; Name of the constructor for this type, if any
  parameters       ; List of location-nodes
  prototype-node)  ; location-node

(defun find-location-node (graph name)
  "Return the location node named NAME from GRAPH if one
   already exists, or NIL otherwise"
  (gethash name graph))

(defun get-location-node (graph name &optional queue)
  "Return the location node named NAME from GRAPH, creating it
   if necessary.  If the node is created and QUEUE is non-NIL,
   then the new node will be queued for processing."
  (multiple-value-bind (node found-p)
      (gethash name graph)
    (if found-p
      node
      (let ((node (make-location-node :name name)))
        (when queue
          (enqueue-node queue node))
        (setf (gethash name graph) node)))))

(defun find-node-property (node name)
  "Return the location node pointed to by NODE's NAME property if
   one already exists, or NIL otherwise"
  (cdr (assoc name (type-graph-node-properties node) :test 'equal)))

(defun get-node-property (graph node name)
  "Return the location node pointed to by NODE's NAME property, creating
   it in GRAPH if necessary"
  (aif (find-node-property node name)
    it
    (let ((new-cell (cons name (get-location-node graph (gensym (format nil "prop$~A" name))))))
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

(defun get-return-node (graph node)
  "Returns the return-node of type-graph-node NODE, creating it if necessary"
  (aif (type-graph-node-return-node node)
    it
    (let ((new-return-node (get-location-node graph (gensym (format nil "~A$ret" (type-graph-node-name node))))))
      (setf (type-graph-node-return-node node) new-return-node)
      new-return-node)))

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
  (pushnew right-node (location-node-assignments left-node))
  (when queue
    (enqueue-node queue left-node)))

(defun find-value-node (graph name)
  "Return the value named NAME from GRAPH"
  (awhen (find-location-node graph name)
    (find-value-node-named name (location-node-assignments it))))

(defun setup-function-node (graph node)
  "Sets up a location-node to point to a value-node of the same name.
   The existence of three nodes will be guaranteed:
     (1) A value-node with an assignment edge from NODE and the same name as NODE
     (2) A location node pointed to by the 'prototype' property of (1)
     (3) A value-node named #:|NAME-prototype| with an assignment edge from (3).

   Returns the function value-node (2)"
  (let* ((name (location-node-name node))
         (function-value (aif (find-value-node-named name (location-node-assignments node))
                           it
                           (make-value-node :name name :constructor-name "Function"))) ;(1)
         (prototype-node (get-node-property graph function-value "prototype"))) ;(2)

    ;; Ensure edge from NODE to (1)
    (pushnew function-value (location-node-assignments node))
    
    ;; Ensure (3)
    (when (null (remove-if-not 'value-node-p (location-node-assignments prototype-node)))
      (push (make-value-node :name (gensym (format nil "~A_proto" name))
                             :constructor-name name)
            (location-node-assignments prototype-node)))

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
         
    (setup-function-node graph ctor-node)
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
    (:this ;TODO - this deals properly with "declared inside function" but not "set to prototype fields" methods
     *innermost-function-node*)
    ((:false :true)
     (get-instance-node graph "Boolean"))
    (:null
     (get-instance-node graph "null"))
    (:undefined
     (get-instance-node graph "undefined"))))

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
     (get-instance-node graph "undefined"))
    (otherwise
     (error "unrecognized unary operation ~A" (unary-operator-op-symbol elm)))))
    
(defmethod populate-nodes (graph (elm var-decl))
  (let ((left-node (get-location-node graph (var-decl-name elm))))
    (if (var-decl-initializer elm)
      (add-assignment-edge left-node (populate-nodes graph (var-decl-initializer elm)))
      (add-assignment-edge left-node (get-instance-node graph "undefined")))))

(defmethod populate-nodes (graph (elm fn-call))
  (let* ((target-node (populate-nodes graph (fn-call-fn elm)))
         (ret-node (get-return-node graph target-node)))

    (setf (location-node-min-call-arity target-node)
          (min* (location-node-min-call-arity target-node)
                (length (fn-call-args elm))))

    (loop for arg in (fn-call-args elm)
          for idx upfrom 0
          do (add-assignment-edge (get-node-argument graph target-node idx)
                                  (populate-nodes graph arg)))

    ret-node))

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
    
    (populate-nodes graph (function-decl-body elm))))

(defmethod populate-nodes (graph (elm function-expression))
  (let* ((function-name (aif (function-expression-name elm)
                          it
                          (gensym "function-expression")))
         (*innermost-function-node* (setup-function-node
                                     graph
                                     (get-location-node graph function-name))))
    (loop for param in (function-expression-parameters elm)
          collect (get-location-node graph param) into param-list
          finally (setf (value-node-parameters *innermost-function-node*)
                        param-list))

    (populate-nodes graph (function-expression-body elm))
    *innermost-function-node*))

(defmethod populate-nodes (graph (elm return-statement))
  (if *innermost-function-node*
    (add-assignment-edge (get-return-node graph *innermost-function-node*)
                         (populate-nodes graph (return-statement-arg elm)))
    (error "Type-analysis found a return statement at topmost scope")))
   
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
    (get-node-property graph target-node (compute-field-name field-elm))))
    
(defmethod populate-nodes (graph (elm new-expr))
  (let ((ctor-node (populate-nodes graph (new-expr-constructor elm))))
    (get-instance-node graph ctor-node)))

(defmethod populate-nodes (graph (elm object-literal))
  (let ((literal-node (get-instance-node graph "Object")))
    (loop for (prop-name . prop-elm) in (object-literal-properties elm)
          for field-name = (compute-field-name prop-name)
          do (pushnew (populate-nodes graph prop-elm)
                      (location-node-assignments (get-node-property graph literal-node field-name))))

    literal-node))

;;HERE all the other source-element types (primarily the expressions)
; switch stmt
; with stmt
; catch
; function_continuation (special value)
; array-literal

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
                            nil nil nil nil))
    graph))
          
          
(defgeneric connect-nodes (node graph queue path
                                env-rets env-args env-min
                                env-props)
  (:documentation
   "Adds extra connections NODE and its descendants to account for
    function calls and property-accesses.

    QUEUE is the queue of nodes to process; CONNECT-NODES may mutate its value.

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
                                    env-rets env-args env-min
                                    env-props)
  (unless (member node path)
    (call-next-method)))

(defmethod connect-nodes ((node location-node) graph queue path
                            env-rets env-args env-min
                            env-props)
  (let ((own-rets (aif (location-node-return-node node)
                    (cons it env-rets)
                    env-rets))
        (own-args (append (location-node-arguments node)
                          env-args))
        (own-min (min* (location-node-min-call-arity node)
                       env-min))
        (own-props (append (location-node-properties node)
                           env-props))
        (own-path (cons node path)))

    ;; We're processing this node, so no need to process it later
    (remove-queued-node queue node)

    (dolist (child (location-node-assignments node))
      (connect-nodes child graph queue own-path
                     own-rets own-args own-min
                     own-props))))

(defmethod connect-nodes ((node value-node) graph queue path
                            env-rets env-args env-min
                            env-props)
  
  ;; Return edges
  (let ((own-ret (get-return-node graph node)))
    (loop for caller-ret in env-rets
          do (add-assignment-edge caller-ret own-ret queue)))

  ;; Undefined argument handling
  (loop for param in (value-node-parameters node)
        for idx upfrom 0
        when (and (numberp env-min)
                  (>= idx env-min))
        do (add-assignment-edge param (get-instance-node graph "undefined" queue) queue))

  ;; Link corresponding arguments and parameters
  ;; TODO Deal with worse-than-quadratic nature of this operation, perhaps
  ;; by using an array for parameters instead of a list.
  (loop for (arg-idx . arg-node) in env-args
        do (add-assignment-edge (nth arg-idx (value-node-parameters node)) arg-node queue))

  ;; Link corresponding properties
  ;; TODO Currently O(n^2); fix by using hash-table for properties
  (loop for (prop-name . prop-node) in env-props
        for own-node = (get-node-property graph node prop-name)
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
               (collapse-nodes node nil))
             (unless (or (value-node-p node)
                         (stringp name))
               (remhash name graph)))
           graph)
  graph)

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
        (setf (location-node-assignments node) new-assignments))
      new-assignments)))

(defmethod collapse-nodes ((node value-node) path)
  (list node))
    
;;; ======================================================================
;;;; Interface functions

(defgeneric compute-types (expression-elm graph)
  (:documentation
  "Returns a list of value-nodes representing a set of possible types for
   the expression represented by EXPRESSION-ELM based on the type-graph
   GRAPH."))

;;TODO When the expression type is added to js-source-model
;(defmethod compute-types (graph (elm expression))
;  nil)

(defmethod compute-types ((elm numeric-literal) graph)
  (get-exemplar-value-nodes graph "Number"))

(defmethod compute-types ((elm string-literal) graph)
  (get-exemplar-value-nodes graph "String"))

(defmethod compute-types ((elm re-literal) graph)
  (get-exemplar-value-nodes graph "RegExp"))

(defmethod compute-types ((elm identifier) graph)
  (let ((node (find-location-node graph (identifier-name elm))))
    (if node
      (location-node-assignments node)
      (get-exemplar-value-nodes graph "undefined"))))

(defmethod compute-types ((elm special-value) graph)
  (ecase (special-value-symbol elm)
    (:this ;TODO
     (error "this contexts not fully handled yet"))
    ((:false :true)
     (get-exemplar-value-nodes graph "Boolean"))
    (:null
     (get-exemplar-value-nodes graph "null"))
    (:undefined
     (get-exemplar-value-nodes graph "undefined"))))

(defmethod compute-types ((elm property-access) graph)
  (let ((target-types (compute-types (property-access-target elm) graph))
        (field-name (compute-field-name (property-access-field elm))))
    (loop for value-node in target-types
          for property-node = (find-node-property value-node field-name)
          append (if property-node
                   (location-node-assignments property-node)
                   (get-exemplar-value-nodes graph "undefined")))))

(defmethod compute-types ((elm fn-call) graph)
  (let ((fn-types (compute-types (fn-call-fn elm) graph)))
    (loop for value-node in fn-types
          append (location-node-assignments (value-node-return-node value-node)))))

(defmethod compute-types ((elm binary-operator) graph)
  (let ((left-types (compute-types (binary-operator-left-arg elm) graph))
        (right-types (compute-types (binary-operator-right-arg elm) graph)))
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

(defmethod compute-types ((elm unary-operator) graph)
  (case (unary-operator-op-symbol elm)
    ((:pre-incr :post-incr :pre-decr :post-decr :unary-plus :unary-minus :bitwise-not)
     (get-exemplar-value-nodes graph "Number"))
    ((:logical-not :delete)
     (get-exemplar-value-nodes graph "Boolean"))
    (:typeof
     (get-exemplar-value-nodes graph "String"))
    (:void
     (get-exemplar-value-nodes graph "undefined"))
    (otherwise
     (error "unrecognized unary operation ~A" (unary-operator-op-symbol elm)))))


;;HERE other elm types (function_continuation)

(defun type-analyze (elm)
  "Perform type analysis on ELM and return the corresponding type-map."
  (let ((graph (populate-type-graph elm)))
    (connect-type-graph graph)
    (collapse-type-graph graph)
    graph))

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
                                          (if (value-node-p node)
                                            (format nil "type_~A" (value-node-name node))
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
                   
                   (when (value-node-p node)
                     (print-collection-edges node 'value-node-parameters ""))

                   (when (location-node-p node)
                     (print-collection-edges node 'location-node-arguments)
                     (print-collection-edges node 'location-node-assignments))
                     
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
      
      (format s "}")))))
