;;;; type-analysis.lisp
;;;
;;; Defines functions and data structures for static type analysis
;;; on jwacs source code.
(in-package :jwacs)

(defparameter *type-graph* nil
  "Hash table from value-key to value-node.  A value-node is a node
   of the type graph, with edges to all the value-nodes that it is
   assigned to.")
;;TODO Define a type-key more completely

(defstruct value-node
  "A node in the type graph that represents a value (eg variable, return
   value, parameter, intermediate value)"
  name
  assignments
  properties) ; Assoc list of (string . node)

(defstruct type-node
  "A node in the type graph that represents a type (eg, FunctionFoo, number)"
  name)

(defun get-value-node (name)
  "Return the value node named NAME from *TYPE-GRAPH*, creating it
   if necessary"
  (multiple-value-bind (node found-p)
      (gethash name *type-graph*)
    (if found-p
        node
        (setf (gethash name *type-graph*)
              (make-value-node :name name)))))

(defun get-value-node-property (node name)
  "Return the value node pointed to by NODE's NAME property, creating
   it if necessary"
  (aif (assoc name (value-node-properties node) :test 'equal)
    (cdr it)
    (let ((new-cell (cons name (make-value-node :name (gensym name)))))
      (push new-cell (value-node-properties node))
      (cdr new-cell))))

(defun find-type-node-named (name node-list)
  "If NODE-LIST contains a type-node named NAME, returns it."
  (find name node-list
            :key (lambda (node)
                   (when (type-node-p node)
                     (type-node-name node)))
            :test 'equal))

(defun get-type-node (name)
  "Return the type node named NAME from *TYPE-GRAPH*, creating it
   if necessary."
  (multiple-value-bind (value-node found-p)
      (gethash name *type-graph*)
    (if found-p
      (find-type-node-named name (value-node-assignments value-node))
      (let ((new-type-node (make-type-node :name name)))
        (setf (gethash name *type-graph*)
              (make-value-node :name name
                               :assignments (list new-type-node)))
        new-type-node))))

(defgeneric internal-analyze (elm)
  (:documentation
   "Performs an analysis of the source element ELM and makes appropriate
    updates to *TYPE-GRAPH*.  Returns a value-node that represents the type
    of ELM when possible."))

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
       (pushnew right-node (value-node-assignments left-node))
       left-node)

      ((:times-equals :divide-equals :mod-equals :minus-equals
        :lshift-equals :rshift-equals :urshift-equals)
       (pushnew (get-type-node "Number") (value-node-assignments left-node))
       left-node)

      ((:multiply :divide :modulo :subtract)
       (setf *h* *type-graph*)
       ;;HERE For some reason this is returning NIL.  WTF?
       (get-type-node "Number"))

      ((:equals :strict-equals :not-equals :strict-not-equals)
       (get-type-node "Boolean"))

      (otherwise
       (let ((expr-node (get-value-node (gensym))))
         (pushnew left-node (value-node-assignments expr-node))
         (pushnew right-node (value-node-assignments expr-node))
         expr-node)))))

(defmethod internal-analyze ((elm var-decl))
  (let ((left-node (get-value-node (var-decl-name elm))))
    (if (var-decl-initializer elm)
      (let ((right-node (internal-analyze (var-decl-initializer elm))))
        (pushnew right-node
                 (value-node-assignments left-node)))
      (pushnew (get-type-node "undefined")
               (value-node-assignments left-node)))))

;;TODO all the other source-element types (primarily the expressions)

;;;; Interface functions
(defun type-analyze (elm)
  "Perform type analysis on ELM and return the corresponding type-map."
  (let ((*type-graph* (make-hash-table :test 'equal)))
    (internal-analyze elm)
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

;; TODO all the other element types

(defstruct node-history
  "A container for a list of already-visited nodes"
  node-list)

(defgeneric compute-node-types (node type-map &optional (node-history (make-node-history)))
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