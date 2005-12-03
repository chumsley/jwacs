;;;; js-source-model.lisp
;;;
;;; Defines the data structures that are used for the internal representation
;;; of parsed Javascript source files.

;;TODO Type declarations on slots (for documentation as much as efficiency)

(in-package :jwacs)

#+(or sbcl cmu)
(eval-when (:compile-toplevel :load-toplevel :execute) 
  (shadow 'defstruct))


#+(or sbcl cmu)
(defmacro defstruct (name &rest slots)
  (let ((type (if (consp name) (first name) name)))
    `(progn
       (cl:defstruct ,name ,@slots)
       (defmethod make-load-form ((self ,type) &optional environment)
         (make-load-form-saving-slots self :environment environment)))))

;;;;= Standard Javascript =

(defstruct source-element
  "A common base type for all source elements")

(defstruct (primitive-value-element (:include source-element))
  "A common base type for all source elements that are guaranteed to
   represent primitive values.
   A primitive value is one that contains no nested function calls.
   So for example, `foo` is a primitive value, as is `1 + 2` or
   `function(x) { foo(bar(baz(y))); }`, but `foo(x)` is not, nor
   is `x + foo(1)`.")

(defstruct (special-value (:include primitive-value-element))
  symbol)

(defstruct (identifier (:include primitive-value-element))
  name)

(defstruct (numeric-literal (:include primitive-value-element))
  value)

(defstruct (string-literal (:include primitive-value-element))
  value)

(defstruct (array-literal (:include source-element))
  elements)

(defstruct (object-literal (:include source-element))
  properties)  ; List of (PROPERTY-NAME . PROPERTY-VALUE)

(defstruct (re-literal (:include primitive-value-element))
  pattern
  options)

(defstruct (new-expr (:include source-element))
  object-name
  args)

(defstruct (fn-call (:include source-element))
  fn
  args)

(defstruct (property-access (:include source-element))
  target
  field)

(defstruct (unary-operator (:include source-element))
  op-symbol
  arg)

(defstruct (binary-operator (:include source-element))
  op-symbol
  left-arg
  right-arg)

(defstruct (conditional (:include source-element))
  condition
  true-arg
  false-arg)

(defstruct (comma-expr (:include source-element))
  exprs)

(defstruct (var-decl-statement (:include source-element))
  var-decls)

(defstruct (var-decl (:include source-element))
  name
  initializer)

(defstruct (statement-block (:include source-element))
  statements)

(defstruct (if-statement (:include source-element))
  condition
  then-statement
  else-statement)

(defstruct (do-statement (:include source-element))
  condition
  body)

(defstruct (while (:include source-element))
  condition
  body)

(defstruct (for (:include source-element))
  initializer
  condition
  step
  body)

(defstruct (for-in (:include source-element))
  binding
  collection
  body)

(defstruct (continue-statement (:include source-element))
  label)

(defstruct (break-statement (:include source-element))
  label)

(defstruct (return-statement (:include source-element))
  arg)

(defstruct (with (:include source-element))
  scope-object
  body)

(defstruct (switch (:include source-element))
  value
  clauses)

(defstruct (case-clause (:include source-element))
  label
  body)

(defstruct (default-clause (:include source-element))
  body)

(defstruct (label (:include source-element))
  name
  statement)

(defstruct (throw-statement (:include source-element))
  value)

(defstruct (try (:include source-element))
  body
  catch-clause
  finally-clause)

(defstruct (catch-clause (:include source-element))
  binding
  body)

(defstruct (finally-clause (:include source-element))
  body)

(defstruct (function-decl (:include source-element))
  name
  parameters
  body)

(defstruct (function-expression (:include primitive-value-element))
  name
  parameters
  body)

;;;;== JWACS extended syntax  ==
(defstruct (suspend-statement (:include source-element))
  arg)

(defstruct (resume-statement (:include source-element))
  arg)

;;;;== Primitive value handling ==
(defgeneric primitive-value-p (elm)
   (:documentation
    "Returns T if ELM is a 'primitive' value, or NIL otherwise.
     See the PRIMITIVE-VALUE-ELEMENT structure for a description of
     primitive values."))

(defmethod primitive-value-p (elm)
  nil)

(defmethod primitive-value-p ((elm primitive-value-element))
  t)

(defmethod primitive-value-p ((elm unary-operator))
  (primitive-value-p (unary-operator-arg elm)))

(defmethod primitive-value-p ((elm binary-operator))
  (and (primitive-value-p (binary-operator-left-arg elm))
       (primitive-value-p (binary-operator-right-arg elm))))

(defmethod primitive-value-p ((elm property-access))
  (and (primitive-value-p (property-access-target elm))
       (primitive-value-p (property-access-field elm))))

;; TODO Array literals and object literals?

(defgeneric primitive-value-references-p (elm identifier-name)
  (:documentation
   "Returns T if the primitive value ELM references an identifier
    whose name is IDENTIFIER-NAME."))

(defmethod primitive-value-references-p ((elm primitive-value-element) identifier-name)
  nil)

(defmethod primitive-value-references-p ((elm identifier) identifier-name)
  (equal (identifier-name elm) identifier-name))

(defmethod primitive-value-references-p ((elm binary-operator) identifier-name)
  (or (primitive-value-references (binary-operator-left-arg elm) identifier-name)
      (primitive-value-references (binary-operator-right-arg elm) identifier-name)))

(defmethod primitive-value-references-p ((elm unary-operator) identifier-name)
  (primitive-value-references (unary-operator-arg elm) identifier-name))

(defmethod primitive-value-references-p ((elm property-access) identifier-name)
  (or (primitive-value-references (property-access-target elm) identifier-name)
      (primitive-value-references (property-access-field elm) identifier-name)))

;;;;== Operator precedence and associativity ==
(defgeneric elm-precedence (elm)
  (:documentation
   "Returns an integer specifying the precedence of the source element
    ELM.  Smaller numbers represent higher precedence.  The precedence
    numbers have no significance except relative to each other."))

(defmethod elm-precedence ((elm source-element))
  (assert (member (type-of elm)
                  '(special-value
                    identifier
                    numeric-literal
                    string-literal
                    array-literal
                    object-literal
                    re-literal)))
  0)

(defmethod elm-precedence ((elm new-expr))
  1)

(defmethod elm-precedence ((elm fn-call))
  1)

(defmethod elm-precedence ((elm property-access))
  1)

(defmethod elm-precedence ((elm unary-operator))
  (ecase (unary-operator-op-symbol elm)
    ((:post-incr :post-decr)
     3)
    ((:delete :void :typeof :pre-incr :pre-decr :unary-plus :unary-minus :logical-not :bitwise-not)
     4)))

(defmethod elm-precedence ((elm binary-operator))
  (ecase (binary-operator-op-symbol elm)
    ((:multiply :divide :modulo)
     5)
    ((:add :subtract)
     6)
    ((:lshift :rshift :urshift)
     7)
    ((:less-than :greater-than :less-than-equals :greater-than-equals :instanceof)
     8)
    ((:equals :not-equals :strict-equals :strict-not-equals)
     9)
    (:bitwise-and
     10)
    (:bitwise-xor
     11)
    (:bitwise-or
     12)
    (:logical-and
     13)
    (:logical-or
     14)
    ((:assign :times-equals :divide-equals :mod-equals :plus-equals :minus-equals
      :lshift-equals :rshift-equals :urshift-equals :and-equals :xor-equals :or-equals)
     16)))

(defmethod elm-precedence ((elm conditional))
  15)

(defmethod elm-precedence ((elm comma-expr))
  17)

(defun op-associativity (op-symbol)
  "Returns either :LEFT or :RIGHT to indicate the associativity of the binary
   infix operation represented by OP-SYMBOL."
  (ecase op-symbol
    ((:multiply :divide :modulo :add :subtract :lshift :rshift :urshift
      :less-than :greater-than :less-than-equals :greater-than-equals :instanceof
      :equals :not-equals :strict-equals :strict-not-equals
      :bitwise-and :bitwise-or :bitwise-xor :logical-and :logical-or)
     :left)
    ((:assign :times-equals :divide-equals :mod-equals :plus-equals :minus-equals
      :lshift-equals :rshift-equals :urshift-equals :and-equals :xor-equals :or-equals)
     :right)))
