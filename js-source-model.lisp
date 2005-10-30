(in-package :sugarscript)

(defstruct special-value
  symbol)

(defstruct identifier
  name)

(defstruct number
  value)

(defstruct string-literal
  value)

(defstruct boolean-literal
  value)

(defstruct array-literal
  value)

(defstruct object-literal
  value)

(defstruct new-expr
  object-name
  args)

(defstruct fn-call
  fn
  args)

(defstruct property-access
  target
  field)

(defstruct unary-operator
  op-symbol
  arg)

(defstruct binary-operator
  op-symbol
  left-arg
  right-arg)
    
(defstruct conditional
  condition
  true-arg
  false-arg)

(defstruct comma-expr
  exprs)

(defstruct var-decl-block
  var-decls)

(defstruct var-decl
  name
  initializer)

(defstruct block
  statements)

(defstruct if
  condition
  then-statement
  else-statement)

(defstruct do
  condition
  body)

(defstruct while
  condition
  body)

(defstruct for
  initializer
  condition
  step
  body)

(defstruct for-in
  binding
  collection
  body)

(defstruct continue
  label)

(defstruct break
  label)

(defstruct return
  arg)

(defstruct with
  scope-object
  body)

(defstruct switch
  clauses)

(defstruct case
  alternative
  body)

(defstruct default
  body)

(defstruct label
  name
  statement)

(defstruct throw
  value)

(defstruct try
  body
  catch-clause
  finally-body)

(defstruct catch-clause
  binding
  body)

(defstruct function-decl
  name
  parameters
  body)

(defstruct function-expression
  name
  parameters
  body)
