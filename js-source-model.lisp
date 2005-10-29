(in-package :sugarscript)

(defstruct this)
(defstruct null)

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
  arguments)

(defstruct call
  function-name
  arguments)

(defstruct property-access
  target
  field)

(defstruct post-incr
  argument)

(defstruct post-deccr
  argument)

(defstruct delete
  argument)

(defstruct void
  argument)

(defstruct typeof
  argument)

(defstruct pre-incr
  argument)

(defstruct pre-decr
  argument)

(defstruct unary-plus
  argument)

(defstruct unary-minus
  argument)

(defstruct bitwise-not
  argument)

(defstruct logical-not
  argument)

(defstruct multiply
  left-argument
  right-argument)

(defstruct divide
  left-argument
  right-argument)

(defstruct modulo
  left-argument
  right-argument)

(defstruct add
  left-argument
  right-argument)

(defstruct subtract
  left-argument
  right-argument)

(defstruct lshift
  left-argument
  right-argument)

(defstruct rshift
  left-argument
  right-argument)

(defstruct
    ;;HERE
    
;;;;; Source model
(defparameter source-element-types
  '(:this
    :identifier
    :null
    :number
    :string-literal
    :boolean
    :array
    :object
    :new
    :call
    :property-access
    :post-incr
    :post-decr
    :delete
    :void
    :typeof
    :pre-incr
    :pre-decr
    :unary-plus
    :unary-minus
    :bitwise-not
    :logical-not
    :multiply
    :divide
    :modulo
    :add
    :subtract
    :lshift
    :rshift
    :urshift
    :in
    :less-than
    :less-than-equals
    :greater-than
    :greater-than-equals
    :instanceof
    :equals
    :not-equals
    :strict-equals
    :strict-not-equals
    :bitwise-and
    :bitwise-xor
    :bitwise-or
    :logical-and
    :logical-or
    :conditional
    :comma
    :var
    :if
    :do
    :while
    :for
    :for-in
    :continue
    :break
    :return
    :with
    :switch
    :case
    :default
    :label
    :throw
    :try
    :catch
    :finally
    :function-decl
    :function-expression))
