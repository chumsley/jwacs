;; pretty-print.lisp
;; Print an abstract syntax tree for Javascript in a nicely-formatted fashion
(in-package :sugarscript)

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

(defun assert-type (elm elm-type)
  (assert (eq (first elm) elm-type)))

(defun source-elm-type (elm)
  (cond
    ((and (listp elm) (symbolp (first elm)))
     (first elm))
    ((listp elm)
     :statement-list)
    (t
     nil)))

(defun identifier-name (elm)
  (assert-type elm :identifier)
  (second elm))

(defun number-value (elm)
  (assert-type elm :number)
  (second elm))


(defmacro def-source-element (element-spec)
  (assert (eq (symbol-package (first element-spec))
              (symbol-package :keyword)))
  (let ((element-type-name (symbol-name (first element-spec))))
    `(progn
      ,@(loop
         for n upfrom 1
         for form in (cdr element-spec)
         collect `(defun ,(intern (format nil "~A-~A" element-type-name  form)) (element)
                   (nth ,n element))))))

;;;;; Pretty-printer

(defun pretty-print (s elm)
  (%pretty-print s (source-elm-type elm) elm))
   
(defmethod %pretty-print (s elm-type (elm string))
  (format s "~A" elm))

(defmethod %pretty-print (s (elm-type (eql :this)) elm)
  (format s "this"))

(defmethod %pretty-print (s (elm-type (eql :null)) elm)
  (format s "null"))

(defmethod %pretty-print (s (elm-type (eql :identifier)) elm)
  (pretty-print s (identifier-name elm)))


