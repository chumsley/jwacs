;;;; js-source-model.lisp
;;;
;;; Defines the data structures that are used for the internal representation
;;; of parsed Javascript source files.

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

;;;; Standard Javascript 

; GS- i've added types, but should make a virtual "expressions" type to be used where appropriate
; instead of source-element
; perhaps also one for statement?
; JRW - I think both of those are good ideas.  There's a couple of places in type-analysis where
; that would be helpful.

(defstruct source-element
  "A common base type for all source elements"
  (label nil :type (or string null)))

(defstruct (special-value (:include source-element))
  (symbol nil :type symbol))

(defstruct (identifier (:include source-element))
  (name nil :type string))

(defstruct (numeric-literal (:include source-element))
  (value nil :type number))

(defstruct (string-literal (:include source-element))
  (value nil :type string))

(defstruct (array-literal (:include source-element))
  (elements nil :type list))

(defstruct (object-literal (:include source-element))
  (properties nil :type list))  ; List of (PROPERTY-NAME . PROPERTY-VALUE)
                                ; PROPERTY-NAME is a STRING-LITERAL

(defstruct (re-literal (:include source-element))
  pattern
  options)

(defstruct (new-expr (:include source-element))
  (constructor nil :type (or identifier property-access fn-call))
  (args nil :type (or (cons source-element) null)))

(defstruct (fn-call (:include source-element))
  (fn nil :type (or identifier property-access fn-call))
  (args nil :type (or (cons source-element) null)))

(defstruct (property-access (:include source-element))
  (target nil :type source-element)
  (field nil :type source-element))

(defstruct (unary-operator (:include source-element))
  (op-symbol nil :type symbol)
  (arg nil :type source-element))

(defstruct (binary-operator (:include source-element))
  (op-symbol nil :type symbol)
  (left-arg nil :type source-element)
  (right-arg nil :type source-element))

(defstruct (conditional (:include source-element))
  (condition nil :type source-element)
  (true-arg nil :type source-element)
  (false-arg nil :type source-element))

(defstruct (comma-expr (:include source-element))
  exprs)

(defstruct (var-decl-statement (:include source-element))
  (var-decls nil :type (cons var-decl)))

(defstruct (var-decl (:include source-element))
  (name nil :type string)
  (initializer nil :type (or source-element null)))

(defstruct (statement-block (:include source-element))
  (statements nil :type (or (cons source-element) null)))

(defstruct (if-statement (:include source-element))
  (condition nil :type source-element)
  (then-statement nil :type (or source-element null))
  (else-statement nil :type (or source-element null)))

(defstruct (do-statement (:include source-element))
  (condition nil :type source-element)
  (body nil :type source-element))

(defstruct (while (:include source-element))
  (condition nil :type source-element)
  (body nil :type (or source-element null)))

(defstruct (for (:include source-element))
  (initializer nil :type (or source-element null))
  (condition nil :type (or source-element null))
  (step nil :type (or source-element null))
  (body nil :type (or source-element null)))

(defstruct (for-in (:include source-element))
  (binding nil :type source-element)
  (collection nil :type source-element)
  (body nil :type (or source-element null)))

(defstruct (continue-statement (:include source-element))
  (target-label nil :type (or string null)))

(defstruct (break-statement (:include source-element))
  (target-label nil :type (or string null)))

(defstruct (return-statement (:include source-element))
  (arg nil :type (or source-element null)))

(defstruct (with (:include source-element))
  scope-object
  body)

(defstruct (switch (:include source-element))
  value
  clauses)

(defstruct (case-clause (:include source-element))
  value
  body)

(defstruct (default-clause (:include source-element))
  body)

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
  (name nil :type string)
  (parameters nil :type (or (cons string) null))
  (body nil :type (or (cons source-element) (cons null) null)))

(defstruct (function-expression (:include source-element))
  (name nil :type (or string null))
  (parameters nil :type (or (cons string) null))
  (body nil :type (or (cons source-element) null)))

;;;; "Administrative lambda" source elements
(defstruct (continuation-function (:include function-expression))
  "A function expression that is used as a continuation")

(defstruct (thunk-function (:include function-expression))
  "A function expression that is used as a thunk in a boxed trampoline result")

(defstruct (continuation-call (:include fn-call))
  "A call to a continuation (as opposed to a call to any other sort of function)")

;;;; JWACS extended syntax  
(defstruct (suspend-statement (:include source-element)))

(defstruct (resume-statement (:include source-element))
  (target nil :type source-element)
  (arg nil :type (or source-element null)))

;;;; Operator precedence and associativity 
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
                    re-literal
                    function-expression)))
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

;;;; Other static properties of source models

;;TODO This function is not complete:
;;TODO is this function in the right place?
;; - binary operators are idempotent so long as they
;;   aren't assignment operators and so long as both args are idempotent.
;; - conditional operators are idempotent so long as all args are idempotent
;; - a small number of unary operators (logical-not, unary-negate, unary-plus, bitwise-not)
;;   are idempotent so long as their arg is idempotent
(defun idempotent-expression-p (elm)
  "Return true if ELM is an 'idempotent expression', ie one which it is
   safe to add repetitions of"
  (typecase elm
    ((or special-value identifier numeric-literal string-literal re-literal)
     t)
    (otherwise
     nil)))

;;;; Convenience functions

(defun make-var-init (var-name init-value)
  "Create a VAR-DECL-STATEMENT that initializes a variable named VAR-NAME to INIT-VALUE"
  (make-var-decl-statement :var-decls
                           (list (make-var-decl :name var-name :initializer init-value))))

(defun single-statement (&rest elm-arguments)
  "Takes a list of source elements and distills them into a single statement.
   If there is only one statement once all the lists have been flattened and
   the statement-blocks pulled apart, then returns that single statement.
   Otherwise, wraps the entire flattened sequence in a statement-block."
  (labels ((combine (elm-arguments)
             "Combine ELM-ARGUMENTS into a single list, stripping out statement-blocks
              if necessary"
             (cond
               ((null elm-arguments)
                nil)
               ((listp (car elm-arguments))
                (append (car elm-arguments)
                        (combine (cdr elm-arguments))))
               ((statement-block-p (car elm-arguments))
                (append (statement-block-statements (car elm-arguments))
                        (combine (cdr elm-arguments))))
               (t
                (cons (car elm-arguments)
                      (combine (cdr elm-arguments)))))))
    (let ((statement-list (combine elm-arguments)))
      (if (> (length statement-list) 1)
        (make-statement-block :statements statement-list)
        (first statement-list)))))

