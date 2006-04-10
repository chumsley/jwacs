;;;; parse-javascript.lisp
;;;
;;; Define a parser for javascript.  The returned parse tree will
;;; be built up from the data types declared in js-source-model.lisp.
;;; The parser is exposed through the PARSE function.
;;;
;;; The grammar for the parser is based on The ECMAScript Language Specification (ECMA-262),
;;; available at <http://www.ecma-international.org/publications/standards/Ecma-262.htm>.

(in-package :jwacs)

;;;; Cross-compiler configuration
(defmacro defparser-generic (&rest args)
  "Use this macro instead of DEFPARSER.  It evaluates to an invocation of either
   PARSERGEN:DEFPARSER or JWACS::DEFPARSER depending on the use-yacc feature."
  #+use-yacc `(jwacs::defparser ,@args)
  #-use-yacc `(parsergen:defparser ,@args))

;;;; Parser

(defparameter undefined-id (make-identifier :name "undefined")
  "Contains the `undefined` identifier")

;;TODO Get rid of this stupid variable by fixing the DEFPARSER-GENERIC macro
(defparameter one 1
  "The macro that we use to translate parsergen grammars to cl-yacc grammars
   only currently deals with symbols and lists.  So we'll use this variable for 
   now instead of the (non-symbol, non-list) number 1.")

(defparser-generic javascript-script
    
    ((program source-elements) $1) ; Starting production

  ;; Expressions
  ((primary-expression :this) (make-special-value :symbol :this))
  ((primary-expression :function_continuation) (make-special-value :symbol :function_continuation))
  ((primary-expression :identifier) (make-identifier :name $1))
  ((primary-expression literal) $1)
  ((primary-expression array-literal) $1)
  ((primary-expression object-literal) $1)
  ((primary-expression :left-paren expression :right-paren) $2)

  ((primary-expression-no-lbf :this) (make-special-value :symbol :this))
  ((primary-expression-no-lbf :function_continuation) (make-special-value :symbol :function_continuation))
  ((primary-expression-no-lbf :identifier) (make-identifier :name $1))
  ((primary-expression-no-lbf literal) $1)
  ((primary-expression-no-lbf array-literal) $1)
  ((primary-expression-no-lbf :left-paren expression :right-paren) $2)
  
  ((array-literal :left-bracket :right-bracket) (make-array-literal :elements nil))
  ((array-literal :left-bracket elision :right-bracket) (make-array-literal :elements (make-list (1+ $2) :initial-element undefined-id)))
  ((array-literal :left-bracket element-list :right-bracket) (make-array-literal :elements $2))
  ((array-literal :left-bracket element-list elision :right-bracket) (make-array-literal :elements
                                                                                         (append $2 (make-list $3 :initial-element undefined-id))))

  ((element-list assignment-expression) (list $1))
  ((element-list elision assignment-expression) (append (make-list $1 :initial-element undefined-id)
                                                        (list $2)))
  ((element-list element-list :comma assignment-expression) (append $1 (list $3)))
  ((element-list element-list :comma elision assignment-expression) (append $1
                                                                            (make-list $3 :initial-element undefined-id)
                                                                            (list $4)))
  
  ((elision :comma) one)
  ((elision elision :comma) (1+ $1))

  ((object-literal :left-curly :right-curly) (make-object-literal :properties nil))
  ((object-literal :left-curly property/value-list :right-curly) (make-object-literal :properties $2))
  
  ((property/value-list property-name :colon assignment-expression) (list (cons $1 $3)))
  ((property/value-list property/value-list :comma property-name :colon assignment-expression) (append $1 (list (cons $3 $5))))

  ((property-name :identifier) (make-string-literal :value $1))
  ((property-name :string-literal) (make-string-literal :value $1))
  ((property-name :number) (make-numeric-literal :value $1))

  ;; Pg 55
  ((member-expression primary-expression) $1)
  ((member-expression function-expression) $1)
  ((member-expression member-expression :left-bracket expression :right-bracket) (make-property-access :target $1 :field $3))
  ((member-expression member-expression :dot :identifier) (make-property-access :target $1 :field (make-string-literal :value $3)))
  ((member-expression :new member-expression arguments) (make-new-expr :constructor $2 :args $3))

  ((member-expression-no-lbf primary-expression-no-lbf) $1)
  ((member-expression-no-lbf member-expression-no-lbf :left-bracket expression :right-bracket) (make-property-access :target $1 :field $3))
  ((member-expression-no-lbf member-expression-no-lbf :dot :identifier) (make-property-access :target $1 :field (make-string-literal :value $3)))
  ((member-expression-no-lbf :new member-expression arguments) (make-new-expr :constructor $2 :args $3))

  ((new-expression member-expression) $1)
  ((new-expression :new new-expression) (make-new-expr :constructor $2))

  ((new-expression-no-lbf member-expression-no-lbf) $1)
  ((new-expression-no-lbf :new new-expression) (make-new-expr :constructor $2))


  ((call-expression member-expression arguments) (make-fn-call :fn $1 :args $2))
  ((call-expression call-expression arguments) (make-fn-call :fn $1 :args $2))
  ((call-expression call-expression :left-bracket expression :right-bracket) (make-property-access :target $1 :field $3))
  ((call-expression call-expression :dot :identifier) (make-property-access :target $1 :field (make-string-literal :value $3)))

  ((call-expression-no-lbf member-expression-no-lbf arguments) (make-fn-call :fn $1 :args $2))
  ((call-expression-no-lbf call-expression-no-lbf arguments) (make-fn-call :fn $1 :args $2))
  ((call-expression-no-lbf call-expression-no-lbf :left-bracket expression :right-bracket) (make-property-access :target $1 :field $3))
  ((call-expression-no-lbf call-expression-no-lbf :dot :identifier) (make-property-access :target $1 :field (make-string-literal :value $3)))
  
  
  ((arguments :left-paren :right-paren) nil)
  ((arguments :left-paren argument-list :right-paren) $2)
  
  ((argument-list assignment-expression) (list $1))
  ((argument-list argument-list :comma assignment-expression) (append $1 (list $3)))

  ((left-hand-side-expression new-expression) $1)
  ((left-hand-side-expression call-expression) $1)

  ((left-hand-side-expression-no-lbf new-expression-no-lbf) $1)
  ((left-hand-side-expression-no-lbf call-expression-no-lbf) $1)

  ;; Pg 57
  ((postfix-expression left-hand-side-expression :plus2) (make-unary-operator :op-symbol :post-incr :arg $1))
  ((postfix-expression left-hand-side-expression :minus2) (make-unary-operator :op-symbol :post-decr :arg $1))
  ((postfix-expression left-hand-side-expression) $1) ; the long versions need to be first

  ((postfix-expression-no-lbf left-hand-side-expression-no-lbf :plus2) (make-unary-operator :op-symbol :post-incr :arg $1))
  ((postfix-expression-no-lbf left-hand-side-expression-no-lbf :minus2) (make-unary-operator :op-symbol :post-decr :arg $1))
  ((postfix-expression-no-lbf left-hand-side-expression-no-lbf) $1) ; the long versions need to be first
; TODO plus2 == incr_no_lt

  ;; Pg 58
  ((unary-expression postfix-expression) $1)
  ((unary-expression :delete unary-expression) (make-unary-operator :op-symbol :delete :arg $2))
  ((unary-expression :void unary-expression) (make-unary-operator :op-symbol :void :arg $2))
  ((unary-expression :typeof unary-expression) (make-unary-operator :op-symbol :typeof :arg $2))
  ((unary-expression :plus2 unary-expression) (make-unary-operator :op-symbol :pre-incr :arg $2))
  ((unary-expression :minus2 unary-expression) (make-unary-operator :op-symbol :pre-decr :arg $2))
  ((unary-expression :plus unary-expression) (make-unary-operator :op-symbol :unary-plus :arg $2))
  ((unary-expression :minus unary-expression) (make-unary-operator :op-symbol :unary-minus :arg $2))
  ((unary-expression :tilde unary-expression) (make-unary-operator :op-symbol :bitwise-not :arg $2))
  ((unary-expression :bang unary-expression) (make-unary-operator :op-symbol :logical-not :arg $2))

  ((unary-expression-no-lbf postfix-expression-no-lbf) $1)
  ((unary-expression-no-lbf :delete unary-expression) (make-unary-operator :op-symbol :delete :arg $2))
  ((unary-expression-no-lbf :void unary-expression) (make-unary-operator :op-symbol :void :arg $2))
  ((unary-expression-no-lbf :typeof unary-expression) (make-unary-operator :op-symbol :typeof :arg $2))
  ((unary-expression-no-lbf :plus2 unary-expression) (make-unary-operator :op-symbol :pre-incr :arg $2))
  ((unary-expression-no-lbf :minus2 unary-expression) (make-unary-operator :op-symbol :pre-decr :arg $2))
  ((unary-expression-no-lbf :plus unary-expression) (make-unary-operator :op-symbol :unary-plus :arg $2))
  ((unary-expression-no-lbf :minus unary-expression) (make-unary-operator :op-symbol :unary-minus :arg $2))
  ((unary-expression-no-lbf :tilde unary-expression) (make-unary-operator :op-symbol :bitwise-not :arg $2))
  ((unary-expression-no-lbf :bang unary-expression) (make-unary-operator :op-symbol :logical-not :arg $2))

  ;; Pg 60
  ((multiplicative-expression unary-expression) $1)
  ((multiplicative-expression multiplicative-expression :asterisk unary-expression) (make-binary-operator :op-symbol :multiply :left-arg $1 :right-arg $3))
  ((multiplicative-expression multiplicative-expression :slash unary-expression) (make-binary-operator :op-symbol :divide :left-arg $1 :right-arg $3))
  ((multiplicative-expression multiplicative-expression :percent unary-expression) (make-binary-operator :op-symbol :modulo :left-arg $1 :right-arg $3))

  ((multiplicative-expression-no-lbf unary-expression-no-lbf) $1)
  ((multiplicative-expression-no-lbf multiplicative-expression-no-lbf :asterisk unary-expression) (make-binary-operator :op-symbol :multiply :left-arg $1 :right-arg $3))
  ((multiplicative-expression-no-lbf multiplicative-expression-no-lbf :slash unary-expression) (make-binary-operator :op-symbol :divide :left-arg $1 :right-arg $3))
  ((multiplicative-expression-no-lbf multiplicative-expression-no-lbf :percent unary-expression) (make-binary-operator :op-symbol :modulo :left-arg $1 :right-arg $3))  

  ;; Pg 62
  ((additive-expression multiplicative-expression) $1)
  ((additive-expression additive-expression :plus multiplicative-expression) (make-binary-operator :op-symbol :add :left-arg $1 :right-arg $3))
  ((additive-expression additive-expression :minus multiplicative-expression) (make-binary-operator :op-symbol :subtract :left-arg $1 :right-arg $3))

  ((additive-expression-no-lbf multiplicative-expression-no-lbf) $1)
  ((additive-expression-no-lbf additive-expression-no-lbf :plus multiplicative-expression) (make-binary-operator :op-symbol :add :left-arg $1 :right-arg $3))
  ((additive-expression-no-lbf additive-expression-no-lbf :minus multiplicative-expression) (make-binary-operator :op-symbol :subtract :left-arg $1 :right-arg $3))
  
  ;; Pg 63
  ((shift-expression additive-expression) $1)
  ((shift-expression shift-expression :lshift additive-expression) (make-binary-operator :op-symbol :lshift :left-arg $1 :right-arg $3))
  ((shift-expression shift-expression :rshift additive-expression) (make-binary-operator :op-symbol :rshift :left-arg $1 :right-arg $3))
  ((shift-expression shift-expression :urshift additive-expression) (make-binary-operator :op-symbol :urshift :left-arg $1 :right-arg $3))

  ((shift-expression-no-lbf additive-expression-no-lbf) $1)
  ((shift-expression-no-lbf shift-expression-no-lbf :lshift additive-expression) (make-binary-operator :op-symbol :lshift :left-arg $1 :right-arg $3))
  ((shift-expression-no-lbf shift-expression-no-lbf :rshift additive-expression) (make-binary-operator :op-symbol :rshift :left-arg $1 :right-arg $3))
  ((shift-expression-no-lbf shift-expression-no-lbf :urshift additive-expression) (make-binary-operator :op-symbol :urshift :left-arg $1 :right-arg $3))

  ;; Pg 64
  ((relational-expression shift-expression) $1)
  ((relational-expression relational-expression :less-than shift-expression) (make-binary-operator :op-symbol :less-than :left-arg $1 :right-arg $3))
  ((relational-expression relational-expression :less-than-equals shift-expression) (make-binary-operator :op-symbol :less-than-equals :left-arg $1 :right-arg $3))
  ((relational-expression relational-expression :greater-than shift-expression) (make-binary-operator :op-symbol :greater-than :left-arg $1 :right-arg $3))
  ((relational-expression relational-expression :greater-than-equals shift-expression) (make-binary-operator :op-symbol :greater-than-equals :left-arg $1 :right-arg $3))
  ((relational-expression relational-expression :instanceof shift-expression) (make-binary-operator :op-symbol :instanceof :left-arg $1 :right-arg $3))
  ((relational-expression relational-expression :in shift-expression) (make-binary-operator :op-symbol :in :left-arg $1 :right-arg $3))

  ((relational-expression-no-lbf shift-expression-no-lbf) $1)
  ((relational-expression-no-lbf relational-expression-no-lbf :less-than shift-expression) (make-binary-operator :op-symbol :less-than :left-arg $1 :right-arg $3))
  ((relational-expression-no-lbf relational-expression-no-lbf :less-than-equals shift-expression) (make-binary-operator :op-symbol :less-than-equals :left-arg $1 :right-arg $3))
  ((relational-expression-no-lbf relational-expression-no-lbf :greater-than shift-expression) (make-binary-operator :op-symbol :greater-than :left-arg $1 :right-arg $3))
  ((relational-expression-no-lbf relational-expression-no-lbf :greater-than-equals shift-expression) (make-binary-operator :op-symbol :greater-than-equals :left-arg $1 :right-arg $3))
  ((relational-expression-no-lbf relational-expression-no-lbf :instanceof shift-expression) (make-binary-operator :op-symbol :instanceof :left-arg $1 :right-arg $3))
  ((relational-expression-no-lbf relational-expression-no-lbf :in shift-expression) (make-binary-operator :op-symbol :in :left-arg $1 :right-arg $3))


  ;; (to avoid confusing the `in` operator in for-in expressions)
  ((relational-expression-no-in shift-expression) $1)
  ((relational-expression-no-in relational-expression-no-in :less-than shift-expression) (make-binary-operator :op-symbol :less-than :left-arg $1 :right-arg $3))
  ((relational-expression-no-in relational-expression-no-in :less-than-equals shift-expression) (make-binary-operator :op-symbol :less-than-equals :left-arg $1 :right-arg $3))
  ((relational-expression-no-in relational-expression-no-in :greater-than shift-expression) (make-binary-operator :op-symbol :greater-than :left-arg $1 :right-arg $3))
  ((relational-expression-no-in relational-expression-no-in :greater-than-equals shift-expression) (make-binary-operator :op-symbol :greater-than-equals :left-arg $1 :right-arg $3))
  ((relational-expression-no-in relational-expression-no-in :instanceof shift-expression) (make-binary-operator :op-symbol :instanceof :left-arg $1 :right-arg $3))

  ;; Pg 67
  ((equality-expression relational-expression) $1)
  ((equality-expression equality-expression :equals2 relational-expression) (make-binary-operator :op-symbol :equals :left-arg $1 :right-arg $3))
  ((equality-expression equality-expression :not-equals relational-expression) (make-binary-operator :op-symbol :not-equals :left-arg $1 :right-arg $3))
  ((equality-expression equality-expression :equals3 relational-expression) (make-binary-operator :op-symbol :strict-equals :left-arg $1 :right-arg $3))
  ((equality-expression equality-expression :not-equals2 relational-expression) (make-binary-operator :op-symbol :strict-not-equals :left-arg $1 :right-arg $3))

  ((equality-expression-no-lbf relational-expression-no-lbf) $1)
  ((equality-expression-no-lbf equality-expression-no-lbf :equals2 relational-expression) (make-binary-operator :op-symbol :equals :left-arg $1 :right-arg $3))
  ((equality-expression-no-lbf equality-expression-no-lbf :not-equals relational-expression) (make-binary-operator :op-symbol :not-equals :left-arg $1 :right-arg $3))
  ((equality-expression-no-lbf equality-expression-no-lbf :equals3 relational-expression) (make-binary-operator :op-symbol :strict-equals :left-arg $1 :right-arg $3))
  ((equality-expression-no-lbf equality-expression-no-lbf :not-equals2 relational-expression) (make-binary-operator :op-symbol :strict-not-equals :left-arg $1 :right-arg $3))
  
  ((equality-expression-no-in relational-expression-no-in) $1)
  ((equality-expression-no-in equality-expression-no-in :equals2 relational-expression-no-in) (make-binary-operator :op-symbol :equals :left-arg $1 :right-arg $3))
  ((equality-expression-no-in equality-expression-no-in :not-equals relational-expression-no-in) (make-binary-operator :op-symbol :not-equals :left-arg $1 :right-arg $3))
  ((equality-expression-no-in equality-expression-no-in :equals3 relational-expression-no-in) (make-binary-operator :op-symbol :strict-equals :left-arg $1 :right-arg $3))
  ((equality-expression-no-in equality-expression-no-in :not-equals2 relational-expression-no-in) (make-binary-operator :op-symbol :strict-not-equals :left-arg $1 :right-arg $3))
 
  ;; Pg 69
  ((bitwise-AND-expression equality-expression) $1)
  ((bitwise-AND-expression bitwise-AND-expression :ampersand equality-expression) (make-binary-operator :op-symbol :bitwise-AND :left-arg $1 :right-arg $3))

  ((bitwise-AND-expression-no-lbf equality-expression-no-lbf) $1)
  ((bitwise-AND-expression-no-lbf bitwise-AND-expression-no-lbf :ampersand equality-expression) (make-binary-operator :op-symbol :bitwise-AND :left-arg $1 :right-arg $3))

  ((bitwise-AND-expression-no-in equality-expression-no-in) $1)
  ((bitwise-AND-expression-no-in bitwise-AND-expression-no-in :ampersand equality-expression-no-in) (make-binary-operator :op-symbol :bitwise-AND :left-arg $1 :right-arg $3))

  ((bitwise-XOR-expression bitwise-AND-expression) $1)
  ((bitwise-XOR-expression bitwise-XOR-expression :caret bitwise-AND-expression) (make-binary-operator :op-symbol :bitwise-XOR :left-arg $1 :right-arg $3))

  ((bitwise-XOR-expression-no-lbf bitwise-AND-expression-no-lbf) $1)
  ((bitwise-XOR-expression-no-lbf bitwise-XOR-expression-no-lbf :caret bitwise-AND-expression) (make-binary-operator :op-symbol :bitwise-XOR :left-arg $1 :right-arg $3))

  ((bitwise-XOR-expression-no-in bitwise-AND-expression-no-in) $1)
  ((bitwise-XOR-expression-no-in bitwise-XOR-expression-no-in :caret bitwise-AND-expression-no-in) (make-binary-operator :op-symbol :bitwise-XOR :left-arg $1 :right-arg $3))

  ((bitwise-OR-expression bitwise-XOR-expression) $1)
  ((bitwise-OR-expression bitwise-OR-expression :bar bitwise-XOR-expression) (make-binary-operator :op-symbol :bitwise-OR :left-arg $1 :right-arg $3))

  ((bitwise-OR-expression-no-lbf bitwise-XOR-expression-no-lbf) $1)
  ((bitwise-OR-expression-no-lbf bitwise-OR-expression-no-lbf :bar bitwise-XOR-expression) (make-binary-operator :op-symbol :bitwise-OR :left-arg $1 :right-arg $3))

  ((bitwise-OR-expression-no-in bitwise-XOR-expression-no-in) $1)
  ((bitwise-OR-expression-no-in bitwise-OR-expression-no-in :bar bitwise-XOR-expression-no-in) (make-binary-operator :op-symbol :bitwise-OR :left-arg $1 :right-arg $3))

  ;; Pg 70
  ((logical-AND-expression bitwise-OR-expression) $1)
  ((logical-AND-expression logical-AND-expression :ampersand2 bitwise-OR-expression) (make-binary-operator :op-symbol :logical-AND :left-arg $1 :right-arg $3))

  ((logical-AND-expression-no-lbf bitwise-OR-expression-no-lbf) $1)
  ((logical-AND-expression-no-lbf logical-AND-expression-no-lbf :ampersand2 bitwise-OR-expression) (make-binary-operator :op-symbol :logical-AND :left-arg $1 :right-arg $3))

  ((logical-AND-expression-no-in bitwise-OR-expression-no-in) $1)
  ((logical-AND-expression-no-in logical-AND-expression-no-in :ampersand2 bitwise-OR-expression-no-in) (make-binary-operator :op-symbol :logical-AND :left-arg $1 :right-arg $3))

  ((logical-OR-expression logical-AND-expression) $1)
  ((logical-OR-expression logical-OR-expression :bar2 logical-AND-expression) (make-binary-operator :op-symbol :logical-OR :left-arg $1 :right-arg $3))

  ((logical-OR-expression-no-lbf logical-AND-expression-no-lbf) $1)
  ((logical-OR-expression-no-lbf logical-OR-expression-no-lbf :bar2 logical-AND-expression) (make-binary-operator :op-symbol :logical-OR :left-arg $1 :right-arg $3))
 
  ((logical-OR-expression-no-in logical-AND-expression-no-in) $1)
  ((logical-OR-expression-no-in logical-OR-expression-no-in :bar2 logical-AND-expression-no-in) (make-binary-operator :op-symbol :logical-OR :left-arg $1 :right-arg $3))

  ;; Pg 71
  ((conditional-expression logical-OR-expression) $1)
  ((conditional-expression logical-OR-expression :hook assignment-expression :colon assignment-expression) (make-conditional :condition $1 :true-arg $3 :false-arg $5))

  ((conditional-expression-no-lbf logical-OR-expression-no-lbf) $1)
  ((conditional-expression-no-lbf logical-OR-expression-no-lbf :hook assignment-expression :colon assignment-expression) (make-conditional :condition $1 :true-arg $3 :false-arg $5))

  ((conditional-expression-no-in logical-OR-expression-no-in) $1)
  ((conditional-expression logical-OR-expression-no-in :hook assignment-expression :colon assignment-expression-no-in) (make-conditional :condition $1 :true-arg $3 :false-arg $5))

  ((assignment-expression conditional-expression) $1)
  ((assignment-expression left-hand-side-expression assignment-operator assignment-expression)
   (let ((op (gethash $2 *tokens-to-symbols*)))
     (if (eq op :equals)
       (make-binary-operator :op-symbol :assign :left-arg $1 :right-arg $3)
       (make-binary-operator :op-symbol op :left-arg $1 :right-arg $3))))

  ((assignment-expression-no-lbf conditional-expression-no-lbf) $1)
  ((assignment-expression-no-lbf left-hand-side-expression-no-lbf assignment-operator assignment-expression)
   (let ((op (gethash $2 *tokens-to-symbols*)))
     (if (eq op :equals)
	 (make-binary-operator :op-symbol :assign :left-arg $1 :right-arg $3)
	 (make-binary-operator :op-symbol op :left-arg $1 :right-arg $3))))
  
  ((assignment-expression-no-in conditional-expression-no-in) $1)
  ((assignment-expression-no-in left-hand-side-expression assignment-operator assignment-expression-no-in)
   (let ((op (gethash $2 *tokens-to-symbols*)))
     (if (eq op :equals)
       (make-binary-operator :op-symbol :assign :left-arg $1 :right-arg $3)
       (make-binary-operator :op-symbol op :left-arg $1 :right-arg $3))))
  
  ((assignment-operator :equals) $1)
  ((assignment-operator :times-equals) $1)
  ((assignment-operator :divide-equals) $1)
  ((assignment-operator :mod-equals) $1)
  ((assignment-operator :plus-equals) $1)
  ((assignment-operator :minus-equals) $1)
  ((assignment-operator :lshift-equals) $1)
  ((assignment-operator :rshift-equals) $1)
  ((assignment-operator :urshift-equals) $1)
  ((assignment-operator :and-equals) $1)
  ((assignment-operator :xor-equals) $1)
  ((assignment-operator :or-equals) $1)
  
  ((expression assignment-expression) $1)
  ((expression expression :comma assignment-expression)
   (if (comma-expr-p $1)
     (make-comma-expr :exprs (append (comma-expr-exprs $1) (list $3)))
     (make-comma-expr :exprs (list $1 $3))))

  ((expression-no-lbf assignment-expression-no-lbf) $1)
  ((expression-no-lbf expression-no-lbf :comma assignment-expression)
   (if (comma-expr-p $1)
     (make-comma-expr :exprs (append (comma-expr-exprs $1) (list $3)))
     (make-comma-expr :exprs (list $1 $3))))

  ((expression-no-in assignment-expression-no-in) $1)
  ((expression-no-in expression-no-in :comma assignment-expression-no-in)
   (if (comma-expr-p $1)
     (make-comma-expr :exprs (append (comma-expr-exprs $1) (list $3)))
     (make-comma-expr :exprs (list $1 $3))))   

  ((literal :null) (make-special-value :symbol :null))
  ((literal boolean-literal) $1)
  ((literal :number) (make-numeric-literal :value $1))
  ((literal :string-literal) (make-string-literal :value $1))
  ((literal :re-literal) (make-re-literal :pattern (car $1) :options (cdr $1)))
  
  ((boolean-literal :true) (make-special-value :symbol :true))
  ((boolean-literal :false) (make-special-value :symbol :false))  

  ;; Statements
  ((statement block) $1)
  ((statement variable-statement) $1)
  ((statement empty-statement) $1)
  ((statement expression-statement) $1)
  ((statement if-statement) $1)
  ((statement iteration-statement) $1)
  ((statement continue-statement) $1)
  ((statement break-statement) $1)
  ((statement return-statement) $1)
  ((statement with-statement) $1)
  ((statement labelled-statement) $1)
  ((statement switch-statement) $1)
  ((statement throw-statement) $1)
  ((statement try-statement) $1)
  ((statement suspend-statement) $1)    ; jwacs-only extensions
  ((statement resume-statement) $1)
  
  ((statement-no-if block) $1)
  ((statement-no-if variable-statement) $1)
  ((statement-no-if empty-statement) $1)
  ((statement-no-if expression-statement) $1)
  ((statement-no-if if-statement-no-if) $1)
  ((statement-no-if iteration-statement-no-if) $1)
  ((statement-no-if continue-statement) $1)
  ((statement-no-if break-statement) $1)
  ((statement-no-if return-statement) $1)
  ((statement-no-if with-statement-no-if) $1)
  ((statement-no-if labelled-statement-no-if) $1)
  ((statement-no-if switch-statement) $1)
  ((statement-no-if throw-statement) $1)
  ((statement-no-if try-statement) $1)
  ((statement-no-if suspend-statement) $1)    ; jwacs-only extensions
  ((statement-no-if resume-statement) $1)

  ;; Pg 73
  ((block :left-curly statement-list :right-curly) (make-statement-block :statements $2))
  ((block :left-curly :right-curly) (make-statement-block :statements nil))

  ((statement-list statement) (list $1))
  ((statement-list statement-list statement) (append $1 (list $2)))

  ;; Pg 74
  ((variable-statement :var variable-decl-list :semicolon) (make-var-decl-statement :var-decls $2))

  ((variable-decl-list variable-decl) (list $1))
  ((variable-decl-list variable-decl-list :comma variable-decl) (append $1 (list $3)))

  ((variable-decl-list-no-in variable-decl-no-in) (list $1))
  ((variable-decl-list-no-in variable-decl-list-no-in :comma variable-decl-no-in) (append $1 (list $3)))

  ((variable-decl :identifier) (make-var-decl :name $1))
  ((variable-decl :identifier :equals assignment-expression) (make-var-decl :name $1 :initializer $3))

  ((variable-decl-no-in :identifier) (make-var-decl :name $1))
  ((variable-decl-no-in :identifier :equals assignment-expression-no-in) (make-var-decl :name $1 :initializer $3))

  ;; Pg 75
  ((empty-statement :semicolon) nil)

  ((expression-statement expression-no-lbf :semicolon) $1)


  ((if-statement :if :left-paren expression :right-paren statement-no-if :else statement) (make-if-statement :condition $3 :then-statement $5 :else-statement $7))
  ((if-statement :if :left-paren expression :right-paren statement) (make-if-statement :condition $3 :then-statement $5))

  ((if-statement-no-if :if :left-paren expression :right-paren statement-no-if :else statement-no-if) (make-if-statement :condition $3 :then-statement $5 :else-statement $7))
  

  ;; Pg 76
  ((iteration-statement :do statement :while :left-paren expression :right-paren :semicolon) (make-do-statement :condition $5 :body $2))
  ((iteration-statement :while :left-paren expression :right-paren statement) (make-while :condition $3 :body $5))

  ;; (generate-rules-with-optional '(iteration-statement :for :left-paren ?expression-no-in :semicolon ?expression :semicolon ?expression :right-paren statement))
  ((iteration-statement :for :left-paren :semicolon :semicolon :right-paren statement)
   (make-for :body $6))
  ((iteration-statement :for :left-paren expression-no-in :semicolon :semicolon :right-paren statement)
   (make-for :initializer $3 :body $7))
  ((iteration-statement :for :left-paren :semicolon expression :semicolon :right-paren statement)
   (make-for :condition $4 :body $7))
  ((iteration-statement :for :left-paren expression-no-in :semicolon expression :semicolon :right-paren statement)
   (make-for :initializer $3 :condition $5 :body $8))
  ((iteration-statement :for :left-paren :semicolon :semicolon expression :right-paren statement)
   (make-for :step $5 :body $7))
  ((iteration-statement :for :left-paren expression-no-in :semicolon :semicolon expression :right-paren statement)
   (make-for :initializer $3 :step $6 :body $8))
  ((iteration-statement :for :left-paren :semicolon expression :semicolon expression :right-paren statement)
   (make-for :condition $4 :step $6 :body $8))
  ((iteration-statement :for :left-paren expression-no-in :semicolon expression :semicolon expression :right-paren statement)
   (make-for :initializer $3 :condition $5 :step $7 :body $9))

  ;; (generate-rules-with-optional '(iteration-statement :for :left-paren :var variable-decl-list-no-in :semicolon ?expression :semicolon ?expression :right-paren statement))
  ((iteration-statement :for :left-paren :var variable-decl-list-no-in :semicolon :semicolon :right-paren statement)
   (make-for :initializer (make-var-decl-statement :var-decls $4) :body $8))
  ((iteration-statement :for :left-paren :var variable-decl-list-no-in :semicolon expression :semicolon :right-paren statement)
   (make-for :initializer (make-var-decl-statement :var-decls $4) :condition $6 :body $9))
  ((iteration-statement :for :left-paren :var variable-decl-list-no-in :semicolon :semicolon expression :right-paren statement)
   (make-for :initializer (make-var-decl-statement :var-decls $4) :step $7 :body $9))
  ((iteration-statement :for :left-paren :var variable-decl-list-no-in :semicolon expression :semicolon expression :right-paren statement)
   (make-for :initializer (make-var-decl-statement :var-decls $4) :condition $6 :step $8 :body $10))

  ((iteration-statement :for :left-paren left-hand-side-expression :in expression :right-paren statement)
   (make-for-in :binding $3 :collection $5 :body $7))
  ((iteration-statement :for :left-paren :var variable-decl-no-in :in expression :right-paren statement)
   (make-for-in :binding (make-var-decl-statement :var-decls (list $4)) :collection $6 :body $8))

  ((iteration-statement-no-if :do statement :while :left-paren expression :right-paren :semicolon) (make-do-statement :condition $5 :body $2))
  ((iteration-statement-no-if :while :left-paren expression :right-paren statement-no-if) (make-while :condition $3 :body $5))

  ;; (generate-rules-with-optional '(iteration-statement :for :left-paren ?expression-no-in :semicolon ?expression :semicolon ?expression :right-paren statement))
  ((iteration-statement-no-if :for :left-paren :semicolon :semicolon :right-paren statement-no-if)
   (make-for :body $6))
  ((iteration-statement-no-if :for :left-paren expression-no-in :semicolon :semicolon :right-paren statement-no-if)
   (make-for :initializer $3 :body $7))
  ((iteration-statement-no-if :for :left-paren :semicolon expression :semicolon :right-paren statement-no-if)
   (make-for :condition $4 :body $7))
  ((iteration-statement-no-if :for :left-paren expression-no-in :semicolon expression :semicolon :right-paren statement-no-if)
   (make-for :initializer $3 :condition $5 :body $8))
  ((iteration-statement-no-if :for :left-paren :semicolon :semicolon expression :right-paren statement-no-if)
   (make-for :step $5 :body $7))
  ((iteration-statement-no-if :for :left-paren expression-no-in :semicolon :semicolon expression :right-paren statement-no-if)
   (make-for :initializer $3 :step $6 :body $8))
  ((iteration-statement-no-if :for :left-paren :semicolon expression :semicolon expression :right-paren statement-no-if)
   (make-for :condition $4 :step $6 :body $8))
  ((iteration-statement-no-if :for :left-paren expression-no-in :semicolon expression :semicolon expression :right-paren statement-no-if)
   (make-for :initializer $3 :condition $5 :step $7 :body $9))

  ;; (generate-rules-with-optional '(iteration-statement :for :left-paren :var variable-decl-list-no-in :semicolon ?expression :semicolon ?expression :right-paren statement))
  ((iteration-statement-no-if :for :left-paren :var variable-decl-list-no-in :semicolon :semicolon :right-paren statement-no-if)
   (make-for :initializer (make-var-decl-statement :var-decls $4) :body $8))
  ((iteration-statement-no-if :for :left-paren :var variable-decl-list-no-in :semicolon expression :semicolon :right-paren statement-no-if)
   (make-for :initializer (make-var-decl-statement :var-decls $4) :condition $6 :body $9))
  ((iteration-statement-no-if :for :left-paren :var variable-decl-list-no-in :semicolon :semicolon expression :right-paren statement-no-if)
   (make-for :initializer (make-var-decl-statement :var-decls $4) :step $7 :body $9))
  ((iteration-statement-no-if :for :left-paren :var variable-decl-list-no-in :semicolon expression :semicolon expression :right-paren statement-no-if)
   (make-for :initializer (make-var-decl-statement :var-decls $4) :condition $6 :step $8 :body $10))

  ((iteration-statement-no-if :for :left-paren left-hand-side-expression :in expression :right-paren statement-no-if)
   (make-for-in :binding $3 :collection $5 :body $7))
  ((iteration-statement-no-if :for :left-paren :var variable-decl-no-in :in expression :right-paren statement-no-if)
   (make-for-in :binding (make-var-decl-statement :var-decls (list $4)) :collection $6 :body $8))

  ((continue-statement :continue :identifier :semicolon) (make-continue-statement :target-label $2))
  ((continue-statement :continue :semicolon) (make-continue-statement))

  ((break-statement :break :identifier :semicolon) (make-break-statement :target-label $2))
  ((break-statement :break :semicolon) (make-break-statement))

  ((return-statement :return expression :semicolon) (make-return-statement :arg $2))
  ((return-statement :return :semicolon) (make-return-statement))

  ((with-statement :with :left-paren expression :right-paren statement) (make-with :scope-object $3 :body $5))

  ((with-statement-no-if :with :left-paren expression :right-paren statement-no-if) (make-with :scope-object $3 :body $5))

  ;; Note that by treating the default clause as just another type of case clause, as opposed
  ;; to as a distinct non-terminal, we lose the ability to guarantee at the parser level that
  ;; there will be only a single default clause per switch statement.  However, we gain the
  ;; freedom from having to play all of the optionality games that will be required to allow
  ;; default clauses at any stage in the clause-block, so this is a sacrifice that I am happy
  ;; to make at this stage.
  ((switch-statement :switch :left-paren expression :right-paren case-block) (make-switch :value $3 :clauses $5))
  ((case-block :left-curly case-clauses :right-curly) $2)
  ((case-block :left-curly :right-curly) nil)

  ((case-clauses case-clause) (list $1))
  ((case-clauses case-clauses case-clause) (append $1 (list $2)))

  ((case-clause :case expression :colon statement-list) (make-case-clause :value $2 :body $4))
  ((case-clause :case expression :colon) (make-case-clause :value $2))
  ((case-clause :default :colon statement-list) (make-default-clause :body $3))
  ((case-clause :default :colon) (make-default-clause))

  ;; Pg 81
  ((labelled-statement :identifier :colon statement)
   (let ((elm $3))
     (setf (source-element-label elm) $1)
     elm))
  ((labelled-statement-no-if :identifier :colon statement-no-if)
   (let ((elm $3))
     (setf (source-element-label elm) $1)
     elm))

  ((throw-statement :throw expression :semicolon) (make-throw-statement :value $2))

  ((try-statement :try block catch) (make-try :body (statement-block-statements $2) :catch-clause $3))
  ((try-statement :try block finally) (make-try :body (statement-block-statements $2) :finally-clause $3))
  ((try-statement :try block catch finally) (make-try :body (statement-block-statements $2) :catch-clause $3 :finally-clause $4))

  ((catch :catch :left-paren :identifier :right-paren block) (make-catch-clause :binding $3 :body (statement-block-statements $5)))
  ((finally :finally block) (make-finally-clause :body (statement-block-statements $2)))

  ;; jwacs extended syntax
  ((suspend-statement :suspend :semicolon) (make-suspend-statement))
  ((resume-statement :resume left-hand-side-expression-no-lbf :semicolon) (make-resume-statement :target $2))
  ((resume-statement :resume left-hand-side-expression-no-lbf :left-arrow expression :semicolon) (make-resume-statement :target $2 :arg $4))

  ;; Functions (Pg 83)
  ((function-decl :function :identifier :left-paren formal-parameter-list :right-paren function-body)
   (make-function-decl :name $2 :parameters $4 :body $6))
  ((function-decl :function :identifier :left-paren :right-paren function-body)
   (make-function-decl :name $2 :body $5))
  
  ((function-expression :function :identifier :left-paren formal-parameter-list :right-paren function-body)
   (make-function-expression :name $2 :parameters $4 :body $6))
  ((function-expression :function :identifier :left-paren :right-paren function-body)
   (make-function-expression :name $2 :body $5))
  ((function-expression :function :left-paren formal-parameter-list :right-paren function-body)
   (make-function-expression :parameters $3 :body $5))
  ((function-expression :function :left-paren :right-paren function-body)
   (make-function-expression :body $4))

  ((formal-parameter-list :identifier) (list $1))
  ((formal-parameter-list formal-parameter-list :comma :identifier) (append $1 (list $3)))

  ((function-body :left-curly source-elements :right-curly) $2)
  ((function-body :left-curly :right-curly) nil)

  ((source-elements source-element) (list $1))
  ((source-elements source-elements source-element) (append $1 (list $2)))

  ((source-element statement) $1)
  ((source-element function-decl) $1)
)

;;;; ======= Helper functions ======================================================================

(defun generate-rules-with-optional (rule-spec &optional (mask 0))
  "Accepts a PARSERGEN rulespec and transforms it by treating each symbol that begins with a
   #\\? character as being optional.  The output is a list of one or more rules that
   exhaust all the present/absent possibilities.  This is an internal utility for generating
   rules for the different grammar rules that are specified with wildcards."
  (labels ((optional-p (symbol)
             (char= #\? (aref (symbol-name symbol) 0)))
           (strip-optional (symbol)
             (if (optional-p symbol)
               (intern (subseq (symbol-name symbol) 1)
                       (symbol-package symbol))
               symbol)))
    (let* ((optionals-count (loop for symbol in rule-spec
                                  count (optional-p symbol)))
           (end-mask (expt 2 optionals-count)))
      (unless (>= mask end-mask)
        (cons
         (loop for symbol in rule-spec
               for symbol-index upfrom 0
               count (optional-p symbol) into optional-index
               unless (and (optional-p symbol)
                           (zerop (logand (expt 2 (1- optional-index)) mask)))
               collect (strip-optional symbol))
         (generate-rules-with-optional rule-spec (1+ mask)))))))

;;;; ======= Public interface ======================================================================
(defun parse (str)
  "Parse a string as a Javascript script, returning a list of statements."
  #+use-yacc (yacc:parse-with-lexer (make-javascript-lexer str) javascript-script)
  #-use-yacc (javascript-script (make-javascript-lexer str)))
