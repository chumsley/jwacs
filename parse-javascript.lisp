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
  "Use this macro instead of defparser.  It evaluates to an invocation of either
   parsergen:defparser or jwacs::defparser depending on the use-yacc feature."
  #+use-yacc `(jwacs::defparser ,@args)
  #-use-yacc `(parsergen:defparser ,@args))

;;;; Parser
(defparser-generic javascript-script
    ;; Starting production
    ((program source-elements) $1)

  ;; Expressions
  ((primary-expression :this) (make-special-value :symbol :this))
  ((primary-expression :identifier) (make-identifier :name $1))
  ((primary-expression literal) $1)
  ((primary-expression array-literal) $1)
  ((primary-expression object-literal) $1)
  ((primary-expression :left-paren expression :right-paren) $2)
  
  ((literal :null) (make-special-value :symbol :null))
  ((literal boolean-literal) $1)
  ((literal :number) (make-numeric-literal :value $1))
  ((literal :string-literal) (make-string-literal :value $1))
  
  ((boolean-literal :true) (make-special-value :symbol :true))
  ((boolean-literal :false) (make-special-value :symbol :false))
  
  ((array-literal :left-bracket :right-bracket) (make-array-literal :elements nil))
  ((array-literal :left-bracket element-list :right-bracket) (make-array-literal :elements $2))
  ((element-list assignment-expression) (list $1))
  ((element-list element-list :comma assignment-expression) (append $1 (list $3)))
  ;;TODO not currently handling elisions

  ((object-literal :left-curly :right-curly) (make-object-literal :properties nil))
  ((object-literal :left-curly property/value-list :right-curly) (make-object-literal :properties $2))
  
  ((property/value-list property-name :colon assignment-expression) (list (cons $1 $3)))
  ((property/value-list property/value-list :comma property-name :colon assignment-expression) (append $1 (list (cons $3 $5))))

  ((property-name :identifier) (make-identifier :name $1))
  ((property-name :string-literal) (make-string-literal :value $1))
  ((property-name :number) (make-numeric-literal :value $1))

  ;; Pg 55
  ((member-expression primary-expression) $1)
  ((member-expression function-expression) $1)
  ((member-expression member-expression :left-bracket expression :right-bracket) (make-property-access :target $1 :field $3))
  ((member-expression member-expression :dot :identifier) (make-property-access :target $1 :field (make-string-literal :value $3)))
  ((member-expression :new member-expression arguments) (make-new-expr :object-name $2 :args $3))
  
  ((new-expression member-expression) $1)
  ((new-expression :new new-expression) (make-new-expr :object-name $2))

  ((call-expression member-expression arguments) (make-fn-call :fn $1 :args $2))
  ((call-expression call-expression arguments) (make-fn-call :fn $1 :args $2))
  ((call-expression call-expression :left-bracket expression :right-bracket) (make-property-access :target $1 :field $3))
  ((call-expression call-expression :dot :identifier) (make-property-access :target $1 :field (make-string-literal :value $3)))
  
  ((arguments :left-paren :right-paren) nil)
  ((arguments :left-paren argument-list :right-paren) $2)
  
  ((argument-list assignment-expression) (list $1))
  ((argument-list argument-list :comma assignment-expression) (append $1 (list $3)))

  ((left-hand-side-expression new-expression) $1)
  ((left-hand-side-expression call-expression) $1)

  ;; Pg 57
  ((postfix-expression left-hand-side-expression :plus2) (make-unary-operator :op-symbol :post-incr :arg $1))
  ((postfix-expression left-hand-side-expression :minus2) (make-unary-operator :op-symbol :post-decr :arg $1))
  ((postfix-expression left-hand-side-expression) $1) ; the long versions need to be first

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

  ;; Pg 60
  ((multiplicative-expression unary-expression) $1)
  ((multiplicative-expression multiplicative-expression :asterisk unary-expression) (make-binary-operator :op-symbol :multiply :left-arg $1 :right-arg $3))
  ((multiplicative-expression multiplicative-expression :slash unary-expression) (make-binary-operator :op-symbol :divide :left-arg $1 :right-arg $3))
  ((multiplicative-expression multiplicative-expression :percent unary-expression) (make-binary-operator :op-symbol :modulo :left-arg $1 :right-arg $3))
  
  ;; Pg 62
  ((additive-expression multiplicative-expression) $1)
  ((additive-expression additive-expression :plus multiplicative-expression) (make-binary-operator :op-symbol :add :left-arg $1 :right-arg $3))
  ((additive-expression additive-expression :minus multiplicative-expression) (make-binary-operator :op-symbol :subtract :left-arg $1 :right-arg $3))
  
  ;; Pg 63
  ((shift-expression additive-expression) $1)
  ((shift-expression shift-expression :lshift additive-expression) (make-binary-operator :op-symbol :lshift :left-arg $1 :right-arg $3))
  ((shift-expression shift-expression :rshift additive-expression) (make-binary-operator :op-symbol :rshift :left-arg $1 :right-arg $3))
  ((shift-expression shift-expression :urshift additive-expression) (make-binary-operator :op-symbol :urshift :left-arg $1 :right-arg $3))

  ;; Pg 64
  ((relational-expression shift-expression) $1)
  ((relational-expression relational-expression :less-than shift-expression) (make-binary-operator :op-symbol :less-than :left-arg $1 :right-arg $3))
  ((relational-expression relational-expression :less-than-equals shift-expression) (make-binary-operator :op-symbol :less-than-equals :left-arg $1 :right-arg $3))
  ((relational-expression relational-expression :greater-than shift-expression) (make-binary-operator :op-symbol :greater-than :left-arg $1 :right-arg $3))
  ((relational-expression relational-expression :greater-than-equals shift-expression) (make-binary-operator :op-symbol :greater-than-equals :left-arg $1 :right-arg $3))
  ((relational-expression relational-expression :instanceof shift-expression) (make-binary-operator :op-symbol :instanceof :left-arg $1 :right-arg $3))
  ((relational-expression relational-expression :in shift-expression) (make-binary-operator :op-symbol :in :left-arg $1 :right-arg $3))

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
  
  ((equality-expression-no-in relational-expression-no-in) $1)
  ((equality-expression-no-in equality-expression-no-in :equals2 relational-expression-no-in) (make-binary-operator :op-symbol :equals :left-arg $1 :right-arg $3))
  ((equality-expression-no-in equality-expression-no-in :not-equals relational-expression-no-in) (make-binary-operator :op-symbol :not-equals :left-arg $1 :right-arg $3))
  ((equality-expression-no-in equality-expression-no-in :equals3 relational-expression-no-in) (make-binary-operator :op-symbol :strict-equals :left-arg $1 :right-arg $3))
  ((equality-expression-no-in equality-expression-no-in :not-equals2 relational-expression-no-in) (make-binary-operator :op-symbol :strict-not-equals :left-arg $1 :right-arg $3))
 
  ;; Pg 69
  ((bitwise-AND-expression equality-expression) $1)
  ((bitwise-AND-expression bitwise-AND-expression :ampersand equality-expression) (make-binary-operator :op-symbol :bitwise-AND :left-arg $1 :right-arg $3))

  ((bitwise-XOR-expression bitwise-AND-expression) $1)
  ((bitwise-XOR-expression bitwise-XOR-expression :caret bitwise-AND-expression) (make-binary-operator :op-symbol :bitwise-XOR :left-arg $1 :right-arg $3))

  ((bitwise-OR-expression bitwise-XOR-expression) $1)
  ((bitwise-OR-expression bitwise-OR-expression :bar bitwise-XOR-expression) (make-binary-operator :op-symbol :bitwise-OR :left-arg $1 :right-arg $3))

  ((bitwise-AND-expression-no-in equality-expression-no-in) $1)
  ((bitwise-AND-expression-no-in bitwise-AND-expression-no-in :ampersand equality-expression-no-in) (make-binary-operator :op-symbol :bitwise-AND :left-arg $1 :right-arg $3))

  ((bitwise-XOR-expression-no-in bitwise-AND-expression-no-in) $1)
  ((bitwise-XOR-expression-no-in bitwise-XOR-expression-no-in :caret bitwise-AND-expression-no-in) (make-binary-operator :op-symbol :bitwise-XOR :left-arg $1 :right-arg $3))

  ((bitwise-OR-expression-no-in bitwise-XOR-expression-no-in) $1)
  ((bitwise-OR-expression-no-in bitwise-OR-expression-no-in :bar bitwise-XOR-expression-no-in) (make-binary-operator :op-symbol :bitwise-OR :left-arg $1 :right-arg $3))

  ;; Pg 70
  ((logical-AND-expression bitwise-OR-expression) $1)
  ((logical-AND-expression logical-AND-expression :ampersand2 bitwise-OR-expression) (make-binary-operator :op-symbol :logical-AND :left-arg $1 :right-arg $3))

  ((logical-OR-expression logical-AND-expression) $1)
  ((logical-OR-expression logical-OR-expression :bar2 logical-AND-expression) (make-binary-operator :op-symbol :logical-OR :left-arg $1 :right-arg $3))

  ((logical-AND-expression-no-in bitwise-OR-expression-no-in) $1)
  ((logical-AND-expression-no-in logical-AND-expression-no-in :ampersand2 bitwise-OR-expression-no-in) (make-binary-operator :op-symbol :logical-AND :left-arg $1 :right-arg $3))

  ((logical-OR-expression-no-in logical-AND-expression-no-in) $1)
  ((logical-OR-expression-no-in logical-OR-expression-no-in :bar2 logical-AND-expression-no-in) (make-binary-operator :op-symbol :logical-OR :left-arg $1 :right-arg $3))

  ;; Pg 71
  ((conditional-expression logical-OR-expression) $1)
  ((conditional-expression logical-OR-expression :hook assignment-expression :colon assignment-expression) (make-conditional :condition $1 :true-arg $3 :false-arg $5))

  ((conditional-expression-no-in logical-OR-expression-no-in) $1)
  ((conditional-expression logical-OR-expression-no-in :hook assignment-expression :colon assignment-expression-no-in) (make-conditional :condition $1 :true-arg $3 :false-arg $5))

  ((assignment-expression conditional-expression) $1)
  ((assignment-expression left-hand-side-expression assignment-operator assignment-expression)
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
  
  ((expression-no-in assignment-expression-no-in) $1)
  ((expression-no-in expression-no-in :comma assignment-expression-no-in) (list :comma $1 $3))
  
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
  

  ;; Pg 73
  ((block :left-curly statement-list :right-curly) (make-statement-block :statements $2))
  ((block :left-curly :right-curly) (make-statement-block :statements nil))

  ((statement-list statement) (list $1))
  ((statement-list statement-list statement) (append $1 (list $2)))

  ;; Pg 74
  ((variable-statement :var variable-decl-list :semicolon) (make-var-decl-statement :var-decls $2))

  ((variable-decl-list variable-decl) (list $1))
  ((variable-decl-list variable-decl-list :comma variable-decl) (append $1 (list $3)))

  ((variable-decl :identifier) (make-var-decl :name $1))
  ((variable-decl :identifier :equals assignment-expression) (make-var-decl :name $1 :initializer $3))

  ((variable-decl-list-no-in variable-decl-no-in) (list $1))
  ((variable-decl-list-no-in variable-decl-list-no-in :comma variable-decl-no-in) (append $1 (list $3)))

  ((variable-decl-no-in :identifier) (make-var-decl :name $1))
  ((variable-decl-no-in :identifier :equals assignment-expression-no-in) (make-var-decl :name $1 :initializer $3))

  ;; Pg 75
  ((empty-statement :semicolon) nil)

  ((expression-statement expression :semicolon) $1) ;TODO lookahead != function or {

  ((if-statement :if :left-paren expression :right-paren statement :else statement) (make-if-statement :condition $3 :then-statement $5 :else-statement $7))
  ((if-statement :if :left-paren expression :right-paren statement) (make-if-statement :condition $3 :then-statement $5))
  
  ;; Pg 76
  ((iteration-statement :do statement :while :left-paren expression :right-paren :semicolon) (make-do-statement :condition $5 :body $2))
  ((iteration-statement :while :left-paren expression :right-paren statement) (make-while :condition $3 :body $5))

  ;;TODO Almost every expression in a for statement should be optional; add that (probably by writing a utility to calculate it for us)
  ((iteration-statement :for :left-paren expression-no-in :semicolon expression :semicolon expression :right-paren statement)
   (make-for :initializer $3 :condition $5 :step $7 :body $9))
  ((iteration-statement :for :left-paren :var variable-decl-list-no-in :semicolon expression :semicolon expression :right-paren statement)
   (make-for :initializer (make-var-decl-statement :var-decls $4) :condition $6 :step $8 :body $10))
  ((iteration-statement :for :left-paren left-hand-side-expression :in expression :right-paren statement)
   (make-for-in :binding $3 :collection $5 :body $7))
  ((iteration-statement :for :left-paren :var variable-decl-no-in :in expression :right-paren statement)
   (make-for-in :binding (make-var-decl-statement :var-decls (list $4)) :collection $6 :body $8))

  ((continue-statement :continue :identifier :semicolon) (make-continue-statement :label $2))
  ((continue-statement :continue :semicolon) (make-continue-statement))

  ((break-statement :break :identifier :semicolon) (make-break-statement :label $2))
  ((break-statement :break :semicolon) (make-break-statement))

  ((return-statement :return expression :semicolon) (make-return-statement :arg $2))
  ((return-statement :return :semicolon) (make-return-statement))

  ((with-statement :with :left-paren expression :right-paren statement) (make-with :scope-object $3 :body $5))

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

  ((case-clause :case expression :colon statement-list) (make-case-clause :label $2 :body $4))
  ((case-clause :case expression :colon) (make-case-clause :label $2))
  ((case-clause :default :colon statement-list) (make-default-clause :body $3))
  ((case-clause :default :colon) (make-default-clause))

  ;; Pg 81
  ((labelled-statement :identifier :colon statement) (make-label :name $1 :statement $3))

  ((throw-statement :throw expression :semicolon) (make-throw-statement :value $2))

  ((try-statement :try block catch) (make-try :body $2 :catch-clause $3))
  ((try-statement :try block finally) (make-try :body $2 :finally-clause $3))
  ((try-statement :try block catch finally) (make-try :body $2 :catch-clause $3 :finally-clause $4))

  ((catch :catch :left-paren :identifier :right-paren block) (make-catch-clause :binding $3 :body $5))
  ((finally :finally block) (make-finally-clause :body $2))

  ;; Functions (Pg 83)
  ((function-decl :function :identifier :left-paren formal-parameter-list :right-paren :left-curly function-body :right-curly)
   (make-function-decl :name $2 :parameters $4 :body $7))
  ((function-decl :function :identifier :left-paren :right-paren :left-curly function-body :right-curly)
   (make-function-decl :name $2 :body $6))
  
  ((function-expression :function :identifier :left-paren formal-parameter-list :right-paren :left-curly function-body :right-curly)
   (make-function-expression :name $2 :parameters $4 :body $7))
  ((function-expression :function :identifier :left-paren :right-paren :left-curly function-body :right-curly)
   (make-function-expression :name $2 :body $6))
  ((function-expression :function :left-paren formal-parameter-list :right-paren :left-curly function-body :right-curly)
   (make-function-expression :parameters $3 :body $6))
  ((function-expression :function :left-paren :right-paren :left-curly function-body :right-curly)
   (make-function-expression :body $5))

  ((formal-parameter-list :identifier) (list $1))
  ((formal-parameter-list formal-parameter-list :comma :identifier) (append $1 (list $3)))

  ((function-body source-elements) $1)

  ((source-elements source-element) (list $1))
  ((source-elements source-elements source-element) (append $1 (list $2)))

  ((source-element statement) $1)
  ((source-element function-decl) $1)
)

(defun parse (str)
  "Parse a string as a Javascript script, returning a list of statements."
  #+use-yacc (yacc:parse-with-lexer (make-javascript-lexer str) javascript-script)
  #-use-yacc (javascript-script (make-javascript-lexer str)))
#|
;;TODO Move this to test-parser (along with the rest of the tests)
(defun structure-to-plist (maybe-structure)
  "Convert a structure to a list (for easier equality checks).
   The list consists of the structure type followed by a property-list of
   slot names and values.
   Eg: (structure-to-plist (make-label :name foo :statement bar)) ==> (label name foo statement bar)"
  (if (typep maybe-structure 'structure-object)
    (cons (type-of maybe-structure)
          (loop for slot in (structure-slots maybe-structure)
                nconc (list slot (slot-value maybe-structure slot))))
    maybe-structure))

;;TODO Tests need to be updated to account for conversion to structure source-model instead
;; of plists
(defun test-broken ()
  (flet ((check (string form)
           (format t "~&~S -> ~S" string (parse string))
           (equal form (parse string))))
    (and
     (check "x = {a:10};" '(#S(object-literal :properties (#S(identifier :name "a") #S(numeric-literal :value 10)))))
     t)))

(defun test-parser ()
  (flet ((check (string form)
           #+nil(format t "~&~S -> ~S" string (parse string))
           (equal form (parse string))))
    (and
     (check "var x = y[44];" 
            '(((:var "x" (:property-access (:identifier "y") (:number 44))))))
     (check "var x = y.z;" 
            '(((:var "x" (:property-access (:identifier "y") (:string-literal "z"))))))
     (check "var x = new ObjectName(10, 20);" 
            '(((:var "x" (:new (:identifier "ObjectName") ((:number 10) (:number 20)))))))
     (check "var x = new 'strtype';" 
            '(((:var "x" (:new (:string-literal "strtype"))))))
     (check "var x = (new 'strtype').field[20];" 
            '(((:var "x" 
               (:property-access 
                (:property-access 
                 (:new (:string-literal "strtype")) 
                 (:string-literal "field")) 
                (:number 20))))))
     (check "var/*x*/varName=func(0x8);" '(((:var "varName" (:call (:identifier "func") ((:number 8)))))))
     (check "var varName=func(0x8, 'str')['sam'].a;" 
            '(((:var "varName" 
               (:property-access
                (:property-access 
                 (:call (:identifier "func") ((:number 8) (:string-literal "str"))) 
                 (:string-literal "sam"))
                (:string-literal "a"))))))
     (check "var x = 10 * 10 / 20;" 
            '(((:var "x" (:divide (:multiply (:number 10) (:number 10)) (:number 20))))))
     (check "var x = 10 * 2 + 3;" 
            '(((:var "x" (:add (:multiply (:number 10) (:number 2)) (:number 3))))))
     (check "var x = 3+10 * 2 ;" 
            '(((:var "x" (:add (:number 3) (:multiply (:number 10) (:number 2)))))))
     (check "var x = 10 << (99 - 50) * 10;" 
            '(((:var "x" (:lshift (:number 10) (:multiply (:subtract (:number 99) (:number 50)) (:number 10)))))))
     (check "var x = a & b | c ^ d;"
            '(((:var "x" (:bitwise-or (:bitwise-and (:identifier "a") (:identifier "b")) (:bitwise-xor (:identifier "c") (:identifier "d")))))))
     (check "var x = a && b || c && d;"
            '(((:var "x" (:logical-or (:logical-and (:identifier "a") (:identifier "b")) (:logical-and (:identifier "c") (:identifier "d")))))))
     (check "x == y;" '((:equals (:identifier "x") (:identifier "y"))))
     (check "x = y;" '((:assign (:identifier "x") (:identifier "y"))))
     (check "delete x++;" '((:delete (:post-incr (:identifier "x")))))
     (check "return;" '((:return)))
     (check "return 8 >> 2;" '((:return (:rshift (:number 8) (:number 2)))))
     (check "continue;" '((:continue)))
     (check "continue blockName;" '((:continue "blockName")))
     (check "break blockName;" '((:break "blockName")))
     (check "x += 50;" '((:plus-equals (:identifier "x") (:number 50))))
     (check "with(x.y) { z -= 10; }" '((:with (:property-access (:identifier "x") (:string-literal "y"))
                                      ((:minus-equals (:identifier "z") (:number 10))))))
     (check "switch(x[y]) { case 10:case 20:return x << 1;default:case 88:break; }"
            '((:switch (:property-access (:identifier "x") (:identifier "y"))
              ((:case (:number 10) nil)
               (:case (:number 20) ((:return (:lshift (:identifier "x") (:number 1)))))
               (:default nil)
               (:case (:number 88) ((:break)))))))

     (check "{hello: x += 20;x*=10;}" '(((:label "hello" (:plus-equals (:identifier "x") (:number 20)))
                                        (:times-equals (:identifier "x") (:number 10)))))
     
     (check "fcn ( arg );" '((:call (:identifier "fcn") ((:identifier "arg")))))
     (check "new fcn;" '((:new (:identifier "fcn"))))
     (check "new fcn (ahoy1, ahoy2);" '((:new (:identifier "fcn") ((:identifier "ahoy1") (:identifier "ahoy2")))))
     (check "throw -1;" '((:throw (:unary-minus (:number 1)))))
     (check "try { throw x++; } catch(y) {return y;}" '((:try
                                                        ((:throw (:post-incr (:identifier "x"))))
                                                        (:catch "y" ((:return (:identifier "y")))))))
     (check "try {throw 10;} finally {delete x;delete y;}" '((:try
                                                             ((:throw (:number 10)))
                                                             nil
                                                             (:finally ((:delete (:identifier "x"))
                                                                        (:delete (:identifier "y")))))))
     (check "try {func(x);} catch(e) {} finally {delete x;}" '((:try
                                                               ((:call (:identifier "func") ((:identifier "x"))))
                                                               (:catch "e" nil)
                                                               (:finally ((:delete (:identifier "x")))))))
                                                             
     (check "if(x == 10) {y=100;} else {y = null;}" '((:if (:equals (:identifier "x") (:number 10)) ((:assign (:identifier "y") (:number 100))) ((:assign (:identifier "y") (:null))))))
     (check "function foo(x) { if(x == 20) return 10; return 55;}"
            '((:function-decl "foo" ("x") ((:if (:equals (:identifier "x") (:number 20)) (:return (:number 10))) (:return (:number 55))))))
     (check "var x = function f(n) { if(n > 0) return n*f(n-1); else return 1;};"
            '(((:var "x" (:function-expression "f" ("n") ((:if (:greater-than (:identifier "n") (:number 0)) (:return (:multiply (:identifier "n") (:call (:identifier "f") ((:subtract (:identifier "n") (:number 1)))))) (:return (:number 1)))))))))
     (check "function(n) { if(n > 0) return n*f(n-1); else return 1;};"
            '((:function-expression nil ("n")
               ((:if (:greater-than (:identifier "n") (:number 0))
                     (:return (:multiply (:identifier "n") (:call (:identifier "f") ((:subtract (:identifier "n") (:number 1))))))
                     (:return (:number 1)))))))
     (check "function make_adder(n) { return function(x) { return x + n;};} make_adder(20);"
            '((:function-decl "make_adder" ("n") ((:return (:function-expression nil ("x") ((:return (:add (:identifier "x") (:identifier "n"))))))))
              (:call (:identifier "make_adder") ((:number 20)))))
     )))
|#
