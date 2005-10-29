(in-package :sugarscript)
  
;;;;; Token definitions
(defmacro deftoken (symbol &optional key token-type)
  (cond
    ((eq token-type :operator)
     `(progn
       (setf (gethash ,key *tokens-to-symbols*) ,symbol)
       (push ,key *operator-tokens*)))
    ((eq token-type :keyword)
     `(progn
       (setf (gethash ,key *tokens-to-symbols*) ,symbol)
       (push ,key *keyword-tokens*)))
    (key
     `(setf (gethash ,key *tokens-to-symbols*) ,symbol))
    (t
     `(setf (gethash ,symbol *tokens-to-symbols*) ,symbol))))

;;;;; Tokens
;;; These are the symbols that the tokenizer will return
(defparameter *tokens-to-symbols* (make-hash-table :test 'equal))

;;; These are operators (as distinct from general tokens) that will be built into
;;; a special regular expression.
(defparameter *operator-tokens* nil)

;;; These are keyword tokens
(defparameter *keyword-tokens* nil)

;; end of input
(deftoken :eoi)

;; Compound assignment operators
(deftoken :times-equals   "*=" :operator)
(deftoken :divide-equals  "/=" :operator)
(deftoken :mod-equals     "%=" :operator)
(deftoken :plus-equals    "+=" :operator)
(deftoken :minus-equals   "-=" :operator)
(deftoken :lshift-equals  "<<=" :operator)
(deftoken :rshift-equals  ">>=" :operator)
(deftoken :urshift-equals ">>>=" :operator)
(deftoken :and-equals     "&=" :operator)
(deftoken :xor-equals     "^=" :operator)
(deftoken :or-equals      "|=" :operator)

;; Operators and punctuators
(deftoken :newline      (format nil "~%"))
(deftoken :semicolon    ";" :operator)
(deftoken :comma        "," :operator)
(deftoken :hook         "?" :operator)
(deftoken :colon        ":" :operator)
(deftoken :conditional)
(deftoken :bar2         "||" :operator)
(deftoken :ampersand2   "&&" :operator)
(deftoken :bar          "|" :operator)
(deftoken :caret        "^" :operator)
(deftoken :ampersand    "&" :operator)
(deftoken :equals3      "===" :operator)
(deftoken :not-equals2  "!==" :operator)
(deftoken :equals2      "==" :operator)
(deftoken :equals       "=" :operator)
(deftoken :not-equals   "!=" :operator)
(deftoken :urshift      ">>>" :operator)
(deftoken :lshift       "<<" :operator)
(deftoken :rshift       ">>" :operator)
(deftoken :less-than    "<" :operator)
(deftoken :less-than-equals "<=" :operator)
(deftoken :greater-than ">" :operator)
(deftoken :greater-than-equals ">=" :operator)

(deftoken :asterisk     "*" :operator)
(deftoken :slash        "/" :operator)
(deftoken :percent      "%" :operator)
(deftoken :bang         "!" :operator)
(deftoken :tilde        "~" :operator)
(deftoken :unary-plus)
(deftoken :unary-minus)
(deftoken :plus2        "++" :operator)
(deftoken :minus2       "--" :operator)
(deftoken :plus         "+" :operator)
(deftoken :minus        "-" :operator)
(deftoken :dot          "." :operator)
(deftoken :left-bracket "[" :operator)
(deftoken :right-bracket "]" :operator)
(deftoken :left-curly   "{" :operator)
(deftoken :right-curly  "}" :operator)
(deftoken :left-paren   "(" :operator)
(deftoken :right-paren  ")" :operator)

;; Keywords
(deftoken :break        "break" :keyword)
(deftoken :case         "case"  :keyword)
(deftoken :catch        "catch" :keyword)
(deftoken :const        "const" :keyword) ;???
(deftoken :continue     "continue" :keyword)
(deftoken :debugger     "debugger" :keyword) ;???
(deftoken :default      "default"  :keyword)
(deftoken :delete       "delete" :keyword)
(deftoken :do           "do"     :keyword)
(deftoken :else         "else"   :keyword)
(deftoken :enum         "enum"   :keyword) ;???
(deftoken :false        "false"  :keyword)
(deftoken :finally      "finally" :keyword) ;???
(deftoken :for          "for"    :keyword)
(deftoken :function     "function" :keyword)
(deftoken :if           "if"     :keyword)
(deftoken :in           "in"     :keyword)
(deftoken :instanceof   "instanceof" :keyword) ;???
(deftoken :new          "new"    :keyword)
(deftoken :null         "null"   :keyword)
(deftoken :return       "return" :keyword)
(deftoken :switch       "switch" :keyword)
(deftoken :this         "this"   :keyword)
(deftoken :throw        "throw"  :keyword)
(deftoken :true         "true"   :keyword)
(deftoken :try          "try"    :keyword)
(deftoken :typeof       "typeof" :keyword)
(deftoken :var          "var"    :keyword)
(deftoken :void         "void"   :keyword) ;???
(deftoken :while        "while"  :keyword)
(deftoken :with         "with"   :keyword)

;; Other terminal types
(deftoken :number)
(deftoken :identifier)
(deftoken :re-literal)
(deftoken :string-literal)

;; Non-terminal tree node types
;;TODO This is not too convincing all of a sudden.  We may want to look for the keywords that actually get used
(deftoken :script)
(deftoken :block)
(deftoken :label)
(deftoken :for-in)
(deftoken :call)
(deftoken :new-with-args)
(deftoken :index)
(deftoken :array-init)
(deftoken :object-init)
(deftoken :property-init)
(deftoken :getter) ;???
(deftoken :setter) ;???
(deftoken :group)
(deftoken :list)


;;;;; Regular expressions
(defparameter floating-re (create-scanner
                            '(:sequence
                              :start-anchor
                              (:alternation
                                (:sequence
                                  (:greedy-repetition 1 nil :digit-class)
                                  #\.
                                  (:greedy-repetition 0 nil :digit-class)
                                  (:greedy-repetition 0 1
                                    (:sequence (:alternation #\e #\E)
                                               (:greedy-repetition 0 1 (:alternation #\+ #\-))
                                               (:greedy-repetition 1 nil :digit-class))))
                                (:sequence
                                  #\.
                                  (:greedy-repetition 1 nil :digit-class)
                                  (:greedy-repetition 0 1
                                    (:sequence (:alternation #\e #\E)
                                               (:greedy-repetition 0 1 (:alternation #\+ #\-))
                                               (:greedy-repetition 1 nil :digit-class))))))))

(defparameter string-re (create-scanner 
                          '(:sequence
                            :start-anchor 
                            (:alternation
                              (:sequence 
                                #\"
                                (:greedy-repetition 0 nil
                                  (:alternation
                                    (:sequence #\\ :everything)
                                    (:inverted-char-class #\")))
                                #\")
                              (:sequence 
                                #\'
                                (:greedy-repetition 0 nil
                                  (:alternation
                                    (:sequence #\\ :everything)
                                    (:inverted-char-class #\')))
                                #\')))))


(defparameter regexp-re (create-scanner
                         '(:sequence
                           :start-anchor
                           #\/
                           (:register
                            (:greedy-repetition 0 nil
                             (:alternation
                              (:sequence #\\ :everything)
                              (:inverted-char-class #\/))))
                           #\/
                           (:register
                            (:greedy-repetition 0 nil
                             (:char-class #\g #\i))))))

(defparameter operator-re (create-scanner 
                           (list :sequence 
                                 :start-anchor 
                                 (cons :alternation
                                       (reverse *operator-tokens*)))))

(defparameter whitespace-and-comments-re (create-scanner
                                          '(:sequence
                                            :start-anchor
                                            (:greedy-repetition 1 nil
                                             (:alternation
                                              (:greedy-repetition 1 nil
                                               :whitespace-char-class)
                                              (:sequence
                                               "//"
                                               (:greedy-repetition 0 nil
                                                (:inverted-char-class #\Newline))
                                               #\Newline)
                                              (:sequence
                                               "/*"
                                               (:greedy-repetition 0 nil
                                                (:branch (:positive-lookahead "*/")
                                                         (:alternation :void :everything)))
                                                "*/"))))))

(defparameter integer-re (create-scanner
                          '(:sequence
                            :start-anchor
                            (:alternation
                             (:sequence "0x" (:greedy-repetition 1 nil 
                                              (:char-class 
                                               (:range #\a #\f) 
                                               (:range #\A #\F) 
                                               :digit-class)))
                             (:sequence "0" (:greedy-repetition 1 nil (:char-class (:range #\0 #\7))))
                             (:greedy-repetition 1 nil :digit-class)))))
  

;;;;; Helper functions
(defun parse-javascript-integer (integer-str &key (start 0) end)
  "Parse integers, taking account of 0x and 0 radix-specifiers"
  (cond
    ((and (> (- (length integer-str) start) 2)
          (eql #\0 (aref integer-str start))
          (eql #\x (aref integer-str (1+ start))))
     (parse-integer integer-str :start (+ start 2) :end end :radix 16 :junk-allowed nil))
    ((scan "^0[0-7]+" integer-str :start start)
     (parse-integer integer-str :start (1+ start) :end end :radix 8 :junk-allowed nil))
    (t
     (parse-integer integer-str :start start :end end :radix 10 :junk-allowed nil))))

(defun unescape-regexp (re-string)
  (regex-replace "\\\\/" re-string "/"))

;;;;; Top-level logic
(defun make-javascript-lexer (string)
  (let ((cursor 0))
    (lambda ()
      ;; Skip whitespace and comments
      (multiple-value-bind (comment-s comment-e)
          (scan whitespace-and-comments-re string :start cursor)
        (if comment-s
          (incf cursor (- comment-e comment-s))))

      ;; Lex a token.  We know that the cursor is at the start of a real token,
      ;; because we just finished skipping all of the whitespace and comments.
      (re-cond (string :start cursor)
       ("^$"
        :eoi)
       (floating-re
        (incf cursor (- %e %s))
        (values :number (read-from-string string nil :eoi :start %s :end %e)))
       (integer-re
        (incf cursor (- %e %s))
        (values :number (parse-javascript-integer string :start %s :end %e)))
       ("^\\w+"
        (incf cursor (- %e %s))
        (let ((token (subseq string %s %e)))
          (if (gethash token *tokens-to-symbols*)
              (values (gethash token *tokens-to-symbols*) token)
              (values :identifier token))))
       (regexp-re
        (incf cursor (- %e %s))
        (values :re-literal (cons (unescape-regexp (subseq string (aref %sub-s 0) (aref %sub-e 0)))
                                   (subseq string (aref %sub-s 1) (aref %sub-e 1)))))
       (string-re
        (incf cursor (- %e %s))
        (values :string-literal (subseq string (1+ %s) (1- %e))))
       (operator-re
        (incf cursor (- %e %s))
        (let ((token (subseq string %s %e)))
          (values (gethash token *tokens-to-symbols*) token)))
       ("^\\S+"
        (error "unrecognized token: '~A'" (subseq string %s %e)))
       (t
        (error "coding error - we should never get here"))))))

;;;;; Auto-tests
(defun test-regexp-re ()
  (and
   (scan regexp-re "/hello/")
   (scan regexp-re "/.\\n/")
   (scan regexp-re "/(this)/g")   
   (scan regexp-re "/(this)/gi")
   (null (scan regexp-re "\"hi\""))
   (null (scan regexp-re "/\"hi\""))))

(defun test-lexer ()
  (let ((js-string-1 "
/* test string */
function (f)
{
    // Ignore this stuff
    var m = 010;
    doStuff('stuff', \"nonsense\", 0xff, 45.0, f(m));
}
")
        (js-string-2 "
var re1 = /hello/g;
var re2 = /hello\\/goodbye/ig;"))
    (and
     (equal
      (loop with l = (make-javascript-lexer js-string-1)
            for x = (multiple-value-list (funcall l))
           while (not (eq (car x) :eoi))
           collect x)
      '((:FUNCTION "function")
        (:LEFT-PAREN "(")
        (:IDENTIFIER "f")
        (:RIGHT-PAREN ")")
        (:LEFT-CURLY "{")
        (:VAR "var")
        (:IDENTIFIER "m")
        (:EQUALS "=")
        (:NUMBER 8)
        (:SEMICOLON ";")
        (:IDENTIFIER "doStuff")
        (:LEFT-PAREN "(")
        (:STRING-LITERAL "stuff")
        (:COMMA ",")
        (:STRING-LITERAL "nonsense")
        (:COMMA ",")
        (:NUMBER 255)
        (:COMMA ",")
        (:NUMBER 45.0)
        (:COMMA ",")
        (:IDENTIFIER "f")
        (:LEFT-PAREN "(")
        (:IDENTIFIER "m")
        (:RIGHT-PAREN ")")
        (:RIGHT-PAREN ")")
        (:SEMICOLON ";")
        (:RIGHT-CURLY "}")))
     (equal
      (loop with l = (make-javascript-lexer js-string-2)
            for x = (multiple-value-list (funcall l))
            while (not (eq (car x) :eoi))
            collect x)
      '((:var "var")
        (:identifier "re1")
        (:equals "=")
        (:re-literal ("hello" . "g"))
        (:semicolon ";")
        (:var "var")
        (:identifier "re2")
        (:equals "=")
        (:re-literal ("hello/goodbye" . "ig"))
        (:semicolon ";"))))))