;;;; lex-javascript.lisp
;;;
;;; Contains the definition of the Javascript lexer used by the parser,
;;; as well as some lookup structures for dealing with tokens.
;;; Unit tests are in tests/test-lexer.lisp.

(in-package :jwacs)

;;;; Token definitions
(defmacro deftoken (symbol &optional key token-type)
  "Add a token's symbol and possibly string to the appropriate lookups.

   Different actions will be taken depending upon the TOKEN-TYPE:
   OPERATOR-TOKENs are infix operators, which are recognized as atomic
   tokens regardless of whether they are surrounded by whitespace (unlike
   identifier tokens, for example).
   KEYWORDs are reserved strings that can never be used as an identifier.
   OPERATIONs will never be returned by the lexer, but they are used in the
   source model in the same place as tokens (ie, in :op-symbol slots), so
   they are treated as tokens for now."
  (cond
    ((eq token-type :operator-token)
     `(progn
       (setf (gethash ,key *tokens-to-symbols*) ,symbol)
       (setf (gethash ,symbol *symbols-to-tokens*) ,key)
       (push ,key *operator-tokens*)))
    ((eq token-type :operation)
     `(setf (gethash ,symbol *symbols-to-tokens*) ,key))
    ((eq token-type :keyword)
     `(progn
       (setf (gethash ,key *tokens-to-symbols*) ,symbol)
       (push ,symbol *keyword-symbols*)))
    (key
     `(setf (gethash ,key *tokens-to-symbols*) ,symbol))
    (t
     `(setf (gethash ,symbol *tokens-to-symbols*) ,symbol))))

;;;; Token lookups

(defparameter *tokens-to-symbols* (make-hash-table :test 'equal)
  "Map from string to token symbol.
   Every symbol that the tokenizer will return is in this map.")

(defparameter *symbols-to-tokens* (make-hash-table :test 'eq)
  "Map from token symbol to token string.  This contains an entry for every token
   in *tokens-to-symbols*, plus additional entries for the 'operation' symbols.")

(defparameter *operator-tokens* nil
  "These are operators (as distinct from general tokens) that will be built into
   a special regular expression.

   We can't just use the keys from *symbols-to-tokens*, because the
   order that the tokens appear in this list is significant.
   Specifically, each 'long' operator must occur before any operator that
   it is a supersequence of.  Eg, '<<<' must occur before '<<', which
   must occur before '<'.  '!=' must occur before both '!' and '='.")

(defparameter *keyword-symbols* nil
  "A list of the keyword symbols.")

;; end of input
(deftoken :eoi)

;; Compound assignment operators
(deftoken :times-equals   "*=" :operator-token)
(deftoken :divide-equals  "/=" :operator-token)
(deftoken :mod-equals     "%=" :operator-token)
(deftoken :plus-equals    "+=" :operator-token)
(deftoken :minus-equals   "-=" :operator-token)
(deftoken :lshift-equals  "<<=" :operator-token)
(deftoken :rshift-equals  ">>=" :operator-token)
(deftoken :urshift-equals ">>>=" :operator-token)
(deftoken :and-equals     "&=" :operator-token)
(deftoken :xor-equals     "^=" :operator-token)
(deftoken :or-equals      "|=" :operator-token)

;; Operators and punctuators
(deftoken :newline      (format nil "~%"))
(deftoken :semicolon    ";" :operator-token)
(deftoken :comma        "," :operator-token)
(deftoken :hook         "?" :operator-token)
(deftoken :colon        ":" :operator-token)
(deftoken :conditional)
(deftoken :bar2         "||" :operator-token)
(deftoken :ampersand2   "&&" :operator-token)
(deftoken :bar          "|" :operator-token)
(deftoken :caret        "^" :operator-token)
(deftoken :ampersand    "&" :operator-token)
(deftoken :equals3      "===" :operator-token)
(deftoken :not-equals2  "!==" :operator-token)
(deftoken :equals2      "==" :operator-token)
(deftoken :equals       "=" :operator-token)
(deftoken :not-equals   "!=" :operator-token)
(deftoken :urshift      ">>>" :operator-token)
(deftoken :lshift       "<<" :operator-token)
(deftoken :rshift       ">>" :operator-token)
(deftoken :less-than    "<" :operator-token)
(deftoken :less-than-equals "<=" :operator-token)
(deftoken :greater-than ">" :operator-token)
(deftoken :greater-than-equals ">=" :operator-token)

(deftoken :asterisk     "*" :operator-token)
(deftoken :slash        "/" :operator-token)
(deftoken :percent      "%" :operator-token)
(deftoken :bang         "!" :operator-token)
(deftoken :tilde        "~" :operator-token)
(deftoken :plus2        "++" :operator-token)
(deftoken :minus2       "--" :operator-token)
(deftoken :plus         "+" :operator-token)
(deftoken :minus        "-" :operator-token)
(deftoken :dot          "." :operator-token)
(deftoken :left-bracket "[" :operator-token)
(deftoken :right-bracket "]" :operator-token)
(deftoken :left-curly   "{" :operator-token)
(deftoken :right-curly  "}" :operator-token)
(deftoken :left-paren   "(" :operator-token)
(deftoken :right-paren  ")" :operator-token)

;; These represent operations (addition, etc.) rather than just text tokens.
;; The lexer will never output them, so possibly the whole *symbols-to-tokens*
;; setup wants to be in js-source-model instead of here.
(deftoken :assign       "=" :operation)
(deftoken :unary-plus   "+" :operation)
(deftoken :unary-minus  "-" :operation)
(deftoken :pre-decr     "--" :operation)
(deftoken :pre-incr     "++" :operation)
(deftoken :post-decr    "--" :operation)
(deftoken :post-incr    "++" :operation)
(deftoken :logical-and  "&&" :operation)
(deftoken :logical-or   "||" :operation)
(deftoken :logical-not  "!" :operation)
(deftoken :bitwise-and  "&" :operation)
(deftoken :bitwise-or   "|" :operation)
(deftoken :bitwise-not  "~" :operation)
(deftoken :bitwise-xor  "^" :operation)
(deftoken :equals       "==" :operation)
(deftoken :strict-equals "===" :operation)
(deftoken :strict-not-equals "!==" :operation)
(deftoken :add          "+" :operation)
(deftoken :subtract     "-" :operation)
(deftoken :multiply     "*" :operation)
(deftoken :divide       "/" :operation)
(deftoken :modulo       "%" :operation)


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
(deftoken :undefined    "undefined" :keyword) ;???
(deftoken :var          "var"    :keyword)
(deftoken :void         "void"   :keyword) ;???
(deftoken :while        "while"  :keyword)
(deftoken :with         "with"   :keyword)

;; Other terminal types
(deftoken :number)
(deftoken :identifier)
(deftoken :re-literal)
(deftoken :string-literal)

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
                                               (:greedy-repetition 1 nil :digit-class)))))))

  "Regular expression for recognizing floating-point literals")

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
                                #\'))))

  "Regular expression for recognizing string literals")

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
                             (:char-class #\g #\i)))))

  "(Lisp) regular expression for recognizing (Javascript) regular expression literals")

(defparameter operator-re (create-scanner 
                           (list :sequence 
                                 :start-anchor 
                                 (cons :alternation
                                       (reverse *operator-tokens*))))

  "Regular expression for recognizing operators")

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
                                                "*/")))))

  "Regular expression for consuming (and thereby skipping) whitespace and comments")

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
                             (:greedy-repetition 1 nil :digit-class))))

  "Regular expression for recognizing integer literals")
  

;;;;; Helper functions
(defun parse-javascript-integer (integer-str &key (start 0) end)
  "Parse integer literals, taking account of 0x and 0 radix-specifiers"
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
  "Construct and return a new lexer that reads from the provided string."
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
