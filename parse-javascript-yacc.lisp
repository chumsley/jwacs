;;;; parse-javascript-yacc.lisp
;;;
;;; Use the cl-yacc package to parse javascript source text.

(in-package :jwacs)

(defun expand-hashtable-to-values (hashtable)
  "Returns a list of all the values stored in a hashtable."
  (let ((valuelist '()))
    (maphash #'(lambda (k v) 
                 (declare (ignore k))
                 (setf valuelist (cons v valuelist)))
             hashtable)
    valuelist))

; need to collect productions 

(defmacro defparser (parser-name starting-production &body productions)
  "This macro emulates the Lispworks parsergenerator's defparser macro, but instead creates output
   for CL-YACC"
  (let* ((starting-point (first starting-production))
         (starting-symbol (first starting-point))
         (header `(yacc:define-parser ,parser-name
                   (:muffle-conflicts ,jwacs-system::*muffle-conflicts*)
                   (:start-symbol ,starting-symbol)
                   (:terminals ,(expand-hashtable-to-values *tokens-to-symbols* ))
                   (:precedence nil)
                   ,starting-point)))
    (append header (generate-productions productions))))

; here we turn
;  ((primary-expression object-literal) $1)
;   into 
;  (primary-expression
;      (object-literal #'(lambda (&rest expr) (nth 0 expr))))
;
; and
;
; ((literal :number) (make-numeric-literal :value $1))
;  into
; (literal
;   (:number #'(lambda (&rest expr) (make-numeric-literal :value (nth 0 expr)))))



(defun generate-productions (productions)
  "Used by defparser macro. Take the lispworks list of productions and convert them into
   CL-YACC versions"
  (let* ((production-map (make-hash-table)))
    (dolist (production productions)lisp convert int to char
            (let* ((rule (nth 0 production))
                   (action (replace-dollar-signs (nth 1 production)))
                   (rule-name (first rule))
                   (rule-terminals (rest rule)))
              (setf (gethash rule-name production-map) 
                    (cons (append rule-terminals `(#'(lambda (&rest expr) ,action)))
                          (gethash rule-name production-map)))))
    (let* ((output '()))
      (maphash #'(lambda (k v) 
                   (setf output (cons (append (list k) v) output)))
               production-map)
      output)))


; go through every symbol

(defun replace-dollar-signs (item)
  "Takes a symbol or a list and replaces $n with (nth n-1 expr)"
  (cond
    ((null item) nil)
    ((not (listp item))
     (replace-dollar-sign-in-symbol item))
    (t (cons (replace-dollar-signs (car item))
             (replace-dollar-signs (cdr item))))))

(defun replace-dollar-sign-in-symbol (sym)
  "Replace $n in a symbol with (nth n-1 expr)"
  (if (not (eq (char (symbol-name sym) 0) #\$))
      sym
      `(nth ,(- (char-int (char (symbol-name sym) 1)) (char-int #\1))expr)))
