;;;; parse-javascript-yacc.lisp
;;;
;;; Use the cl-yacc package to parse javascript source text.
;;;
;;; Copyright (c) 2005 Greg Smolyn
;;; See LICENSE for full licensing details.
;;;
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
;		   (:print-derives-epsilon t)
;		   (:print-first-terminals t)
;		   (:print-states t)
;		   (:print-goto-graph t)
;		   (:print-lookaheads )
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
                   (setf output (cons (append (list k) (reverse v)) output)))
               production-map)
      (reverse output))))


; go through every symbol

(defun replace-dollar-signs (item)
  "If item is a symbol or list, replaces $n with (nth n-1 expr).
   If item is neither, returns it unchanged."
  (cond
    ((null item) nil)
    ((consp item) (cons (replace-dollar-signs (car item))
                        (replace-dollar-signs (cdr item))))
    ((symbolp item)
     (replace-dollar-sign-in-symbol item))
    (t
     item)))

(defun replace-dollar-sign-in-symbol (sym)
  "Replace $n in a symbol with (nth n-1 expr)"
  (let* ((symname (symbol-name sym)))
    (if (not (eq (char symname 0) #\$))
      sym
      `(nth ,(1- (parse-integer (subseq symname 1))) expr))))

