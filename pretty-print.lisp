;; pretty-print.lisp
;; Print an abstract syntax tree for Javascript in a nicely-formatted fashion
(in-package :jwacs)

;;TODO I don't have any indentation handling yet.  Once we are printing a parseable
;; representation, a fairly crucial piece of back-fill will be to go back and add
;; indentation.

(defun pretty-print-separated-list (elm-list s &optional (sep-string ", "))
  (loop
      for idx upfrom 0
      for elm in elm-list
      do
      (unless (zerop idx)
        (format s sep-string))
      (pretty-print elm s)))

(defmethod pretty-print ((element special-value) s)
  ;; ??? Would a hash table make more sense here?
  (ecase (special-value-symbol element)
    (:this "this")
    (:null "null")
    (:true "true")
    (:false "false")
    (:undefined "undefined")))

(defmethod pretty-print ((element identifier) s)
  (format s "~A" (identifier-name element)))

(defmethod pretty-print ((element number) s)
  (format s "~D" (number-value element)))

(defmethod pretty-print ((element string-literal) s)
  (format s "~S" (string-literal-value element)))

(defmethod pretty-print ((element array-literal) s)
  (format s "[")
  (pretty-print-separated-list (array-literal-elements element) s)
  (format s "]"))

;; TODO May want some extra smarts here for things like class defns
;; ie, add newlines after each property if any of the properties are of type function-expression
(defmethod pretty-print ((element object-literal) s)
  (format s "{")
  (loop for idx upfrom 0
        for name/value in (object-literal-properties element)
        do
        (unless (zerop idx)
          (format s ", "))
        (pretty-print (car name/value) s)
        (format s ": ")
        (pretty-print (cdr name/value) s))
  (format s "}"))

(defmethod pretty-print ((element new-expr) s)
  (cond
    ((null (new-expr-args element))
     (format s "new ~A" (new-expr-object-name element)))
    (t
     (format s "new ~A (")
     (pretty-print-separated-list (new-expr-args element) s))))

(defmethod pretty-print ((element fn-call) s)
  (pretty-print (fn-call-fn element) s)
  (format s "(")
  (pretty-print-separated-list (fn-call-args element) s)
  (format s ")"))

(defun printable-as-dot (string-literal-elm)
  (not (null (scan "^\\w+$" (string-literal-value string-literal-elm)))))

(defmethod pretty-print ((element property-access) s)
  (pretty-print (property-access-target element) s)
  (cond
    ((printable-as-dot (property-access-field element))
     (format s ".~A" (string-literal-value (property-access-field element))))
    (t
     (format s "[")
     (pretty-print (property-access-field element) s)
     (format s "]"))))

;;HERE precedence handling
;(defmethod pretty-print ((element unary-operator) s)
;  (case (unary-operator-op-symbol element)
;    ((:post-decr :post-incr)
;     (pretty-print (unary-operator-arg) s)
;     ;;HERE