;-*-syntax:COMMON-LISP;Package:(RT :use "COMMON-LISP" :colon-mode :external)-*-

#|----------------------------------------------------------------------------|
 | Copyright 1990 by the Massachusetts Institute of Technology, Cambridge MA. |
 |                                                                            |
 | Permission  to  use,  copy, modify, and distribute this software  and  its |
 | documentation for any purpose  and without fee is hereby granted, provided |
 | that this copyright  and  permission  notice  appear  in  all  copies  and |
 | supporting  documentation,  and  that  the  name  of M.I.T. not be used in |
 | advertising or  publicity  pertaining  to  distribution  of  the  software |
 | without   specific,   written   prior   permission.      M.I.T.  makes  no |
 | representations  about  the  suitability of this software for any purpose. |
 | It is provided "as is" without express or implied warranty.                |
 |                                                                            |
 |  M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,  INCLUDING  |
 |  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL  |
 |  M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAMAGES  OR  |
 |  ANY  DAMAGES  WHATSOEVER  RESULTING  FROM  LOSS OF USE, DATA OR PROFITS,  |
 |  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER  TORTIOUS  ACTION,  |
 |  ARISING  OUT  OF  OR  IN  CONNECTION WITH THE USE OR PERFORMANCE OF THIS  |
 |  SOFTWARE.                                                                 |
 |----------------------------------------------------------------------------|#

(defpackage #:regression-test
  (:nicknames #:rtest #-lispworks #:rt) 
  (:use #:cl)
  (:export #:*do-tests-when-defined* #:*test* #:continue-testing
           #:deftest #:do-test #:do-tests #:get-test #:pending-tests
           #:rem-all-tests #:rem-test
           #:defnote #:enable-note #:disable-note #:enable-all-notes #:disable-all-notes)
  (:documentation "The MIT regression tester with pfdietz's modifications & later JWACS modifications"))

;;This was the December 19, 1990 version of the regression tester, but
;;has since been modified.

;;;; Additional changes since being added to jwacs project:

;;; James Wright Nov 11/2005:
;;;   - Added rt.lisp to external/
;;;   - Converted rt.lisp from CR/LF to LF-only
;;;   - Added DEFNOTE, ENABLE-NOTE, DISABLE-NOTE to list of exported symbols

;;; James Wright Nov 13/2005:
;;;   - DO-TESTS now returns 2 values.  First value is T if no tests fail,
;;;     second value is T if no tests unexpectedly fail.

;;; James Wright Nov 19/2005:
;;;   - We now take account of the :EQUALP-FN parameter to DEFTEST for specifying
;;;     the equality test to use (synonyms :EQUAL-FN and :TEST also work)
;;;   - EQUALP-WITH-CASE now works properly for structure values

;;; James Wright Nov 20/2005:
;;;   - Changed from using the STRUCTURE type to using the STRUCTURE-OBJECT type
;;;     in EQUALP-WITH-CASE (Lispworks has both, but SBCL has only STRUCTURE-OBJECT)

;;; James Wright Nov 21/2005:
;;;   - The STRUCTURE case for EQUALP-WITH-CASE now checks that the TYPE-OF for the
;;;     two values are EQ as well as the set of slots.

;;; James Wright Aug 25/2006:
;;;   - Added ENABLE-ALL-NOTES and DISABLE-ALL-NOTES

(in-package :regression-test)

(declaim (ftype (function (t) t) get-entry expanded-eval do-entries))
(declaim (type list *entries*))
(declaim (ftype (function (t &rest t) t) report-error))
(declaim (ftype (function (t &optional t) t) do-entry))

(defvar *test* nil "Current test name")
(defvar *do-tests-when-defined* nil)
(defvar *entries* '(nil) "Test database.  Has a leading dummy cell that does not contain an entry.")
(defvar *entries-tail* *entries* "Tail of the *entries* list")
(defvar *entries-table* (make-hash-table :test #'equal)
  "Map the names of entries to the cons cell in *entries* that precedes the one whose car is the entry.")
(defvar *in-test* nil "Used by TEST")
(defvar *debug* nil "For debugging")
(defvar *catch-errors* t "When true, causes errors in a test to be caught.")
(defvar *print-circle-on-failure* nil
  "Failure reports are printed with *PRINT-CIRCLE* bound to this value.")

(defvar *compile-tests* nil "When true, compile the tests before running them.")
(defvar *expanded-eval* nil "When true, convert the tests into a form that is less likely to have compiler optimizations.")
(defvar *optimization-settings* '((safety 3)))

(defvar *expected-failures* nil
  "A list of test names that are expected to fail.")

(defvar *notes* (make-hash-table :test 'equal)
  "A mapping from names of notes to note objects.")
  
(defstruct (entry (:conc-name nil))
  pend name props form vals)

;;; Note objects are used to attach information to tests.
;;; A typical use is to mark tests that depend on a particular
;;; part of a set of requirements, or a particular interpretation
;;; of the requirements.

(defstruct note
  name  
  contents
  disabled ;; When true, tests with this note are considered inactive
  )

;; (defmacro vals (entry) `(cdddr ,entry))

(defmacro defn (entry)
  (let ((var (gensym)))
    `(let ((,var ,entry))
      (list* (name ,var) (form ,var) (vals ,var)))))

(defun entry-notes (entry)
  (let* ((props (props entry))
         (notes (getf props :notes)))
    (if (listp notes)
      notes
      (list notes))))

(defun has-disabled-note (entry)
  (let ((notes (entry-notes entry)))
    (loop for n in notes
          for note = (if (note-p n) n
                       (gethash n *notes*))
          thereis (and note (note-disabled note)))))

(defun equalp-fn (entry)
  (let ((props (props entry)))
    (or
     (getf props :test)
     (getf props :equal-fn)
     (getf props :equalp-fn 'equalp-with-case))))

(defun pending-tests ()
  (loop for entry in (cdr *entries*)
        when (and (pend entry) (not (has-disabled-note entry)))
        collect (name entry)))

(defun rem-all-tests ()
  (setq *entries* (list nil))
  (setq *entries-tail* *entries*)
  (clrhash *entries-table*)
  nil)

(defun rem-test (&optional (name *test*))
  (let ((pred (gethash name *entries-table*)))
    (when pred
      (if (null (cddr pred))
        (setq *entries-tail* pred)
        (setf (gethash (name (caddr pred)) *entries-table*) pred))
      (setf (cdr pred) (cddr pred))
      (remhash name *entries-table*)
      name)))

(defun get-test (&optional (name *test*))
  (defn (get-entry name)))

(defun get-entry (name)
  (let ((entry;; (find name (the list (cdr *entries*))
	       ;;     :key #'name :test #'equal)
         (cadr (gethash name *entries-table*))
          ))
    (when (null entry)
      (report-error t
                    "~%No test with name ~:@(~S~)."
                    name))
    entry))

(defmacro deftest (name &rest body)
  (let* ((p body)
         (properties
          (loop while (keywordp (first p))
            unless (cadr p)
            do (error "Poorly formed deftest: ~A~%"
                      (list* 'deftest name body))
            append (list (pop p) (pop p))))
         (form (pop p))
         (vals p))
    `(add-entry (make-entry :pend t
                 :name ',name
                 :props ',properties
                 :form ',form
                 :vals ',vals))))

(defun add-entry (entry)
  (setq entry (copy-entry entry))
  (let* ((pred (gethash (name entry) *entries-table*)))
    (cond
      (pred
       (setf (cadr pred) entry)
       (report-error nil
                     "Redefining test ~:@(~S~)"
                     (name entry)))
      (t
       (setf (gethash (name entry) *entries-table*) *entries-tail*)
       (setf (cdr *entries-tail*) (cons entry nil))
       (setf *entries-tail* (cdr *entries-tail*))
       )))
  (when *do-tests-when-defined*
    (do-entry entry))
  (setq *test* (name entry)))

(defun report-error (error? &rest args)
  (cond (*debug* 
         (apply #'format t args)
         (if error? (throw '*debug* nil)))
        (error? (apply #'error args))
        (t (apply #'warn args)))
  nil)

(defun do-test (&optional (name *test*))
  #-sbcl (do-entry (get-entry name))
  #+sbcl (handler-bind ((sb-ext:code-deletion-note #'muffle-warning))
		       (do-entry (get-entry name))))

(defun my-aref (a &rest args)
  (apply #'aref a args))

(defun my-row-major-aref (a index)
  (row-major-aref a index))

(defun structure-slots (object)
  "Returns a list of the slot-names of the provided structure object"
  #+openmcl
  (let* ((sd (gethash (class-name (class-of object)) ccl::%defstructs%))
	 (slots (if sd (ccl::sd-slots sd))))
    (mapcar #'car (if (symbolp (caar slots)) slots (cdr slots))))
  #+cmu
  (mapcar #'pcl:slot-definition-name (pcl:class-slots (class-of object)))
  #+lispworks
  (structure:structure-class-slot-names (class-of object))
  #+sbcl
  (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (class-of object)))
  #+allegro
  (mapcar #'mop:slot-definition-name (mop:class-slots (class-of object))))

(defun equalp-with-case (x y)
  "Like EQUALP, but doesn't do case conversion of characters.
   Currently doesn't work on arrays of dimension > 2."
  (cond
    ((eq x y) t)
    ((consp x)
     (and (consp y)
          (equalp-with-case (car x) (car y))
          (equalp-with-case (cdr x) (cdr y))))
    ((and (typep x 'array)
          (= (array-rank x) 0))
     (equalp-with-case (my-aref x) (my-aref y)))
    ((typep x 'vector)
     (and (typep y 'vector)
          (let ((x-len (length x))
                (y-len (length y)))
            (and (eql x-len y-len)
                 (loop
                     for i from 0 below x-len
                     for e1 = (my-aref x i)
                     for e2 = (my-aref y i)
                     always (equalp-with-case e1 e2))))))
    ((and (typep x 'array)
          (typep y 'array)
          (not (equal (array-dimensions x)
                      (array-dimensions y))))
     nil)

    ((typep x 'array)
     (and (typep y 'array)
          (let ((size (array-total-size x)))
            (loop for i from 0 below size
                  always (equalp-with-case (my-row-major-aref x i)
                                           (my-row-major-aref y i))))))

    ((typep x 'structure-object)
     (and (typep y 'structure-object)
          (eq (type-of x) (type-of y))
          (reduce (lambda (a b) (and a b))
                  (mapcar #'equalp-with-case
                          (structure-slots x)
                          (structure-slots y))
                  :initial-value t)
          (reduce (lambda (a b) (and a b))
                  (structure-slots x)
                  :key (lambda (slot)
                         (equalp-with-case (slot-value x slot) (slot-value y slot)))
                  :initial-value t)))
                          
    (t (eql x y))))

(defun do-entry (entry &optional
                       (s *standard-output*))
  (catch '*in-test*
    (setq *test* (name entry))
    (setf (pend entry) t)
    (let* ((*in-test* t)
           ;; (*break-on-warnings* t)
           (aborted nil)
           
           r)
      ;; (declare (special *break-on-warnings*))

      (block aborted
        (setf r
              (flet ((%do ()
                       (cond
                         (*compile-tests*
                          (multiple-value-list
                              (funcall (compile
                                        nil
                                        `(lambda ()
                                          (declare
                                           (optimize ,@*optimization-settings*))
                                          ,(form entry))))))
                         (*expanded-eval*
                          (multiple-value-list
                              (expanded-eval (form entry))))
                         (t
                          (multiple-value-list
                              (eval (form entry)))))))
                (if *catch-errors*
                  (handler-bind
                      (#-ecl (style-warning #'muffle-warning)
                             (error #'(lambda (c)
                                        (setf aborted t)
                                        (setf r (list c))
                                        (return-from aborted nil))))
                    (%do))
                  (%do)))))

      (setf (pend entry)
            (or aborted
                (not (funcall (equalp-fn entry) r (vals entry)))))
      
      (when (pend entry)
        (let ((*print-circle* *print-circle-on-failure*))
          (format s "~&Test ~:@(~S~) failed~
                   ~%Form: ~S~
                   ~%Expected value~P: ~
                      ~{~S~^~%~17t~}~%"
                  *test* (form entry)
                  (length (vals entry))
                  (vals entry))
          (handler-case
              (let ((st (format nil "Actual value~P: ~
                      ~{~S~^~%~15t~}.~%"
                                (length r) r)))
                (format s "~A" st))
            (error () (format s "Actual value: #<error during printing>~%")
                   ))
          (finish-output s)
          ))))
  (when (not (pend entry)) *test*))

(defun expanded-eval (form)
  "Split off top level of a form and eval separately.  This reduces the chance that
   compiler optimizations will fold away runtime computation."
  (if (not (consp form))
    (eval form)
    (let ((op (car form)))
      (cond
        ((eq op 'let)
         (let* ((bindings (loop for b in (cadr form)
                                collect (if (consp b) b (list b nil))))
                (vars (mapcar #'car bindings))
                (binding-forms (mapcar #'cadr bindings)))
           (apply
            (the function
              (eval `(lambda ,vars ,@(cddr form))))
            (mapcar #'eval binding-forms))))
        ((and (eq op 'let*) (cadr form))
         (let* ((bindings (loop for b in (cadr form)
                                collect (if (consp b) b (list b nil))))
                (vars (mapcar #'car bindings))
                (binding-forms (mapcar #'cadr bindings)))
           (funcall
            (the function
              (eval `(lambda (,(car vars) &aux ,@(cdr bindings)) ,@(cddr form))))
            (eval (car binding-forms)))))
        ((eq op 'progn)
         (loop for e on (cdr form)
               do (if (null (cdr e)) (return (eval (car e)))
                    (eval (car e)))))
        ((and (symbolp op) (fboundp op)
              (not (macro-function op))
              (not (special-operator-p op)))
         (apply (symbol-function op)
                (mapcar #'eval (cdr form))))
        (t (eval form))))))

(defun continue-testing ()
  (if *in-test*
    (throw '*in-test* nil)
    (do-entries *standard-output*)))

(defun do-tests (&optional
                 (out *standard-output*))
  (dolist (entry (cdr *entries*))
    (setf (pend entry) t))
  (if (streamp out)
    (do-entries out)
    (with-open-file 
        (stream out :direction :output)
      (do-entries stream))))

(defun do-entries* (s)
  (format s "~&Doing ~A pending test~:P ~
             of ~A tests total.~%"
          (count t (the list (cdr *entries*)) :key #'pend)
          (length (cdr *entries*)))
  (finish-output s)
  (dolist (entry (cdr *entries*))
    (when (and (pend entry)
               (not (has-disabled-note entry)))
      (format s "~@[~<~%~:; ~:@(~S~)~>~]"
              (do-entry entry s))
      (finish-output s)
      ))
  (let ((pending (pending-tests))
        (expected-table (make-hash-table :test #'equal)))
    (dolist (ex *expected-failures*)
      (setf (gethash ex expected-table) t))
    (let ((new-failures
           (loop for pend in pending
                 unless (gethash pend expected-table)
                 collect pend)))
      (if (null pending)
        (format s "~&No tests failed.")
        (progn
          (format s "~&~A out of ~A ~
                   total tests failed: ~
                   ~:@(~{~<~%   ~1:;~S~>~
                         ~^, ~}~)."
                  (length pending)
                  (length (cdr *entries*))
                  pending)
          (if (null new-failures)
            (format s "~&No unexpected failures.")
            (when *expected-failures*
              (format s "~&~A unexpected failures: ~
                   ~:@(~{~<~%   ~1:;~S~>~
                         ~^, ~}~)."
                      (length new-failures)
                      new-failures)))
          ))
      (finish-output s)
      (values (null pending)
              (null new-failures)))))

(defun do-entries (s)
  #-sbcl (do-entries* s)
  #+sbcl (handler-bind ((sb-ext:code-deletion-note #'muffle-warning))
		       (do-entries* s)))

;;; Note handling functions and macros

(defmacro defnote (name contents &optional disabled)
  `(eval-when (:load-toplevel :execute)
    (let ((note (make-note :name ',name
                           :contents ',contents
                           :disabled ',disabled)))
      (setf (gethash (note-name note) *notes*) note)
      note)))

(defun disable-note (n)
  (let ((note (if (note-p n) n
                (setf n (gethash n *notes*)))))
    (unless note (error "~A is not a note or note name." n))
    (setf (note-disabled note) t)
    note))

(defun enable-note (n)
  (let ((note (if (note-p n) n
                (setf n (gethash n *notes*)))))
    (unless note (error "~A is not a note or note name." n))
    (setf (note-disabled note) nil)
    note))

(defun enable-all-notes ()
  (loop for note being each hash-value of *notes*
        do (setf (note-disabled note) nil)))

(defun disable-all-notes ()
  (loop for note being each hash-value of *notes*
        do (setf (note-disabled note) t)))