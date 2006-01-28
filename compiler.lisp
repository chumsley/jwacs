;;;; compiler.lisp
;;;
;;; Defines the interface functions for the jwacs compiler.
;;; The outside world calls JWACS:PROCESS to process a set
;;; of .JW source files into a jwacs program.
(in-package :jwacs)

(defparameter *compiler-pipeline*
  '(shift-function-decls                ; Move function-decls to front of each scope
    uniquify                            ; TODO we should talk about whether this step is necessary/desirable
    loop-canonicalize                   ; Convert loops to a canonical form for easier CPS conversion
    explicitize                         ; Give all intermediate values a name
    cps                                 ; Convert functions and loops to continuation-passing style functions
    trampoline                          ; Convert functions to trampoline-style functions
    runtime)                            ; Add calls into the runtime to support dynamic behaviour
  "The list of transformations (in order) that are performed to convert jwacs source into
   Javascript source.")

(defun pipeline-compile (elm &optional (pipeline *compiler-pipeline*))
  "Applies the transformations specified in PIPELINE to ELM in order.
   ELM may be either a source-element or a list of source-elements."
  (let ((*genvar-counter* 0))
    (if (null pipeline)
      elm
      (pipeline-compile (transform (car pipeline) elm)
                        (cdr pipeline)))))

(defun parse-file (path)
  "Load the file at PATH and parse it into a js/jw source model"
  (with-open-file (in path :direction :input :if-does-not-exist :error)
    (let ((text (loop for line = (read-line in nil nil nil)
                      until (null line)
                      collect line into lines
                      collect (format nil "~%") into lines
                      finally (return (apply 'concatenate 'string lines)))))
      (parse text))))

(defun process (pathspecs &key out-path (include-runtime t) (pretty-output t))
  "Parses the file or files specified by PATHSPECS and compiles them into a single
   output .js file, which will be written to OUT-PATH.

   PATHSPECS can be a string, a pathname, a list of strings, or a list of pathnames.
   The pathnames may include wildcards.

   If OUT-PATH is not specified, then the output will be written to a
   file whose name is taken from the last input file.  eg:

     (PROCESS #P'c:/temp/t.jw') ==> Writes to c:/temp/t.js
     (PROCESS #P'c:/temp/t?.jw') ==> Writes to c:/temp/t9.js (if t9.jw is the last file that matches)

   If INCLUDE-RUNTIME is non-NIL, then the runtime will be included as part
   of the output file.  If it is NIL, then the runtime will not be included;
   that's useful for web frameworks that send the runtime separately so as to
   increase the chance the the user has cached it already.

   If PRETTY-OUTPUT is non-NIL, the output will be pretty-printed; if it is
   non-NIL, then output will be obfuscated."
  (let* ((input-paths (find-pathspec-matches pathspecs))
         (input-elms (mapcan 'parse-file input-paths))
         (target (compute-output-path out-path (car (last input-paths)))))

    ;; Save the user from themselves if they attempt to overwrite an input file
    (when (member target input-paths :test 'pathname-match-p)
      (error "Output target ~S matches an input path ~S"
             target
             (car (member target input-paths :test 'pathname-match-p))))
             
    ;; Emit the compiled elements
    (with-open-file (out target :direction :output :if-exists :supersede :if-does-not-exist :create)
      (when include-runtime
        (emit-runtime out :pretty-output pretty-output))
      (emit-elms (pipeline-compile input-elms) out :pretty-output pretty-output))

    ;; Return the path to the output filename
    target))

(defun find-pathspec-matches (pathspecs)
  "Returns a list of pathnames that match PATHSPECS.  PATHSPECS may be either
   a string, a pathname, or a list of pathnames.  It may include wildcards."
  (if (listp pathspecs)
    (mapcan 'directory pathspecs)
    (directory pathspecs)))

(defun compute-output-path (out-path default-pathname)
  "If OUT-PATH is non-NIL, returns it.  Otherwise, builds a new pathname
   whose extension is '.js' and whose name is the same as that of
   DEFAULT-PATHNAME."
  (if out-path
    out-path
    (merge-pathnames (make-pathname :type "js") default-pathname)))

(defun emit-runtime (out-stream &key pretty-output)
  "Prints the jwacs runtime to OUT-STREAM.  If PRETTY-OUTPUT is NIL,
   the runtime will be printed compactly."
  (let* ((fname (asdf:component-pathname
                 (asdf:find-component (asdf:find-system :jwacs) "jw-runtime")))
         (elms (parse-file fname)))
    (emit-elms elms out-stream :pretty-output pretty-output)))         

(defun emit-elms (elms out-stream &key pretty-output)
  "Prints ELMS to OUT-STREAM.  If PRETTY-OUTPUT is NIL, the elements will
   be printed compactly"
  (let ((*pretty-mode* pretty-output)
         (*opt-space* (if pretty-output " " "")))
    (pretty-print elms out-stream)))