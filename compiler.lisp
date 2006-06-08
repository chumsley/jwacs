;;;; compiler.lisp
;;;
;;; Defines the interface functions for the jwacs compiler.
(in-package :jwacs)

;;;; ======= Dependency checks =====================================================================

(defun determine-imported-modules (base-pathname prefix-lookup &optional already-imported)
  "Determines the modules imported by the file specified by BASE-PATHNAME.  Modules that
   are elements of ALREADY-IMPORTED will not be included."
  (labels ((import-decl-to-module (elm)
             (with-slots (uri) elm
               (if (null (import-decl-type-symbol elm))
                 (make-module :type (lookup-module-type (pathname-type (pathname uri)))
                              :path (resolve-import-uri base-pathname uri prefix-lookup)
                              :uri uri)
                 (make-module :type (lookup-module-type (import-decl-type-symbol elm))
                              :path (resolve-import-uri base-pathname uri prefix-lookup)
                              :uri uri))))
           (elms-to-modules (elm-list)
             (loop for elm in elm-list
                   when (import-decl-p elm)
                   collect (import-decl-to-module elm))))
    (let* ((queue (elms-to-modules (parse-file base-pathname)))
           (own-already-imported (union (mapcar 'module-path queue)
                                        already-imported
                                        :test 'pathnames-equal)))
      (loop for module in queue
            unless (find (module-path module) already-imported :test 'pathnames-equal)
            append (if (eq (module-type module) 'jw)
                     (postpend (determine-imported-modules (module-path module)
                                                           prefix-lookup
                                                           own-already-imported)
                               module)
                     (list module))))))

(defun determine-modules (base-pathname prefix-lookup)
  "Using the file pointed to by BASE-PATHNAME as the main module, determine a list of modules
   that need to be processed to generate an app."
  (let* ((uri (if (null (pathname-type base-pathname))
                (pathname-name base-pathname)
                (format nil "~A.~A" (pathname-name base-pathname) (pathname-type base-pathname)))))
    (postpend (determine-imported-modules base-pathname
                                          prefix-lookup
                                          (list base-pathname))
              (make-module :type 'jw
                           :path base-pathname
                           :uri uri))))

;;;; ======= URI handling ==========================================================================
;;TODO Checking for valid URIs in import statements
;;TODO Some less horrible URI handling generally

(defun absolute-uri-p (uri)
  "Predicate for checking if a URI is specified from the root rather than
   relative to the current tree position."
  (char= #\/ (aref uri 0)))

(defun resolve-absolute-uri (uri prefix-lookup)
  "Finds the base-pathname in PREFIX-LOOKUP that most closely matches URI.
  URI should be a string representing a URI (minus the host and protocol).
  PREFIX-LOOKUP should be an assoc list of cells whose CAR is a prefix
  that begins and ends with a slash, and whose CDR is a pathname representing
  a directory in the filesystem."
  (flet ((match-degree (prefix)
           (let ((prefix-len (length prefix))
                 (uri-len (length uri)))
             (if (and (<= prefix-len uri-len)
                      (equal (subseq uri 0 prefix-len) prefix))
               prefix-len
               0))))
    (let* ((prefix (reduce (lambda (left right) (if (> (match-degree left)
                                                       (match-degree right))
                                                  left
                                                  right))
                           (mapcar #'car prefix-lookup)))
           (prefix-base (cdr (assoc prefix prefix-lookup)))
           (suffix (subseq uri (length prefix))))
      (assert (absolute-uri-p uri))
      (if (or (null prefix)
              (zerop (match-degree prefix)))
        (error "~A has no prefixes in ~S" uri prefix-lookup)
        (merge-pathnames (pathname suffix) prefix-base)))))
      
(defun resolve-import-uri (base-pathname uri prefix-lookup)
  "Resolves the URI of an import that appears in the file located at BASE-PATHNAME.
   Absolute URIs are resolved using PREFIX-LOOKUP to determine their base-pathname."
  (if (absolute-uri-p uri)
    (resolve-absolute-uri uri prefix-lookup)
    (merge-pathnames (pathname uri) base-pathname)))

(defun transform-uri (uri new-extension)
  "Converts URI to a URI that points to a different type of file.
   Eg, (TRANSFORM-URI \"/common/lib.jw\" \"js\") ==> \"/common/lib.js\" "
  (let ((search-uri (copy-seq uri)))     ; REGEX-REPLACE may have side-effects
    (regex-replace "\\.[^\\.]*$" search-uri (format nil ".~A" new-extension))))

;;;; ======= Module datatype =======================================================================
(defstruct module
  "Represents a single module of a jwacs application"
  type
  path
  uri)

(defun lookup-module-type (raw-type)
  "Converts a 'raw' module type into a canonical type symbol.
   A warning will be signalled for unrecognized raw types.
   RAW-TYPE should be either a string or a symbol.

   The recognized types are:

      Symbol | Description            | Extensions
      -------|------------------------|------------------
      'JW    | jwacs source           | .jw, .jwa, .jwacs
      'JS    | Javascript source      | .js"
  
  (let ((type-symbol (if (stringp raw-type)
                            (intern (string-upcase raw-type) :jwacs)
                            raw-type)))
    (case type-symbol
      ((jw jwa jwacs)
       'jw)
      (js
       'js)
      (otherwise
       (warn "Unrecognized import type ~A" type-symbol)
       type-symbol))))

;;;; ======= Source transformation =================================================================

(defparameter *compiler-pipeline*
  '(strip-imports                       ; Remove import decls (which we're done with and which aren't valid Javascript)
    shift-function-decls                ; Move function-decls to front of each scope
    uniquify                            ; TODO we should talk about whether this step is necessary/desirable
    loop-canonicalize                   ; Convert loops to a canonical form for easier CPS conversion
    explicitize                         ; Give all intermediate values a name
    shadow-values                       ; "Shadow" references to `this` and `arguments`
    cps                                 ; Convert functions and loops to continuation-passing style functions
    trampoline                          ; Convert functions to trampoline-style functions
    runtime)                            ; Add calls into the runtime to support dynamic behaviour
  "The list of transformations (in order) that are performed to convert jwacs source into
   Javascript source.")

(defun transform-modules (module-list &optional (pipeline *compiler-pipeline*) (var-counter 0))
  "Transforms each module in MODULE-LIST and returns a list of modules suitable for wrapping."
  (flet ((transform-jwacs-module (module)
           (let* ((*genvar-counter* var-counter)
                  (in-path (module-path module))
                  (xformed-elms (pipeline-compile (parse-file in-path) pipeline))
                  (out-path (merge-pathnames (make-pathname :type "js") in-path)))

             ;; Make sure that we're not trying to overwrite any input files
             (when (find out-path module-list :test 'pathnames-equal :key 'module-path)
               (error "Attempt to overwrite ~A" out-path))

             ;; Emit the transformed code
             (with-open-file (out-stream out-path :direction :output :if-exists :supersede)
               (emit-elms xformed-elms out-stream :pretty-output t))

             ;; Return the output module
             (make-module :uri (transform-uri (module-uri module) "js")
                                :path out-path
                                :type 'js))))
    
    (loop for module in module-list
        if (eq (module-type module) 'jw)
        collect (transform-jwacs-module module)
        else
        ;; Non-jwacs modules are passed through unchanged
        collect module)))

(defun pipeline-compile (elm &optional (pipeline *compiler-pipeline*))
  "Applies the transformations specified in PIPELINE to ELM in order.
   ELM may be either a source-element or a list of source-elements."
  (if (null pipeline)
    elm
    (pipeline-compile (transform (car pipeline) elm)
                      (cdr pipeline))))

(defun parse-file (path)
  "Load the file at PATH and parse it into a js/jw source model"
  (with-open-file (in path :direction :input :if-does-not-exist :error)
    (let ((text (with-output-to-string (str)
                  (loop for line = (read-line in nil nil nil)
                      until (null line)
                      do (format str "~A~%" line)))))
      (parse text))))

(defun emit-elms (elms out-stream &key pretty-output)
  "Prints ELMS to OUT-STREAM.  If PRETTY-OUTPUT is NIL, the elements will
   be printed compactly"
  (let ((*pretty-mode* pretty-output)
         (*opt-space* (if pretty-output " " "")))
    (pretty-print elms out-stream)))

;;;; ======= strip-imports transformation ==========================================================

(defmethod transform ((xform (eql 'strip-imports)) (elm-list list))
  (remove-if #'null (call-next-method)))

(defmethod transform ((xform (eql 'strip-imports)) (elm import-decl))
  nil)
             
;;;; ======= Module wrapping =======================================================================

(defun wrap-modules (module-list template-path out-path)
  "Creates a 'wrapper' html file that represents a jwacs application containing
   all of the files of MODULE-LIST.  We read in a template file from
   TEMPLATE-PATH, modify its HEAD tag to contain appropriate pointers to each
   of the modules, and write it to OUT-PATH."

  ;; Make sure we're not overwriting any input modules
  (when (find out-path module-list :key 'module-path :test 'pathnames-equal)
    (error "Attempt to overwrite ~A" out-path))
    
  (let ((template-string (read-entire-file template-path)))
    (multiple-value-bind (s e)
        (cl-ppcre:scan "<%\\s*jwacs_imports\\s*%>" template-string)
      (with-open-file (out out-path :direction :output :if-exists :supersede)
        (format out "~A" (subseq template-string 0 s))
        (dolist (module module-list)
          (wrap-module module (module-type module) out))
        (format out "~A" (subseq template-string e)))))

  ;; Return the path of the output file
  out-path)

(defgeneric wrap-module (module module-type head-stream)
  (:documentation
   "Outputs HTML to HEAD-STREAM to cause the wrapper html file to link to MODULE."))

(defmethod wrap-module (module (module-type (eql 'jw)) head-stream)
  (error "Internal error: jwacs modules must be transformed before they are wrapped"))

(defmethod wrap-module (module (module-type (eql 'js)) head-stream)
  (format head-stream "~&<script type='text/javascript' src='~A'></script>" (module-uri module)))

;;;; ======= Cached defaults =======================================================================
;; These strings will be used to generate default versions of missing files.

(defparameter *runtime-text* (with-output-to-string (str)
                               (pretty-print (parse (read-asdf-component-text '(:jwacs "jw-runtime"))) str))
  "The text of the jwacs runtime, stripped of comments etc.")

(defparameter *default-template* (read-asdf-component-text '(:jwacs "default-template"))
  "The text of the default application template")

(defparameter *default-bootframe* (read-asdf-component-text '(:jwacs "default-bootframe"))
  "The text of the 'boot frame' file for the default application template")

;;;; ======= Exported API ==========================================================================

(defun build-app (main-module-path &key template-path output-path prefix-lookup runtime-uri)
  "Build a wrapper html file for a jwacs application"
  (flet ((get-path (param-path path-name path-type)
           "If PARAM-PATH is non-NIL, return it.
            Otherwise, make a new path based on MAIN-MODULE-PATH."
           (if (null param-path)
             (merge-pathnames (make-pathname :name path-name :type path-type)
                              main-module-path)
             param-path)))
    (let ((template-path (get-path template-path nil "template"))
          (output-path (get-path output-path nil "html"))
          (bootstrap-path (get-path nil "bootstrap" "html"))
          (runtime-module (if (null runtime-uri)
                            (make-module :type 'js
                                         :path (get-path nil "jw-rt" "js")
                                         :uri "jw-rt.js")
                            (make-module :type 'js
                                         :path (resolve-import-uri main-module-path runtime-uri prefix-lookup)
                                         :uri runtime-uri))))

      ;; If no template file exists, generate one
      (when (null (probe-file template-path))
        (unless (probe-file bootstrap-path)
          (with-open-file (out template-path :direction :output)
            (format out "~A" *default-template*))
          (with-open-file (out bootstrap-path :direction :output)
            (format out "~A" *default-bootframe*))))

      ;; If no runtime file exists, generate one
;TEST      (when (null (probe-file (module-path runtime-module)))
      (when t
        (with-open-file (out (module-path runtime-module) :direction :output :if-exists :supersede)
          (format out "~A" *runtime-text*)))

      ;; Wrap the modules.  Note that we force the runtime onto the front of the list
      ;; of imports.
      (wrap-modules
       (cons runtime-module (transform-modules
                             (determine-modules main-module-path prefix-lookup)))
       template-path
       output-path))))

(defun process (in-path)
  (transform-modules (list (make-module :type 'jw :path in-path))))