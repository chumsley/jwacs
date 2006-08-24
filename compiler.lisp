;;;; compiler.lisp
;;;
;;; Defines the interface functions for the jwacs compiler.
;;;
;;; Copyright (c) 2006 James Wright
;;; See LICENSE for full licensing details.
;;;
(in-package :jwacs)

;;;; ======= Dependency checks =====================================================================

(defun determine-imported-modules (main-pathname prefix-lookup &optional already-imported)
  "Determines the modules imported by the file specified by MAIN-PATHNAME.  Modules that
   are elements of ALREADY-IMPORTED will not be included."
  (labels ((import-decl-to-module (elm)
             (with-slots (uripath) elm
               (if (null (import-decl-type-symbol elm))
                 (make-module :type (lookup-module-type (pathname-type (pathname uripath)))
                              :path (resolve-import-uripath main-pathname uripath prefix-lookup)
                              :uripath uripath)
                 (make-module :type (lookup-module-type (import-decl-type-symbol elm))
                              :path (resolve-import-uripath main-pathname uripath prefix-lookup)
                              :uripath uripath))))
           (elms-to-modules (elm-list)
             (loop for elm in elm-list
                   when (import-decl-p elm)
                   collect (import-decl-to-module elm))))
    (let* ((queue (elms-to-modules (parse-file main-pathname)))
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

(defun determine-modules (main-pathname prefix-lookup)
  "Using the file pointed to by MAIN-PATHNAME as the main module, determine a list of modules
   that need to be processed to generate an app."
  (let* ((uripath (if (null (pathname-type main-pathname))
                    (pathname-name main-pathname)
                    (format nil "~A.~A" (pathname-name main-pathname) (pathname-type main-pathname)))))
    (postpend (determine-imported-modules main-pathname
                                          prefix-lookup
                                          (list main-pathname))
              (make-module :type 'jw
                           :path main-pathname
                           :uripath uripath))))

;;;; ======= URIPATH handling ==========================================================================

(defun absolute-uripath-p (uripath)
  "Predicate for checking if a URIPATH is specified from the root rather than
   relative to the current tree position."
  (char= #\/ (aref uripath 0)))

(defun resolve-absolute-uripath (uripath prefix-lookup)
  "Finds the base-pathname in PREFIX-LOOKUP that most closely matches URIPATH.
  URIPATH should be a string representing the path component of a URI.
  PREFIX-LOOKUP should be an assoc list of cells whose CAR is a prefix
  that begins and ends with a slash, and whose CDR is a pathname representing
  a directory in the filesystem."
  (flet ((match-degree (prefix)
           (let ((prefix-len (length prefix))
                 (uripath-len (length uripath)))
             (if (and (<= prefix-len uripath-len)
                      (equal (subseq uripath 0 prefix-len) prefix))
               prefix-len
               0))))
    (let* ((prefix (reduce (lambda (left right) (if (> (match-degree left)
                                                       (match-degree right))
                                                  left
                                                  right))
                           (mapcar #'car prefix-lookup)
                           :initial-value ""))
           (prefix-base (cdr (assoc prefix prefix-lookup)))
           (suffix (subseq uripath (length prefix))))
      (assert (absolute-uripath-p uripath))
      (if (or (null prefix)
              (zerop (match-degree prefix)))
        (error "~S has no prefixes in ~S" uripath prefix-lookup)
        (merge-pathnames (pathname suffix) prefix-base)))))
      
(defun resolve-import-uripath (base-pathname uripath prefix-lookup)
  "Resolves the URIPATH of an import that appears in the file located at BASE-PATHNAME.
   Absolute URIPATHs are resolved using PREFIX-LOOKUP to determine their base-pathname."
  (if (absolute-uripath-p uripath)
    (resolve-absolute-uripath uripath prefix-lookup)
    (merge-pathnames (pathname uripath) base-pathname)))

(defun change-uripath-extension (uripath new-extension)
  "Converts URIPATH to a URIPATH that points to a different type of file.
   Eg, (CHANGE-URIPATH-EXTENSION \"/common/lib.jw\" \"js\") ==> \"/common/lib.js\" "
  (if (find #\. uripath)
    (let ((search-uripath (copy-seq uripath))) ; REGEX-REPLACE may have side-effects
      (regex-replace "\\.[^\\.]*$" search-uripath (format nil ".~A" new-extension)))
    (format nil "~A.~A" uripath new-extension)))

;;;; ======= Module datatype =======================================================================
(defstruct module
  "Represents a single module of a jwacs application"
  type
  path
  uripath)

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
    shift-decls                         ; Move function-decls to front of each scope, global var-decls to front of toplevel
    uniquify                            ; TODO we should talk about whether this step is necessary/desirable
    loop-canonicalize                   ; Convert loops to a canonical form for easier CPS conversion
    explicitize                         ; Give all intermediate values a name
    shadow-values                       ; "Shadow" references to `this` and `arguments`
    cps                                 ; Convert functions and loops to continuation-passing style functions
    trampoline                          ; Convert functions to trampoline-style functions
    runtime)                            ; Add calls into the runtime to support dynamic behaviour
  "The list of transformations (in order) that are performed to convert jwacs source into
   Javascript source.")

(defun transform-modules (module-list &key (compress-mode (not *debug-mode*)) (pipeline *compiler-pipeline*) (var-counter 0))
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
               (emit-elms xformed-elms out-stream :pretty-output (not compress-mode)))

             ;; Return the output module
             (make-module :uripath (change-uripath-extension (module-uripath module) "js")
                          :path out-path
                          :type 'js)))

         (confirm-file (module)
           "Confirm that the file specified by MODULE's uripath actually exists"
           (unless (probe-file (module-path module))
             (if (eq (module-type module) 'jw)
               (error "Cannot read '~A' (specified by URI path '~A')" (module-path module) (module-uripath module))
               (warn "Cannot read '~A' (specified by URI path '~A')" (module-path module) (module-uripath module))))))
    
    (loop for module in module-list
          do (confirm-file module)
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
      (parse-only text))))

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

(defun wrap-modules (module-list template-path out-path combined-js-module)
  "Creates a 'wrapper' html file that represents a jwacs application containing
   all of the files of MODULE-LIST.  We read in a template file from
   TEMPLATE-PATH, modify it to contain appropriate <SCRIPT> tags referencing each
   of the modules, and write it to OUT-PATH.  If COMBINED-JS-MODULE is NIL, each
   js-module gets its own SCRIPT tag; otherwise we will wrap all JS modules into a
   single file."

  ;; Make sure we're not overwriting any input modules
  (when (find out-path module-list :key 'module-path :test 'pathnames-equal)
    (error "Attempt to overwrite ~A" out-path))
    
  ;; If there's a combined-js-module, make sure that we replace it
  (when (and combined-js-module
             (probe-file (module-path combined-js-module)))
    (delete-file (module-path combined-js-module)))
  
  ;; Okay, let's build that html file
  (let ((template-string (read-entire-file template-path)))
    (multiple-value-bind (s e)
        (cl-ppcre:scan "<@\\s*jwacs_imports\\s*@>" template-string)
      (with-open-file (out out-path :direction :output :if-exists :supersede)
        (format out "~A" (subseq template-string 0 s))
        (dolist (module module-list)
          (if (and combined-js-module
                   (eq (module-type module) 'js))
            (append-module module combined-js-module)
            (wrap-module module (module-type module) out)))
        (if combined-js-module
          (wrap-module combined-js-module (module-type combined-js-module) out))
        (format out "~A" (subseq template-string e)))))

  ;; Return the path of the output file
  out-path)

(defgeneric wrap-module (module module-type head-stream)
  (:documentation
   "Outputs HTML to HEAD-STREAM to cause the wrapper html file to link to MODULE."))

(defmethod wrap-module (module (module-type (eql 'jw)) head-stream)
  (error "Internal error: jwacs modules must be transformed before they are wrapped"))

(defmethod wrap-module (module (module-type (eql 'js)) head-stream)
  (format head-stream "~&<script type='text/javascript' src='~A'></script>" (module-uripath module)))

(defun append-module (src-module target-module)
  "Appends the contents of SRC-MODULE to the end of TARGET-MODULE, creating TARGET-MODULE if
   necessary"
  (with-open-file (in (module-path src-module)
                      :direction :input
                      :element-type 'unsigned-byte)
    (with-open-file (out (module-path target-module)
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create
                         :element-type 'unsigned-byte)
      (let ((buffer (make-array 8192 :element-type 'unsigned-byte)))
        (loop for pos = (read-sequence buffer in)
              until (zerop pos)
              do (write-sequence buffer out :end pos))))))

;;;; ======= Cached defaults =======================================================================
;; These strings will be used to generate default versions of missing files.

(defparameter *runtime-text* (with-output-to-string (str)
                               (pretty-print (parse (read-asdf-component-text '(:jwacs "jw-runtime"))) str))
  "The text of the jwacs runtime, stripped of comments etc.")

(defparameter *default-template* (read-asdf-component-text '(:jwacs "default-template"))
  "The text of the default application template")

(defparameter *default-iframe* (read-asdf-component-text '(:jwacs "default-iframe"))
  "The text of the hidden iframe for bookmark handling")

;;;; ======= Exported API ==========================================================================

(defun build-app (main-module-path
                   &key template-uripath output-uripath prefix-lookup runtime-uripath
                        debug-mode (compress-mode (not debug-mode)) (combine-mode (not debug-mode)))
  "Build a wrapper html file for a jwacs application"
  (flet ((get-path (param-uripath path-name path-type)
           "If PARAM-PATH is non-NIL, return it.
            Otherwise, make a new path based on MAIN-MODULE-PATH."
           (if (null param-uripath)
             (merge-pathnames (make-pathname :name path-name :type path-type)
                              main-module-path)
             (resolve-import-uripath main-module-path param-uripath prefix-lookup))))
    (let ((*debug-mode* debug-mode)
          (template-path (get-path template-uripath nil "template"))
          (output-path (get-path output-uripath nil "html"))
          (iframe-path (get-path nil "blank" "html"))
          (runtime-module (if (null runtime-uripath)
                            (make-module :type 'js
                                         :path (get-path nil "jw-rt" "js")
                                         :uripath "jw-rt.js")
                            (make-module :type 'js
                                         :path (resolve-import-uripath main-module-path runtime-uripath prefix-lookup)
                                         :uripath runtime-uripath))))
          
      ;; If no template file exists, generate one
      (unless (probe-file template-path)
        (with-open-file (out template-path :direction :output)
            (format out "~A" *default-template*)))

      (unless (probe-file iframe-path)
        (with-open-file (out iframe-path :direction :output)
          (format out "~A" *default-iframe*)))
      
      ;; If no runtime file exists, generate one
     (when (null (probe-file (module-path runtime-module)))
;;TEST      (when t
        (with-open-file (out (module-path runtime-module) :direction :output :if-exists :supersede)
          (if compress-mode
            (emit-elms (parse *runtime-text*) out :pretty-output (not compress-mode))
            (format out "~A" *runtime-text*))))

      ;; Wrap the modules.  Note that we force the runtime onto the front of the list
      ;; of imports.
      (let* ((module-list (cons runtime-module
                                (remove (module-path runtime-module)
                                        (determine-modules main-module-path prefix-lookup)
                                        :key 'module-path
                                        :test 'pathnames-equal)))
             (combined-js-module (when combine-mode ; TODO deal with non-jwacs main modules
                                   (let ((combined-path (get-path nil (format nil "~A_all" (pathname-name main-module-path)) "js")))

                                     ;; Ensure that we don't overwrite any input files
                                     (loop while (find combined-path module-list :test 'pathnames-equal :key 'module-path)
                                       do (setf combined-path
                                                (get-path nil (format nil "~A_all" (pathname-name combined-path)) "js")))

                                     ;; Generate the combined module name with the unique name from above
                                     (make-module :type 'js
                                                  :path combined-path
                                                  :uripath (file-namestring combined-path))))))


        (wrap-modules
         (transform-modules module-list :compress-mode compress-mode)
         template-path
         output-path
         combined-js-module)))))

(defun process (in-path)
  (transform-modules (list (make-module :type 'jw :path in-path))))