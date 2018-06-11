(in-package :jwacs-tests)

(defun node (&rest args)
  (trivial-shell:shell-command
   (format nil "node ~A"
           (asdf:component-pathname
            (asdf:find-component (asdf:find-component (asdf:find-system :jwacs-tests)
                                                      "tests")
                                 "rt-test-harness")))
   :input "Here is some input!"))

(defun emit-js (ast)
  (let ((module (make-module :type 'js :compressed-p nil
                             :inline-stream (make-string-output-stream))))
    (with-module-output (out module)
      (emit-elms ast out :pretty-output t)
      (get-module-text module))))

(defun parse-expr (s)
  (car (parse s #+-(concatenate 'string "(" s ")"))))

(defun write-test (target-dir name effect-factory transform-effect-factory source data)
  (let ((out-path (make-pathname
                   :directory target-dir
                   :name name
                   :type "js")))
    (ensure-directories-exist out-path)
    (let ((module (make-module :path out-path :type 'js :compressed-p nil))
          (tran-source (pipeline-compile source *compiler-pipeline*))
          (tran-effect-factory (pipeline-compile transform-effect-factory *compiler-pipeline*)))
      (with-module-output (out module)
        (format out "~A" *runtime-text*)
        (emit-elms
         (make-fn-call :fn (parse-expr #+-"require(\"./rt-test-harness\").run_tests"
                                       (concatenate
                                        'string "require(\""
                                        (namestring
                                         (asdf:component-pathname
                                          (asdf:find-component (asdf:find-component (asdf:find-system :jwacs-tests)
                                                                                    "tests")
                                                               "rt-test-harness")))
                                        "\").runTests"))
                       :args (list (make-object-literal
                                    :properties
                                    `((#s(string-literal :value "fn_original") .
                                         ,source)
                                      (#s(string-literal :value "fn_transformed") .
                                         ,tran-source)
                                      (#s(string-literal :value "effect_factory_original") .
                                         ,effect-factory)
                                      (#s(string-literal :value "effect_factory_transformed") .
                                         ,tran-effect-factory)
                                      (#s(string-literal :value "data") .
                                         ,data)))))
         out :pretty-output t)))))

'(comment

  *runtime-text*

  (defvar number-gen-async (parse-expr "(function async_number_gen(n) {
    var label = \"number-\" + n;
    var rand = require('random-seed').create(label);
    var ret = function (trace_array) {
        return function () {
            var args = Array.prototype.slice.call(arguments);
            var ret = rand.floatBetween(-5,5);
            Array.prototype.unshift.call(args, ret);
            Array.prototype.unshift.call(args, label);
            Array.prototype.push.call(trace_array, args);
            var k = function_continuation;
            setTimeout(function(){
              resume k <- ret
            }, 1);
            suspend;
        }
    };
    ret.label = label;
    ret.effect_id = \"number-gen\";
    return ret;
})"))

  (defvar number-gen
    (parse-expr "(function number_gen(n) {
    var label = \"number-\" + n;
    var rand = require('random-seed').create(label);
    var ret = function (trace_array) {
        return function () {
            var args = Array.prototype.slice.call(arguments);
            var ret = rand.floatBetween(-5,5);
            Array.prototype.unshift.call(args, ret);
            Array.prototype.unshift.call(args, label);
            Array.prototype.push.call(trace_array, args);
            return ret;
        }
    };
    ret.label = label;
    ret.effect_id = \"number-gen\";
    return ret;
})"))
  
  (write-test "/tmp/foo" "simple"
   number-gen number-gen
   (parse-expr "(function (on_finish, f,g,h){
  return function(a,b,c) {
    var i = 10;
    while(i>0) {
    i--;
    if(a < 0.5) {
      c = c + h(a,b,c)*0.1 + f(a,b,c)*0.1;
    } else {
      c = c + g(a,b,c)*0.1 + h(a,b,c)*0.1;
    }
    if(b < 0.5) {
      a = a + f(a,b,c)*0.1 + h(a,b,c)*0.1;
    } else {
      b = b + h(a,b,c)*0.1 + g(a,b,c)*0.1;
    }
    if(c < 0.5) {
      b = b + g(a,b,c)*0.1 + f(a,b,c)*0.1;
    } else {
      a = a + f(a,b,c)*0.1 + g(a,b,c)*0.1;
    }
    }
    on_finish([a,b,c]);
  }
})")
   (parse-expr "[[0.2, 0.7, 0.51],[0.2, 0.7, 0.51],[0.4, 0.6, 0.49],[0.8, 0.45, 0.21]]"))


  (pipeline-compile
   (car (parse "function X(a){return a();}"))
   *compiler-pipeline*)
  

  '(let ((harness (asdf:component-pathname
                   (asdf:find-component (asdf:find-component (asdf:find-system :jwacs-tests) "tests") "rt-test-harness"))))
    (defun compile-node-test (js-fn)
      ))

  '(cl:ensure-directories-exist
    (uiop/pathname:merge-pathnames*
     (uiop:temporary-directory)
     (random 10000))
    ))
