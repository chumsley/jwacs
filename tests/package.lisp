;;;; package.lisp
;;; Defines the package used by the unit tests

(defpackage :jwacs-tests
  (:use :cl :rtest :cl-ppcre :jwacs)
  (:nicknames :jw-tests)
  (:import-from jwacs
                ;; lexer-specific symbols for testing
                regexp-re
                make-javascript-lexer
                eoi
                make-load-form

                ;; source-model structure types
                source-element
                special-value
                identifier
                numeric-literal
                string-literal
                array-literal
                object-literal
                re-literal
                new-expr
                fn-call
                property-access
                unary-operator
                binary-operator
                conditional
                comma-expr
                var-decl-statement
                var-decl
                statement-block
                if-statement
                do-statement
                while
                for
                for-in
                continue-statement
                break-statement
                return-statement
                with
                switch
                case-clause
                default-clause
                label
                throw-statement
                try
                catch-clause
                finally-clause
                function-decl
                function-expression
                suspend-statement
                resume-statement
                
                ;; constructors for source-model structures
                make-source-element
                make-special-value
                make-identifier
                make-numeric-literal
                make-string-literal
                make-array-literal
                make-object-literal
                make-re-literal
                make-new-expr
                make-fn-call
                make-property-access
                make-unary-operator
                make-binary-operator
                make-conditional
                make-comma-expr
                make-var-decl-statement
                make-var-decl
                make-statement-block
                make-if-statement
                make-do-statement
                make-while
                make-for
                make-for-in
                make-continue-statement
                make-break-statement
                make-return-statement
                make-with
                make-switch
                make-case-clause
                make-default-clause
                make-label
                make-throw-statement
                make-try
                make-catch-clause
                make-finally-clause
                make-function-decl
                make-function-expression
                make-suspend-statement
                make-resume-statement
                
                ;; structure management
                structure-slots

                ;; pretty-printer-specific symbols
                pretty-print
                with-indent
                *indent*

                ;; ugly-printer symbols
                genvar
                *genvar-counter*
                ugly-print

                ;; source-transformation-specific symbols
                *cont-name*
                *cont-id*
                transform
                cps
                explicitize
                shift-function-decls
                loop-to-function
                uniquify
                in-local-scope
                ))
                