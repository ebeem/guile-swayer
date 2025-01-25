;;; Commentary:

;;; Code:

(define-module (guile-swayer libs)
  #:use-module (guile-swayer libs sway-tree-helper))

(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))

(re-export-modules (guile-swayer libs sway-tree-helper))

;;; (modules) ends here
