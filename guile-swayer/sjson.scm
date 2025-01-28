;;; Commentary:

;;; Code:

(define-module (guile-swayer sjson)
  #:use-module (guile-swayer sjson builder)
  #:use-module (guile-swayer sjson parser)
  #:use-module (guile-swayer sjson record))

(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))

(re-export-modules (guile-swayer sjson builder)
                   (guile-swayer sjson parser)
                   (guile-swayer sjson record))

;;; (sjson) ends here
