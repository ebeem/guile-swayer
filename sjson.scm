;;; Commentary:

;;; Code:

(define-module (sjson)
  #:use-module (sjson builder)
  #:use-module (sjson parser)
  #:use-module (sjson record))

(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))

(re-export-modules (sjson builder)
                   (sjson parser)
                   (sjson record))

;;; (sjson) ends here
