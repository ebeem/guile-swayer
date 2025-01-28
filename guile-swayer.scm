;;; Commentary:

;;; Code:

(define-module (guile-swayer)
  #:use-module (guile-swayer swayipc)
  #:use-module (guile-swayer sjson)
  #:use-module (guile-swayer libs)
  #:use-module (guile-swayer modules))

(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))

(re-export-modules (guile-swayer swayipc)
                   (guile-swayer sjson)
                   (guile-swayer libs)
                   (guile-swayer modules))

;;; (guile-swayer) ends here
