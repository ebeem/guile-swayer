;;; Commentary:

;;; Code:

(define-module (guile-swayer swayipc)
  #:use-module (guile-swayer swayipc connection)
  #:use-module (guile-swayer swayipc dispatcher)
  #:use-module (guile-swayer swayipc events)
  #:use-module (guile-swayer swayipc info)
  #:use-module (guile-swayer swayipc records))

(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))

(re-export-modules (guile-swayer swayipc connection)
                   (guile-swayer swayipc dispatcher)
                   (guile-swayer swayipc events)
                   (guile-swayer swayipc info)
                   (guile-swayer swayipc records))

;;; (swayipc) ends here
