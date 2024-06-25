;;; Commentary:

;;; Code:

(define-module (swayipc)
  #:use-module (swayipc connection)
  #:use-module (swayipc dispatcher)
  #:use-module (swayipc events)
  #:use-module (swayipc info)
  #:use-module (swayipc records))

(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))

(re-export-modules (swayipc connection)
                   (swayipc dispatcher)
                   (swayipc events)
                   (swayipc info)
                   (swayipc records))

;;; (swayipc) ends here
