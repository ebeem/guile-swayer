;;; Commentary:

;;; Code:

(define-module (guile-swayer modules)
  #:use-module (guile-swayer modules auto-reload)
  #:use-module (guile-swayer modules general)
  #:use-module (guile-swayer modules kbd)
  #:use-module (guile-swayer modules which-key)
  #:use-module (guile-swayer modules workspace-grid)
  #:use-module (guile-swayer modules workspace-groups))

(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))

(re-export-modules (guile-swayer modules auto-reload)
                   (guile-swayer modules general)
                   (guile-swayer modules kbd)
                   (guile-swayer modules which-key)
                   (guile-swayer modules workspace-grid)
                   (guile-swayer modules workspace-groups))

;;; (modules) ends here
