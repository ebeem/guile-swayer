(define-module (modules which-key)
  #:use-module (swayipc)
  #:use-module (modules general)
  #:use-module (ice-9 hash-table)
  #:use-module (srfi srfi-18)
  #:export (which-key-init
            which-key-configure
            which-key-delay-idle
            which-key-display-keybindings-hook
            which-key-event-counter
            which-key-hide-keybindings-hook))

;; idle delay before showing 
(define which-key-delay-idle 1.5)

;; used to keep track of how many events occured 
(define which-key-event-counter 0)

;; used to keep track of whether the which-key is active or not
(define which-key-active #f)

(define* (get-submap-keybindings submap #:optional (keybindings general-keybindings))
  "Return all provided submap keybindings.
Returned value is a list of keybindings. Each is a list
that contains the values: (CHORD KEY EXP WK SUBMAP)."
  (filter (lambda (ls) (equal? (list-ref ls 4) submap))
           (hash-map->list cons keybindings)))

;; emitted on submap change.
;; Parameters:
;;   - arg1: submap.
;;   - arg2: available keybindings.
(define which-key-display-keybindings-hook
  (make-hook 2))

;; emitted on submap change to default.
;; Parameters:
;;   - arg1: submap.
(define which-key-hide-keybindings-hook
  (make-hook 1))

(define (mode-changed mode-event)
  (let ((mode (sway-mode-event-change mode-event))
        (counter (+ 1 which-key-event-counter)))
    (set! which-key-event-counter counter)
    (if (equal? "default" mode)
        (begin
          (set! which-key-active #f)
          (run-hook which-key-hide-keybindings-hook mode))
        (thread-start!
          (make-thread
            (lambda ()
              (unless which-key-active
                (usleep (inexact->exact (* 1000000 which-key-delay-idle))))
              (when (equal? which-key-event-counter counter)
                (set! which-key-active #t)
                (run-hook which-key-display-keybindings-hook mode (get-submap-keybindings mode)))))))))

(define* (which-key-configure #:key delay-idle)
  "Configure which-key
Parameters:
  - delay-idle: the idle delay before showing the which key menu."
  (when delay-idle
    (set! which-key-delay-idle delay-idle)))

(define (which-key-init)
  "Initialize the which-key module."
  (add-hook! sway-mode-hook mode-changed)
  (display "starting which-key\n"))
