(define-module (modules general)
  #:use-module (sjson parser)
  #:use-module (swayipc dispatcher)
  #:use-module (swayipc connection)
  #:use-module (srfi srfi-18)
  #:use-module (ice-9 hash-table)
  #:export (sway-keybinding-translator
            configure-sway-keybinding-translator
            sway-define-keys
            sway-define-key
            sway-keybindings
            sway-submaps))

;; Local copy of keybindings configured by general, it's recommended to
;; only use general to assign keybindings, otherwise this hashtable won't
;; by synced with actual keybindings available in sway
(define sway-keybindings (make-hash-table))

;; Local copy of submaps configured by general, it's recommended to
;; only use general to assign submaps, otherwise this hashtable won't
;; by synced with actual submaps available in sway"
(define sway-submaps (make-hash-table))
(hash-set! sway-submaps "" "default")

(define (sway-keybinding-translator key)
  "Translates keybindings, passing kbd function will enable emacs
   like key chords. The default implementation doesn't modify passed keybindings"
  key)

(define (configure-sway-keybinding-translator proc)
  (set! sway-keybinding-translator proc))

(define (exp->string exp)
  (call-with-output-string (lambda (p)
                             (write exp p))))

(define* (define-submap chord key wk submap parent-submap)
  (display (string-append "define submap " chord "\n"))
  (hash-set! sway-submaps chord submap)
  (define-keybindings chord key
    `(sway-mode ,submap)
    wk parent-submap)
  (define-keybindings (string-append chord " Esc")
    "Escape" `(sway-mode "default")
    "Escape" submap))

(define (last-key key)
  (car (reverse (string-split key #\+))))

(define (sway-command exp-str)
  (string-append "exec '" (dirname (dirname (current-filename))) "/commander "
                 (exp->string exp-str) "'"))

(define* (define-keybindings chord key exp wk submap)
  (hash-set! sway-keybindings chord '(key exp wk submap))
  (let* ((type (if (string->number (last-key key)) "bindcode" "bindsym"))
         (command (string-append type " " key " " (sway-command (exp->string exp))))
         (esc (string-append type " " key " " (sway-command (exp->string `(and ,exp (sway-mode "default")))))))
    (if (equal? submap "default")
      (dispatch-command command)
      (begin
        (sway-mode-subcommand submap command)
        (unless (equal? "sway-mode" (symbol->string (car exp)))
          (display "escape after executing\n")
          (display esc)
          (sway-mode-subcommand submap esc))))))

(define (find-submap chord)
  (or
   (hash-get-handle sway-submaps
                    (string-join
                     (list-head chord (- (length chord) 1)) " "))
   (error 'find-submap "chord was not found in submaps, a submap has to be defined for it" chord)))

(define* (sway-define-key chord #:optional exp #:key wk submap)
  "assign a key to a given expression"
  (let* ((chord-ls
          (map sway-keybinding-translator
               (string-split chord #\Space)))
         (chord (string-join chord-ls " "))
         (key (car (last-pair chord-ls)))
         (key-code (string->number key))
         (description (or wk (or submap (symbol->string (car exp))))))
    (if submap
        (define-submap chord key wk submap
          (cdr (find-submap chord-ls)))
        (define-keybindings chord key exp wk
          (cdr (find-submap chord-ls))))))

(define* (sway-define-keys #:key parent-prefix prefix wk . args )
  (when parent-prefix
    (set! prefix (string-append parent-prefix " " prefix)))

  (when prefix
    (display (string-append "define prefix submap: " prefix ", wk " wk "\n"))
    (sway-define-key prefix #:submap wk))

  (map (lambda (arg)
         (when (list? arg)

            (display "ARG: ")
            (display arg)
            (newline)
            (display (car arg))
            (newline)
            (if (and
                 (symbol? (car arg))
                 (equal? "sway-define-keys" (symbol->string (car arg))))
              (apply sway-define-keys (append `(#:parent-prefix ,prefix) (cdr arg)))
              (apply sway-define-key
                  (cons (string-append
                        (if prefix (string-append prefix " ") "")
                        (car arg)) (cdr arg))))))
       args))

(define (custom-exception-handler exc command-id payload)
  (display "An error occurred: ")
  (display (exp->string exc))
  (newline)
  (display (string-append "command: " (number->string command-id) ", payload: " payload)))

(add-hook! command-received-hook
           (lambda (command-id payload)
            (eval-string (json-string->scm payload))))
