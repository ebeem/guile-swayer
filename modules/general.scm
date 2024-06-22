(define-module (modules general)
  #:use-module (sjson parser)
  #:use-module (swayipc dispatcher)
  #:use-module (swayipc connection)
  #:use-module (srfi srfi-18)
  #:use-module (ice-9 hash-table)
  #:export (general-keybinding-translator
            general-commander-path
            general-configure
            general-define-keys
            general-define-key
            general-keybindings
            general-submaps))

;; Local copy of keybindings configured by general, it's recommended to
;; only use general to assign keybindings, otherwise this hashtable won't
;; by synced with actual keybindings available in sway
(define general-keybindings (make-hash-table))

;; Local copy of submaps configured by general, it's recommended to
;; only use general to assign submaps, otherwise this hashtable won't
;; by synced with actual submaps available in sway
(define general-submaps (make-hash-table))

;; add default submap, this is the default submap in sway
(hash-set! general-submaps "" "default")

(define (general-keybinding-translator key)
  "Translate a given key, passing a function can enable easier keybindings
like emacs key chords (refer to module modules/kbd.scm). The default implementation
doesn't modify passed keybindings"
  key)

;; The path of commander executable, it's used to send back scheme expressions
;; via unix socket.
(define general-commander-path
  (if current-filename
      (string-append (dirname (dirname current-filename)) "/commander")
      "commander"))

(define* (general-configure #:key keybinding-translator commander-path)
  "Configure keybinding-translator (refer to general-keybinding-translator) and
commander-path (refer to 'general-commander-path).
Parameters:
  - keybinding-translator: a function that takes a key and returns the translated version.
  - commander-path: a path to the commander executable to be used to send keybindings commands."
  (when keybinding-translator
    (set! general-keybinding-translator keybinding-translator))
  (when commander-path
    (set! general-commander-path commander-path)))

(define (exp->string exp)
  "Convert a given expression exp to a string."
  (call-with-output-string (lambda (p)
                             (write exp p))))

(define* (define-submap chord wk submap parent-submap)
  "Define a submap, this will be translated to sway by defining a keybinding
to switch to the provided submap, and add a default ESC keybinding to return
to the default submap.
Parameters:
	- chord: chord string to trigger the keybinding. e.g. (s-SPC f f).
	- wk: which-key's description.
	- submap: the name of the submap.
	- parent-submap: parent submap, default if submap is a child of the default submap."
  (let* ((chord-ls (map general-keybinding-translator
                        (string-split chord #\Space)))
         (key (car (last-pair chord-ls))))
    (format #t "define submap ~a\n" chord)
    (hash-set! general-submaps chord submap)
    (define-keybindings chord
      `(sway-mode ,submap)
      wk parent-submap)
    (define-keybindings (string-append chord " Esc")
      `(sway-mode "default")
      "Escape" submap)))

(define (last-key chord)
  "Return last key from a given chord."
  (car (reverse (string-split chord #\+))))

(define (general-command exp-str)
  "Execute a general command (scheme expression)"
  (string-append "exec '" general-commander-path " "
                 (exp->string exp-str) "'"))

(define* (define-keybindings chord exp wk submap)
  "Define a sway keybinding.
Parameters:
	- chord: chord string to trigger the keybinding. e.g. (s-SPC f f).
	- exp: expression to execute when the chord is triggered.
	- wk: which-key's description.
	- submap: the name of the submap."
  (let* ((chord-ls (map general-keybinding-translator
                        (string-split chord #\Space)))
         (key (car (last-pair chord-ls)))
         (type (if (string->number (last-key key)) "bindcode" "bindsym"))
         (command (string-append type " " key " " (general-command (exp->string exp))))
         (esc (string-append type " " key " " (general-command (exp->string `(and ,exp (sway-mode "default")))))))

    (hash-set! general-keybindings chord '(key exp wk submap))
    (if (equal? submap "default")
      (dispatch-command command)
      (begin
        (sway-mode-subcommand submap command)
        (unless (equal? "sway-mode" (symbol->string (car exp)))
          (sway-mode-subcommand submap esc))))))

(define (find-submap chord)
  "Return the submap for the provided chord.
If the submap isn't found, #f is returned."
  (or
   (hash-get-handle general-submaps
                    (string-join
                     (list-head chord (- (length chord) 1)) " "))
   #f))

(define* (general-define-key chord #:optional exp #:key wk submap)
  "Assign a given chord to a given expression.
Parameters:
	- chord: chord string to trigger the keybinding. e.g. (s-SPC f f).
	- exp: expression to execute when the chord is triggered.
	- wk: which-key's description.
	- submap: the name of the submap."
  (let* ((chord-ls
          (map general-keybinding-translator
               (string-split chord #\Space)))
         (key (car (last-pair chord-ls)))
         (chord (string-join chord-ls " "))
         (key-code (string->number key))
         (description (or wk (or submap (symbol->string (car exp)))))
         (tsubmap (cdr (find-submap chord-ls))))

    ;; if target submap isn't found, throw an error
    ;; the submap must always be defined before defining a keybinding
    (unless tsubmap
      (error 'find-submap "chord was not found in submaps, a submap has to be defined for it" chord))

    (if submap
        ;; if submap key is provided, then define a submap (ignore exp)
        (define-submap chord wk submap
          (cdr (find-submap chord-ls)))
        ;; otherwise, define a keybinding with exp
        (define-keybindings chord exp wk
          (cdr (find-submap chord-ls))))))

(define* (general-define-keys #:key parent-prefix prefix wk . args )
  "Assign given list of chords and expressions to a submap.
Parameters:
	- prefix: a prefix that's applied to all the keybindings passed. e.g. (s-Spc).
	- parent-prefix: a prefix of the parent (passed internally, you should probably use prefix instead).
	- wk: which-key's description of the submap.
	- args: list of keybindings to assign to the prefix, these will be passed to general-define-key.

For example:
(general-define-keys
  #:prefix \"s-Space\" #:wk \"Leader\"
  `(\"o\" (exec \"rofi -show drun\"))
  `(\"C-g\" (sway-mode \"default\") #:wk \"abort\")

  `(general-define-keys
    #:prefix \"r\" #:wk \"Rofi\"
    (\"p\" (exec \"~/.config/rofi/bin/password-manager\"))))"

  (when parent-prefix
    (set! prefix (string-append parent-prefix " " prefix)))

  (when prefix
    (format #t "define prefix submap: ~a, wk ~a\n" prefix wk)
    (general-define-key prefix #:submap wk))

  (map (lambda (arg)
         (when (list? arg)
            (if (and
                 (symbol? (car arg))
                 (equal? "general-define-keys" (symbol->string (car arg))))
              (apply general-define-keys (append `(#:parent-prefix ,prefix) (cdr arg)))
              (apply general-define-key
                  (cons (string-append
                        (if prefix (string-append prefix " ") "")
                        (car arg)) (cdr arg))))))
       args))

(define (custom-exception-handler exc command-id payload)
  "Exception handler for evaluating expressions from commander."
  (format #t "An error occurd while executing the received
general command: command: ~a, payload: ~a\n" command-id payload)
  (format #t "exception: ~a\n" exc))

;; add a hook to listen to received commands (usually from commander)
(add-hook! command-received-hook
           (lambda (command-id payload)
             (with-exception-handler
                 (lambda (exc)
                   (custom-exception-handler exc command-id payload))
               (lambda () (eval-string (json-string->scm payload)))
               #:unwind? #t)))
