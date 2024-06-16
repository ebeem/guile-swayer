(define-module (modules kbd)
  #:use-module (ice-9 hash-table)
  #:export (kbd-init
            define-keysym
            define-modsym
            keysym-translations
            modsym-translations
            keysym-clean
            kbd))

(define keysym-translations (make-hash-table))
(define modsym-translations (make-hash-table))

(define (replace-char str old-char new-char)
  (string-map
   (lambda (ch)
     (if (char=? ch old-char)
         new-char
         ch))
   str))

(define (keysym-clean key)
  (if (<= (string-length (string-trim-both key)) 1)
      key
      (string-trim-both
       (string-downcase
        (replace-char key #\_ #\-)))))

(define (define-keysym key translation)
  "Define a mapping from a modifier to a code."
  (hash-set! keysym-translations (keysym-clean key) translation))

(define (define-modsym key translation)
  "Define a mapping from a key to a code."
  (hash-set! modsym-translations key translation))

(define (string-starts-with? str prefix)
  (let ((prefix-length (string-length prefix)))
    (and (>= (string-length str) prefix-length)
         (string=? (substring str 0 prefix-length) prefix))))

(define* (replace-modifiers key #:optional (translation ""))
  (cond
   ((< (string-length key) 2) (list translation key))
   ((hash-get-handle modsym-translations (substring key 0 2))
    (replace-modifiers
     (substring key 2)
     (string-append
      translation
      (cdr (hash-get-handle modsym-translations (substring key 0 2))))))
   (else (list translation key))))

(define* (replace-key-symbols key)
  (let* ((lkey (keysym-clean key))
          (translation (hash-get-handle keysym-translations lkey)))
     (if (pair? translation)
         (cdr translation)
         key)))

(define (sway-key key)
  (let* ((modifier (replace-modifiers key))
         (rkey (replace-key-symbols (list-ref modifier 1))))
    (string-append (list-ref modifier 0)
                   (if (number? rkey)
                       (number->string rkey)
                       rkey))))

(define (kbd seq)
  "return sway compatible keybinding symbols from emacs like key sequence"
  (string-join
   (map sway-key
        (string-split seq #\Space)) " "))

(define (kbd-init)
  ;; key modifiers
  (define-modsym "C-" "Control+")
  (define-modsym "S-" "Shift+")
  (define-modsym "s-" "mod4+")
  (define-modsym "M-" "Alt+")
  (define-modsym "C-" "Control+")

  ;; key symbols
  (define-keysym "RET" "Return")
  (define-keysym "ESC" "Escape")
  (define-keysym "TAB" "Tab")
  (define-keysym "DEL" "BackSpace")
  (define-keysym "SPC" "space")
  (define-keysym "!" "exclam")
  (define-keysym "\"" "quotedbl")
  (define-keysym "$" "dollar")
  (define-keysym "£" "sterling")
  (define-keysym "%" "percent")
  (define-keysym "&" "ampersand")
  (define-keysym "'" "apostrophe")
  (define-keysym "`" "grave")
  (define-keysym "&" "ampersand")
  (define-keysym "(" "parenleft")
  (define-keysym ")" "parenright")
  (define-keysym "*" "asterisk")
  (define-keysym "+" "plus")
  (define-keysym "," "comma")
  (define-keysym "-" "minus")
  (define-keysym "." "period")
  (define-keysym "/" "slash")
  (define-keysym ":" "colon")
  (define-keysym ";" "semicolon")
  (define-keysym "<" "less")
  (define-keysym "=" "equal")
  (define-keysym ">" "greater")
  (define-keysym "?" "question")
  (define-keysym "@" "at")
  (define-keysym "[" "bracketleft")
  (define-keysym "\\" "backslash")
  (define-keysym "]" "bracketright")
  (define-keysym "^" "asciicircum")
  (define-keysym "_" "underscore")
  (define-keysym "#" "numbersign")
  (define-keysym "{" "braceleft")
  (define-keysym "|" "bar")
  (define-keysym "}" "braceright")
  (define-keysym "~" "asciitilde")
  (define-keysym "<" "quoteleft")
  (define-keysym ">" "quoteright"))
