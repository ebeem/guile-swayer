(define-module (modules kbd)
  #:use-module (ice-9 hash-table)
  #:export (kbd-init
            define-keysym
            define-modsym
            keysym-translations
            modsym-translations
            keysym-clean
            kbd))

;; Hashmap that's used to translate key symbols
;; Use define-keysym to add and translations
(define keysym-translations (make-hash-table))

;; Hashmap that's used to translate modifier symbols
;; Use define-modsym to add and translations
(define modsym-translations (make-hash-table))

(define (replace-char str old-char new-char)
  "Replace all occurances of a character (old-char) with the
charcter (new-char)."
  (string-map
   (lambda (ch)
     (if (char=? ch old-char)
         new-char
         ch))
   str))

(define (keysym-clean key)
  "Clean the keysym, it replaces all underscores (_) with dashes (-)
and lowercase all characters. This is done to make it easier to translate
symbols, for example: Spc and SPC should both be translated to the same
symbol. Note: symbols shouldn't include letter like a or b."
  (if (<= (string-length (string-trim-both key)) 1)
      key
      (string-trim-both
       (string-downcase
        (replace-char key #\_ #\-)))))

(define (define-keysym key translation)
  "Define a mapping from a symbol to a sway compatible symbol in keys
Parameters:
	- key: the key to look for in the keybinding.
	- translation: the symbol to convert the key to.

Example:
  (define-keysym \"SPC\" \"space\")

With the above definition, kbd will translate the keys below.
\"s-spc\" => \"s-space\" 
\"s-Spc\" => \"s-space\" 
\"s-SPC\" => \"s-space\" 

For more information why case is ignored, refer or modify keysym-clean."
  (hash-set! keysym-translations (keysym-clean key) translation))

(define (define-modsym key translation)
  "Define a mapping from a symbol to a sway compatible symbol in modifier keys
Parameters:
	- key: the key to look for in the keybinding.
	- translation: the symbol to convert the key to.

Example:
  (define-modsym \"s-\" \"mod4+\")

With the above definition, kbd will translate the keys below.
\"s-spc\" => \"mod4+space\" 

Note: unlike define-keysym, keysym-clean is not used."
  (hash-set! modsym-translations key translation))

(define* (replace-modifiers key #:optional (translation ""))
  "Replace modifier keys in the given key with translations
defined in modsym-translations."
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
  "Replace keys in the given key with translations
defined in keysym-translations."
  (let* ((lkey (keysym-clean key))
          (translation (hash-get-handle keysym-translations lkey)))
     (if (pair? translation)
         (cdr translation)
         key)))

(define (sway-key key)
  "Replace the provided key/chord with translations defined in both
modsym-translations and keysym-translations."
  (let* ((modifier (replace-modifiers key))
         (rkey (replace-key-symbols (list-ref modifier 1))))
    (string-append (list-ref modifier 0)
                   (if (number? rkey)
                       (number->string rkey)
                       rkey))))

(define (kbd seq)
  "Return sway compatible keybinding symbols from emacs like key sequence."
  (string-join
   (map sway-key
        (string-split seq #\Space)) " "))

(define (kbd-init)
  "Definie initial translations"

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
