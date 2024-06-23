(define-module (modules kbd)
  #:use-module (ice-9 hash-table)
  #:export (kbd-init
            kbd-define-keysym
            kbd-define-modsym
            kbd-keysym-translations
            kbd-modsym-translations
            kbd-keysym-clean
            kbd-translate))

;; Hashmap that's used to translate key symbols
;; Use kbd-define-keysym to add and translations
(define kbd-keysym-translations (make-hash-table))

;; Hashmap that's used to translate modifier symbols
;; Use kbd-define-modsym to add and translations
(define kbd-modsym-translations (make-hash-table))

(define (replace-char str old-char new-char)
  "Replace all occurances of a character (old-char) with the
charcter (new-char)."
  (string-map
   (lambda (ch)
     (if (char=? ch old-char)
         new-char
         ch))
   str))

(define (kbd-keysym-clean key)
  "Clean the keysym, it replaces all underscores (_) with dashes (-)
and lowercase all characters. This is done to make it easier to translate
symbols, for example: Spc and SPC should both be translated to the same
symbol. Note: symbols shouldn't include letter like a or b."
  (if (<= (string-length (string-trim-both key)) 1)
      key
      (string-trim-both
       (string-downcase
        (replace-char key #\_ #\-)))))

(define (kbd-define-keysym key translation)
  "Define a mapping from a symbol to a sway compatible symbol in keys
Parameters:
	- key: the key to look for in the keybinding.
	- translation: the symbol to convert the key to.

Example:
  (kbd-define-keysym \"SPC\" \"space\")

With the above definition, kbd will translate the keys below.
\"s-spc\" => \"s-space\" 
\"s-Spc\" => \"s-space\" 
\"s-SPC\" => \"s-space\" 

For more information why case is ignored, refer or modify kbd-keysym-clean."
  (hash-set! kbd-keysym-translations (kbd-keysym-clean key) translation))

(define (kbd-define-modsym key translation)
  "Define a mapping from a symbol to a sway compatible symbol in modifier keys
Parameters:
	- key: the key to look for in the keybinding.
	- translation: the symbol to convert the key to.

Example:
  (kbd-define-modsym \"s-\" \"mod4+\")

With the above definition, kbd will translate the keys below.
\"s-spc\" => \"mod4+space\" 

Note: unlike kbd-define-keysym, kbd-keysym-clean is not used."
  (hash-set! kbd-modsym-translations key translation))

(define* (replace-modifiers key #:optional (translation ""))
  "Replace modifier keys in the given key with translations
defined in kbd-modsym-translations."
  (cond
   ((< (string-length key) 2) (list translation key))
   ((hash-get-handle kbd-modsym-translations (substring key 0 2))
    (replace-modifiers
     (substring key 2)
     (string-append
      translation
      (cdr (hash-get-handle kbd-modsym-translations (substring key 0 2))))))
   (else (list translation key))))

(define* (replace-key-symbols key)
  "Replace keys in the given key with translations
defined in kbd-keysym-translations."
  (let* ((lkey (kbd-keysym-clean key))
          (translation (hash-get-handle kbd-keysym-translations lkey)))
     (if (pair? translation)
         (cdr translation)
         key)))

(define (sway-key key)
  "Replace the provided key/chord with translations defined in both
kbd-modsym-translations and kbd-keysym-translations."
  (let* ((modifier (replace-modifiers key))
         (rkey (replace-key-symbols (list-ref modifier 1))))
    (string-append (list-ref modifier 0)
                   (if (number? rkey)
                       (number->string rkey)
                       rkey))))

(define (kbd-translate seq)
  "Return sway compatible keybinding symbols from emacs like key sequence."
  (string-join
   (map sway-key
        (string-split seq #\Space)) " "))

(define (kbd-init)
  "Definie initial translations"

  ;; key modifiers
  (kbd-define-modsym "C-" "Control+")
  (kbd-define-modsym "S-" "Shift+")
  (kbd-define-modsym "s-" "mod4+")
  (kbd-define-modsym "M-" "Alt+")
  (kbd-define-modsym "C-" "Control+")

  ;; key symbols
  (kbd-define-keysym "RET" "Return")
  (kbd-define-keysym "ESC" "Escape")
  (kbd-define-keysym "TAB" "Tab")
  (kbd-define-keysym "DEL" "BackSpace")
  (kbd-define-keysym "SPC" "space")
  (kbd-define-keysym "!" "exclam")
  (kbd-define-keysym "\"" "quotedbl")
  (kbd-define-keysym "$" "dollar")
  (kbd-define-keysym "£" "sterling")
  (kbd-define-keysym "%" "percent")
  (kbd-define-keysym "&" "ampersand")
  (kbd-define-keysym "'" "apostrophe")
  (kbd-define-keysym "`" "grave")
  (kbd-define-keysym "&" "ampersand")
  (kbd-define-keysym "(" "parenleft")
  (kbd-define-keysym ")" "parenright")
  (kbd-define-keysym "*" "asterisk")
  (kbd-define-keysym "+" "plus")
  (kbd-define-keysym "," "comma")
  (kbd-define-keysym "-" "minus")
  (kbd-define-keysym "." "period")
  (kbd-define-keysym "/" "slash")
  (kbd-define-keysym ":" "colon")
  (kbd-define-keysym ";" "semicolon")
  (kbd-define-keysym "<" "less")
  (kbd-define-keysym "=" "equal")
  (kbd-define-keysym ">" "greater")
  (kbd-define-keysym "?" "question")
  (kbd-define-keysym "@" "at")
  (kbd-define-keysym "[" "bracketleft")
  (kbd-define-keysym "\\" "backslash")
  (kbd-define-keysym "]" "bracketright")
  (kbd-define-keysym "^" "asciicircum")
  (kbd-define-keysym "_" "underscore")
  (kbd-define-keysym "#" "numbersign")
  (kbd-define-keysym "{" "braceleft")
  (kbd-define-keysym "|" "bar")
  (kbd-define-keysym "}" "braceright")
  (kbd-define-keysym "~" "asciitilde")
  (kbd-define-keysym "<" "quoteleft")
  (kbd-define-keysym ">" "quoteright"))
