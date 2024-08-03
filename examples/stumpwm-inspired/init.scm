#!/usr/bin/env guile
!#

;; assuming your are running from a path relative to swaypic & modules
;; you can hardcode the load path here if that assumption isn't valid.
;; you have to add to load path the directory the contains modules and guile-swayer
;; these 2 directories exist in the root directory of the repostiry and are
;; supposed to be 2 parent levels away from this init file.
(let ((path (dirname
             (dirname
              (dirname (current-filename))))))
  (format #t "adding folder to load path ~a\n" path)
  (add-to-load-path path))

;; you can simply uncomment the above section and hardcode the path as below
;; (add-to-load-path "/home/YOUR_USER_HERE/git/guile-swayer")

;; if you would like to be relative to home, do as below 
;; (string-append (getenv "HOME") "/.config/sway/init.scm")

(use-modules (oop goops)
             (srfi srfi-18)
             (modules workspace-groups)
             (modules workspace-grid)
             (modules auto-reload)
             (modules which-key)
             (swayipc))

(sway-connect-sockets!)

;; load look and feel
;; a separate scheme file for look and feel configuration
(load "behavior.scm")

;; init keybindings
;; a separate scheme file for keybindings (using general)
(load "keybindings.scm")
(keybindings-init)

;; subscribe to all events
(sway-subscribe-all)

;; configure workspace groups to sync groups
(define OUTPUTS '("HDMI-A-2" "DP-1" "DP-2"))
(define GROUPS
  '(("11-browser" 		"21-browser" 		"31-browser")
    ("12-development" 	"22-development" 	"32-development")
    ("13-databases" 	"23-databases" 		"33-databases")
    ("14-communication" "24-communication" 	"34-communication")
    ("15-development" 	"25-development" 	"35-development")
    ("16-gaming" 		"26-gaming" 		"36-gaming")
    ("17-mail" 			"27-mail" 			"37-mail")
    ("18-development" 	"28-development" 	"38-development")
    ("19-media" 		"29-media" 			"39-media")))

(workspace-groups-configure #:groups GROUPS #:outputs OUTPUTS)
(workspace-groups-init)

;; configure workspace grid to arrange workspaces in a matrix
(define ROWS 3)
(define COLUMNS 3)
(define WORKSPACES (apply map list GROUPS))

(workspace-grid-configure #:rows ROWS #:columns COLUMNS #:workspaces WORKSPACES)
(workspace-grid-init)

;; configure auto reload to automatically reload sway when a config file changes
(auto-reload-configure #:directories
                       `(,(string-append (getenv "HOME") "/.config/sway/")))
(auto-reload-init)

;; configure which key to show available keybindings
(which-key-configure #:delay-idle 1.2)
(which-key-init)

(define (show-rofi-message msg)
  (hide-rofi-message)
  (display (format #f "rofi -e \"~a\"" msg))
  (system (format #f "rofi -e \"~a\"" msg)))

(define (hide-rofi-message)
  (system "pkill -f '.*rofi -e.*'"))

(define (show-which-key submap bindings)
  (format #t "Displaying Submap ~a Bindings:\n" submap)
  (let ((message ""))
    ;; printing to display (via repl/terminal)
    (for-each
     (lambda (ls)
       (let ((nmsg (format #f "    - ~a -> ~a\n" (list-ref ls 1) (list-ref ls 3))))
        (display nmsg)
        (set! message (string-append message nmsg))))
     bindings)

    ;; showing in rofi
    (show-rofi-message message)))

(define (hide-which-key submap)
  (format #t "Hiding Submap Bindings:\n")
  ;; hide your which-key viewer (rofi, eww, etc.)
  (hide-rofi-message))

;; add the display and hide hook functions
(add-hook! which-key-display-keybindings-hook show-which-key)
(add-hook! which-key-hide-keybindings-hook hide-which-key)

;; start listening to sway events
(sway-start-event-listener-thread)
(thread-join! SWAY-LISTENER-THREAD)
