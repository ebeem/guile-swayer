#!/usr/bin/guile
!#

;; assuming your are running from a path relative to swaypic & modules
;; you can hardcode the load path here if that assumption isn't valid.
;; you have to add to load path the directory the contains modules and swayipc
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
             (ice-9 pretty-print)
             (swayipc))


;; subscribe to all events
(sway-subscribe-all)

(define (workspace-changed event)
  (display "workspace-changed\n")
  (pretty-print event))

(add-hook! sway-workspace-hook workspace-changed)

(define (output-changed event)
  (display "output-changed\n")
  (pretty-print event))

(add-hook! sway-output-hook output-changed)

(define (mode-changed event)
  (display "mode-changed\n")
  (pretty-print event))

(add-hook! sway-mode-hook mode-changed)

(define (window-changed event)
  (display "window-changed\n")
  (pretty-print event))

(add-hook! sway-window-hook window-changed)

(define (binding-changed event)
  (display "binding-changed\n")
  (pretty-print event))

(add-hook! sway-binding-hook binding-changed)

(define (bar-config-changed event)
  (display "bar-config-changed\n")
  (pretty-print event))

(add-hook! sway-bar-config-hook bar-config-changed)

(define (shutdown-changed event)
  (display "shutdown-changed\n")
  (pretty-print event))

(add-hook! sway-shutdown-hook shutdown-changed)

(define (tick-changed event)
  (display "tick-changed\n")
  (pretty-print event))

(add-hook! sway-tick-hook tick-changed)

(define (bar-state-update-changed event)
  (display "bar-state-update-changed\n")
  (pretty-print event))

(add-hook! sway-bar-state-update-hook bar-state-update-changed)

(define (input-changed event)
  (display "input-changed\n")
  (pretty-print event))

(add-hook! sway-input-hook input-changed)

(sway-start-event-listener-thread)
(thread-join! SWAY-LISTENER-THREAD)
