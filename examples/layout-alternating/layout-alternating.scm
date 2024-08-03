#!/usr/bin/env guile
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
             (modules layout-alternating)
             (ice-9 pretty-print)
             (swayipc))

(sway-connect-sockets!)
(layout-alternating-configure)
(layout-alternating-init)

;; subscribe to all events
(sway-subscribe-all)

(sway-start-event-listener-thread)
(thread-join! SWAY-LISTENER-THREAD)
