#!/usr/bin/env guile
!#

;; the below snippet assumes your are running the script
;; from a path relative to guile-swayer. In case you're not,
;; you have to add guile-swayer's directory to your load path
;; the below is adding guile-swayer by going to parent directory twice.
(let ((path (dirname
              (dirname
               (dirname (current-filename))))))
  (format #t "adding folder to load path ~a\n" path)
  (add-to-load-path path))

;; you can simply comment the above section and hardcode the path as below
;; (add-to-load-path "/home/YOUR_USER_HERE/git/guile-swayer")

;; if you would like to be relative to home, do as below 
;; (string-append (getenv "HOME") "/git/guile-swayer")

(use-modules (oop goops)
             (srfi srfi-18)
             (guile-swayer modules layout-alternating)
             (ice-9 pretty-print)
             (guile-swayer swayipc))

(sway-connect-sockets!)
(layout-alternating-configure)
(layout-alternating-init)

;; subscribe to all events
(sway-subscribe-all)

(sway-start-event-listener-thread)
(thread-join! SWAY-LISTENER-THREAD)
