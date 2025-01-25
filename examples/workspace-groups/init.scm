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
             (guile-swayer modules workspace-groups)
             (guile-swayer modules workspace-grid)
             (guile-swayer modules auto-reload)
             (guile-swayer modules which-key)
             (guile-swayer swayipc))

(sway-connect-sockets!)

;; subscribe to all events
(sway-subscribe-all)

;; configure workspace groups to sync groups
;; NOTE: change outputs according to your outputs
;; to get your output names, use:
;; swaymsg -t get_outputs -r | jq -r '.[].name'
(define OUTPUTS '("HDMI-A-2" "DP-1" "DP-2"))

;; number of workspaces per group must be equal to number of outputs
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

;; start listening to sway events
(sway-start-event-listener-thread)
(thread-join! SWAY-LISTENER-THREAD)
