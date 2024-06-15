#!/usr/bin/guile
!#

(add-to-load-path
 (dirname (or (current-filename)
              (string-append (getenv "HOME") "/.config/sway/init.scm"))))

(use-modules (oop goops)
             (srfi srfi-18)
             (modules workspace-groups)
             (modules workspace-grid)
             (swayipc connection)
             (swayipc records)
             (swayipc info)
             (swayipc events)
             (swayipc dispatcher))

(load "behavior.scm")
;; init keybindings
(load "keybindings.scm")
(keybindings-init)

;; subscribe to all events
(sway-subscribe-all)

(set! OUTPUTS '("HDMI-A-2" "DP-1" "DP-2"))
(set! GROUPS
  '(("11-browser" 		"21-browser" 		"31-browser")
    ("12-development" 	"22-development" 	"32-development")
    ("13-databases" 	"23-databases" 		"33-databases")
    ("14-communication" "24-communication" 	"34-communication")
    ("15-development" 	"25-development" 	"35-development")
    ("16-gaming" 		"26-gaming" 		"36-gaming")
    ("17-mail" 			"27-mail" 			"37-mail")
    ("18-development" 	"28-development" 	"38-development")
    ("19-media" 		"29-media" 			"39-media")))

(workspace-groups-init)

(set! ROWS 3)
(set! COLUMNS 3)
(set! WORKSPACES (apply map list GROUPS))

(workspace-grid-init)

;; TODO: load which key module

(start-commands-listener-thread)
(start-event-listener-thread)
(thread-join! LISTENER-THREAD)
(thread-join! COMMANDS-LISTENER-THREAD)
