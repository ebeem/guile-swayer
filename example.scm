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


;; get focused workspace from a list of workspaces
(define (focused-workspace-name workspaces)
  (cond
    ((null? workspaces) #f)
    ((equal? #t (sway-workspace-focused (car workspaces)))
     (sway-workspace-name (car workspaces)))
    (else (focused-workspace-name (cdr workspaces)))))

(format #t "output record from function #sway-get-workspaces:\n ~a\n"
        (sway-get-workspaces)) 

(format #t "current focused workspace is [~a]\n"
        (focused-workspace-name (sway-get-workspaces)))

;; assign simple keybindings
;; refer to the module modules/general.scm for easier interface
(sway-bindsym "Mod4+t" "exec alacritty")

;; subscribe to events
(define (workspace-changed workspace-event)
  (let* ((current-tree (sway-workspace-event-current workspace-event))
         (workspace (sway-tree-name current-tree)))

    (format #t "workspace changed to ~a!\n" workspace)))

;; subscribe to all events
(sway-subscribe-all)

(add-hook! sway-workspace-hook workspace-changed)
(sway-start-event-listener-thread)
(thread-join! SWAY-LISTENER-THREAD)
