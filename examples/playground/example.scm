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
             (modules workspace-groups)
             (modules workspace-grid)
             (ice-9 pretty-print)
             (swayipc))

(sway-connect-sockets!)

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
