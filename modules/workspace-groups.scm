;; use example:

;; (workspace-groups-configure
;;   #:outputs '("DP-1" "DP-2")
;;   #:groups '(("dp-1-browsing" "dp-2-browsing")
;;              ("dp-1-programming" "dp-2-programming")))
;; (workspace-groups-init)

(define-module (modules workspace-groups)
  #:use-module (swayipc)

  #:export (workspace-groups-init
            workspace-groups-configure
            workspace-groups-outputs
            workspace-groups-groups))

;; The order in which the outputs are organized, it's important that
;; the order of outputs match the order of workspaces in `WORKSPACE-GROUPS`
(define workspace-groups-outputs '())

;; The workspace groups, each is a list of workspace names
;; if a workspace of this list is activated, the rest of the workspaces
;; will be activated as well.
(define workspace-groups-groups '())

(define* (workspace-groups-configure #:key outputs groups)
  "Configure workspace groups.
Parameters:
	- outputs: list of outputs for the groups (they must match the order in groups).
	- groups: list of list of workspaces to sync.

Example: configuring workspaces \"dp-1-browsing\" \"dp-2-browsing\" to span together
also configuring \"dp-1-programming\" \"dp-2-programming\" to span together.
This means when ever the workspace \"dp-1-browsing\" is focused, the workspace
\"dp-2-browsing\" will silently be focused as well (switched to, but not focused).

(workspace-groups-configure
  #:outputs '(\"DP-1\" \"DP-2\")
  #:groups '((\"dp-1-browsing\" \"dp-2-browsing\")
             (\"dp-1-programming\" \"dp-2-programming\")))"
  (when outputs (set! workspace-groups-outputs outputs))
  (when groups (set! workspace-groups-groups groups)))


;; keep track of last switched group, prevents switching to
;; group that's already focused.
(define last-switched-group '())

(define* (is-workspace-focused workspace output
                              #:optional (outputs (sway-get-outputs)))
  "Return whether a workspace is focused in an output."
  (cond
   ((null? outputs) #f)
   ((equal? output (sway-output-name (car outputs)))
    (equal? workspace (sway-output-current-workspace (car outputs))))
   (else (is-workspace-focused workspace output (cdr outputs)))))

(define (switch-to-workspace-group group initiator)
  "Switch to a workspace group, causing all workspaces in that group to be focused.
Parameters:
	- group: the group of workspaces (workspace name list).
	- initiator: the name of the workspace to be focused after switching to the group.
Note: the last focused workspace is initiator. It will be the actually focused workspace."
  (unless (equal? last-switched-group group)
    (let* ((initiator-output ""))
      (set! last-switched-group group)
      (newline)
      (for-each
        (lambda (workspace output)
          (if (equal? workspace initiator)
              (set! initiator-output output)
              (unless (is-workspace-focused workspace output)
                (sway-switch-workspace workspace))))
        group workspace-groups-outputs)

      ;; switch to initiator at last so the focus behaves as expected
      (sway-switch-workspace initiator))))

(define (focused-workspace workspaces)
  "Return focused workspace name from a list of sway workspace.
#f is returned if the workspace isn't found in the list."
  (cond
   ((null? workspaces) #f)
   ((equal? #t (sway-workspace-focused (car workspaces)))
    (sway-workspace-name (car workspaces)))
   (else (focused-workspace (cdr workspaces)))))

(define (workspace-changed workspace-event)
  "Triggered when sway workspace has changed. This function retrives
and focused all other workspaces in the group."
  (let* ((current-tree (sway-workspace-event-current workspace-event))
         (workspace (sway-tree-name current-tree))
         (focused-workspace (focused-workspace (sway-get-workspaces))))

    ;; sometimes there is a delay in events, it's neccessary to ensure
    ;; that event workspace is same as the currently focused workspace
    (when (equal? workspace focused-workspace)
      (unless (member workspace last-switched-group)
        (set! last-switched-group '()))
      (for-each
      (lambda (group)
        (when (member workspace group)
          (switch-to-workspace-group group workspace)))
    workspace-groups-groups))))

(define (pin-workspaces-to-output groups outputs)
  "Pin the groups provided to the outputs. This is called while initializing
to ensure that whenever a workspace is focused, it goes to the outputs it's assigned to.

Parameters:
	- outputs: list of outputs for the groups (they must match the order in groups).
	- groups: list of list of workspaces to sync."
  (for-each
   (lambda (group)
     (for-each
      (lambda (workspace output)
        (sway-switch-workspace-on-output workspace output))
      group outputs))
   groups))

(define (workspace-groups-init)
  "Initialize the workspace groups."
  ;; pin workspaces to output
  (pin-workspaces-to-output workspace-groups-groups workspace-groups-outputs)
  (add-hook! sway-workspace-hook workspace-changed))
