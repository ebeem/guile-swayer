;; use example:

;; (set! OUTPUTS '("HDMI-A-2" "DP-1" "DP-2"))
;; (set! GROUPS
;;   '(("10" "20" "30")
;;     ("11" "21" "31")
;;     ("12" "22" "32")
;;     ("13" "23" "33")
;;     ("14" "24" "34")
;;     ("15" "25" "35")
;;     ("16" "26" "36")
;;     ("17" "27" "37")
;;     ("18" "28" "38")
;;     ("19" "29" "39")))

;; (workspace-groups-init)

(define-module (modules workspace-groups)
  #:use-module (swayipc records)
  #:use-module (swayipc info)
  #:use-module (swayipc dispatcher)
  #:use-module (swayipc events)

  #:export (workspace-groups-init
            OUTPUTS
            GROUPS))

;; The order in which the outputs are organized, it's important that
;; the order of outputs match the order of workspaces in `WORKSPACE-GROUPS`
;; example:
;; (set! OUTPUTS '("HDMI-A-2" "DP-1" "DP-2"))
(define OUTPUTS '())

;; The workspace groups, each is a list of workspace names
;; if a workspace of this list is activated, the rest of the workspaces
;; will be activated as well.
;; example: 
;; (set! GROUPS
;;   '(("10" "20" "30")
;;     ("19" "29" "39")))
(define GROUPS '())

(define (configure-outputs outputs)
  (set! OUTPUTS outputs))

(define (configure-groups groups)
  (set! GROUPS groups))

(define last-switched-group '())

(define (is-workspace-focused workspace output outputs)
  (cond
   ((null? outputs) #f)
   ((equal? output (sway-output-name (car outputs)))
    (equal? workspace (sway-output-current-workspace (car outputs))))
   (else (is-workspace-focused workspace output (cdr outputs)))))

(define (switch-to-workspace-group group initiator)
  (unless (equal? last-switched-group group)
    (let* ((initiator-output "")
           (outputs (sway-get-outputs)))
      (set! last-switched-group group)
      (newline)
      (for-each
        (lambda (workspace output)
          (if (equal? workspace initiator)
              (set! initiator-output output)
              (unless (is-workspace-focused workspace output outputs)
                (sway-switch-workspace workspace))))
        group OUTPUTS)

      ;; switch to initiator at last so the focus behaves as expected
      (sway-switch-workspace initiator))))

(define (focused-workspace workspaces)
  (cond
   ((null? workspaces) #f)
   ((equal? #t (sway-workspace-focused (car workspaces)))
    (sway-workspace-name (car workspaces)))
   (else (focused-workspace (cdr workspaces)))))

(define (workspace-changed workspace-event)
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
    GROUPS))))

(define (pin-workspaces-to-output groups outputs)
  (for-each
   (lambda (group)
     (for-each
      (lambda (workspace output)
        (sway-switch-workspace-on-output workspace output))
      group outputs))
   groups))

(define (workspace-groups-init)
  ;; pin workspaces to output
  (pin-workspaces-to-output GROUPS OUTPUTS)
  (add-hook! sway-workspace-hook workspace-changed))
