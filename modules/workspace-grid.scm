;; use example:

;; (workspace-grid-configure #:rows 2 #:columns
;;                           '(("ws-o1-1" "ws-o1-2" "ws-o1-3" "ws-o1-3")
;;                             ("ws-o2-1" "ws-o2-2" "ws-o2-3" "ws-o2-3")
;;                             ("ws-o3-1" "ws-o3-2" "ws-o3-3" "ws-o3-3")))
;; (workspace-grid-init)

(define-module (modules workspace-grid)
  #:use-module (swayipc records)
  #:use-module (swayipc info)
  #:use-module (swayipc dispatcher)
  #:use-module (swayipc events)

  #:export (WORKSPACES
            COLUMNS
            ROWS
            workspace-grid-configure
            get-active-workspace-index
            switch-workspace-up
            switch-workspace-right
            switch-workspace-down
            switch-workspace-left
            move-container-to-workspace-up
            move-container-to-workspace-right
            move-container-to-workspace-down
            move-container-to-workspace-left
            valid-grid?
            workspace-grid-init))

;; The order in which the outputs are organized, it's important that
;; the order of outputs match the order of workspaces in `WORKSPACE`
(define WORKSPACES '())

;; number of rows in the grid
(define ROWS 1)
;; number of columns in the grid
(define COLUMNS 1)

(define* (workspace-grid-configure #:key rows columns workspaces)
  "Configure workspace grid.
Parameters:
	- rows: number of rows in the grid.
	- columns: number of columns in the grid
	- workspaces: list of list of workspaces. should match the amount of outputs.

Example: configuring a 2x2 workspace grid for 3 monitors.
This means 3x2x2= 12 workspaces should be provided.

(workspace-grid-configure #:rows 2 #:columns
                          '((\"ws-o1-1\" \"ws-o1-2\" \"ws-o1-3\" \"ws-o1-3\")
                            (\"ws-o2-1\" \"ws-o2-2\" \"ws-o2-3\" \"ws-o2-3\")
                            (\"ws-o3-1\" \"ws-o3-2\" \"ws-o3-3\" \"ws-o3-3\")))"
  (when rows (set! ROWS rows))
  (when columns (set! COLUMNS columns))
  (when workspaces (set! WORKSPACES workspaces)))

(define* (get-active-workspace-name #:optional (workspaces (sway-get-workspaces)))
  "Return name of active workspace."
  (cond
   ((null? workspaces) #f)
   ((equal? (sway-workspace-focused (car workspaces)) #t)
    (sway-workspace-name (car workspaces)))
   (else (get-active-workspace-name (cdr workspaces)))))

(define* (get-output-index workspace-name #:optional (workspaces WORKSPACES) (index 0))
  "Return output index of target workspace name (workspace-name)"
  (cond
   ((null? workspaces) #f)
   ((member workspace-name (car workspaces)) index)
   (else (get-output-index workspace-name (cdr workspaces) (+ index 1)))))

(define* (get-workspace-index workspace-name #:optional
                             (workspaces
                              (list-ref WORKSPACES (get-output-index workspace-name))))
  "Return index of target workspace name (workspace-name)."
  (let* ((memberls (member workspace-name workspaces)))
    (if memberls (- (length workspaces) (length memberls)))))

(define (get-active-workspace-index)
  "Return index of active/focused workspace."
  (let* ((workspace (get-active-workspace-name)))
    (get-workspace-index workspace)))

;; available directions, up, right, down, left
(define* (get-workspace-direction direction #:optional (index -1))
  "Return the index the target workspace after applying the given direction.
Parameters:
	- direction: can be one of \"up\", \"right\", \"down\", \"left\".
	- index: the index of the workspace to get the direction from (current by default)."
  (let* ((index (if (< index 0) (get-active-workspace-index) index))
         (current-row (floor (/ index COLUMNS)))
         (current-column (modulo index COLUMNS))
         (target-row
          (cond ((equal? direction "up") (- current-row 1))
                ((equal? direction "down") (+ current-row 1))
                (else current-row)))
         (target-column
          (cond ((equal? direction "left") (- current-column 1))
                ((equal? direction "right") (+ current-column 1))
                (else current-column))))
    (+ (* COLUMNS (modulo target-row ROWS))
       (modulo target-column COLUMNS))))

(define* (get-workspace-name  #:optional
                              (workspace (get-active-workspace-index))
                              (output (get-output-index (get-active-workspace-name))))
  "Get workspace name from a given workspace index.
Parameters:
	- workspace: workspace index as in configuraiton (by default, current active workspace index).
	- output: output index as in configuraiton (by default, current active output index).

Note: returned name is based on configured variable WORKSPACES."
  (list-ref (list-ref WORKSPACES output) workspace))

;; exposed command for easier access
(define (switch-workspace-up)
  "Focus workspace up in grid."
  (sway-switch-workspace
   (get-workspace-name
    (get-workspace-direction "up"))))

(define (switch-workspace-right)
  "Focus workspace right in grid."
  (sway-switch-workspace
   (get-workspace-name
    (get-workspace-direction "right"))))

(define (switch-workspace-down)
  "Focus workspace down in grid."
  (sway-switch-workspace
   (get-workspace-name
    (get-workspace-direction "down"))))

(define (switch-workspace-left)
  "Focus workspace left in grid."
  (sway-switch-workspace
   (get-workspace-name
    (get-workspace-direction "left"))))

(define (move-container-to-workspace-up)
  "Move current container to workspace up in grid and focus it."
  (sway-move-container-to-workspace
   (get-workspace-name
    (get-workspace-direction "up")))
  (switch-workspace-up))

(define (move-container-to-workspace-right)
  "Move current container to workspace right in grid and focus it."
  (sway-move-container-to-workspace
   (get-workspace-name
    (get-workspace-direction "right")))
  (switch-workspace-right))

(define (move-container-to-workspace-down)
  "Move current container to workspace down in grid and focus it."
  (sway-move-container-to-workspace
   (get-workspace-name
    (get-workspace-direction "down")))
  (switch-workspace-down))

(define (move-container-to-workspace-left)
  "Move current container to workspace left in grid and focus it."
  (sway-move-container-to-workspace
   (get-workspace-name
    (get-workspace-direction "left")))
  (switch-workspace-left))

(define (valid-grid? rows columns workspaces)
  "Validate the grid structure by ensuring the number of workspaces
matches the number of rowsxcolumns."
  (and (> (length workspaces) 0)
       (equal? (* rows columns) (length (car workspaces)))))

(define (workspace-grid-init)
  "Initialize the workspace grid."
  (format #t "starting workspace-grid\n~a\n" WORKSPACES)
  (if (valid-grid? ROWS COLUMNS WORKSPACES)
      (format #t "successfully started workspace ~ax~a\n" ROWS COLUMNS)
      (format #t "workspace grid failed to start the grid configs ~ax~a\n" ROWS COLUMNS)))
