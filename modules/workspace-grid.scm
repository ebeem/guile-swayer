;; use example:

;; (set! WORKSPACES
;; (("10" "11" "12" "13" "14" "15" "16" "17" "18" "19")
;;  ("20" "21" "22" "23" "24" "25" "26" "27" "28" "29")
;;  ("30" "31" "32" "33" "34" "35" "36" "37" "38" "39"))

;; (set! ROWS 3)
;; (set! COLUMNS 3)

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
;; example:
;; (set! WORKSPACES '(("ws-o1-1" "ws-o1-2" "ws-o1-3") ("ws-o2-1" "ws-o2-2" "ws-o2-3"))
(define WORKSPACES '())

;; number of rows in the grid
(define ROWS 1)
;; number of columns in the grid
(define COLUMNS 1)

(define* (workspace-grid-configure #:key rows columns workspaces)
  (when rows (set! ROWS rows))
  (when columns (set! COLUMNS columns))
  (when workspaces (set! WORKSPACES workspaces)))

(define* (get-active-workspace-name #:optional (workspaces (sway-get-workspaces)))
  "get name of active workspace"
  (cond
   ((null? workspaces) #f)
   ((equal? (sway-workspace-focused (car workspaces)) #t)
    (sway-workspace-name (car workspaces)))
   (else (get-active-workspace-name (cdr workspaces)))))

(define* (get-output-index workspace #:optional (workspaces WORKSPACES) (index 0))
  "get output index of target workspace"
  (cond
   ((null? workspaces) #f)
   ((member workspace (car workspaces)) index)
   (else (get-output-index workspace (cdr workspaces) (+ index 1)))))

(define* (get-workspace-index workspace #:optional
                             (workspaces
                              (list-ref WORKSPACES (get-output-index workspace))))
  "get index of target workspace"
  (let* ((memberls (member workspace workspaces)))
    (if memberls (- (length workspaces) (length memberls)))))

(define (get-active-workspace-index)
  "get index of active/focused workspace"
  (let* ((workspace (get-active-workspace-name)))
    (get-workspace-index workspace)))

;; available directions, up, right, down, left
(define* (get-workspace-direction direction #:optional (index -1))
  "get the index the next workspace after applying the direction"
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
  (list-ref (list-ref WORKSPACES output) workspace))

;; exposed command for easier access
(define (switch-workspace-up)
  (sway-switch-workspace
   (get-workspace-name
    (get-workspace-direction "up"))))

(define (switch-workspace-right)
  (sway-switch-workspace
   (get-workspace-name
    (get-workspace-direction "right"))))

(define (switch-workspace-down)
  (sway-switch-workspace
   (get-workspace-name
    (get-workspace-direction "down"))))

(define (switch-workspace-left)
  (sway-switch-workspace
   (get-workspace-name
    (get-workspace-direction "left"))))

(define (move-container-to-workspace-up)
  (sway-move-container-to-workspace
   (get-workspace-name
    (get-workspace-direction "up")))
  (switch-workspace-up))

(define (move-container-to-workspace-right)
  (sway-move-container-to-workspace
   (get-workspace-name
    (get-workspace-direction "right")))
  (switch-workspace-right))

(define (move-container-to-workspace-down)
  (sway-move-container-to-workspace
   (get-workspace-name
    (get-workspace-direction "down")))
  (switch-workspace-down))

(define (move-container-to-workspace-left)
  (sway-move-container-to-workspace
   (get-workspace-name
    (get-workspace-direction "left")))
  (switch-workspace-left))

(define (valid-grid? rows columns workspaces)
  "validate the grid structure"
  (and (> (length workspaces) 0)
       (equal? (* rows columns) (length (car workspaces)))))

(define (workspace-grid-init)
  (display "starting workspace-grid\n")
  (display WORKSPACES)
  (newline)
  (if (valid-grid? ROWS COLUMNS WORKSPACES)
    (display (string-append "successfully started workspace "
                            (number->string ROWS) "x"
                            (number->string COLUMNS) "\n"))
    (display (string-append "workspace grid failed to start the grid configs "
                            (number->string ROWS) "x"
                            (number->string COLUMNS) "\n"))))
