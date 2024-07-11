;; use example:
;; TODO

(define-module (modules layout-alternating)
  #:use-module (swayipc)
  #:use-module (libs sway-tree-helper)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 pretty-print)
  #:use-module (oop goops)

  #:export (layout-alternating-init
            layout-alternating-update-layout
            layout-alternating-configure))

;; whether first split (first 2 nodes) should be
;; vertical `SWAY-LAYOUT-SPLITV` or horizontal `SWAY-LAYOUT-SPLITH`
;; the default value is vertical
(define layout-alternating-first-split SWAY-LAYOUT-SPLITV)

(define* (layout-alternating-configure)
  "Configure alternating tiling.
Parameters:
	- empty-window-command: the command that spawns an empty transparent window.")

(define (layout-alternating-init)
  "Initialize the alternating tiling module."
  (add-hook! sway-window-hook window-changed))

(define* (layout-alternating-update-layout node #:optional parent)
  (let* ((type (sway-tree-type node))
         (id (sway-tree-id node))
         (children (sway-tree-nodes node))
         (layout (sway-tree-layout node))
         (parent-layout (and parent (sway-tree-layout parent)))
         (width (sway-rect-width (sway-tree-window-rect node)))
         (height (sway-rect-height (sway-tree-window-rect node)))
         (expected-layout (cond
                           ((equal? type "workspace") layout-alternating-first-split)
                           ((equal? parent-layout SWAY-LAYOUT-SPLITV) SWAY-LAYOUT-SPLITH)
                           (else SWAY-LAYOUT-SPLITV))))
    (unless (equal? layout expected-layout)
      ;; applying split on first child so it's applied on parent
      (sway-dispatch-command
       (format #f "~a ~a"
               (sway-criteria #:con-id (sway-tree-id (car children)))
               (sway-layout expected-layout #:exec #f))))

    (when children
      (let* ((left-node (car children))
             (right-node (and (> (length children) 1) (car (cdr children))))
             (other-nodes (and (> (length children) 2) (cdr (cdr children)))))

        ;; if children are more than 2, then we have to fix the layout
        ;; all the children except the first one should be combined under
        ;; one container. if we have exactly 2 children, we still need to
        ;; ensure that the container layout is valid (vertical/horizontal)

        ;; if right-node isn't a container, create a container for it
        (when right-node
          (update-right-node right-node node))

        ;; other nodes should become children of right-node's container
        (when other-nodes
          (map (lambda (n) (sway-tree-move-node n right-node))
               other-nodes))

        ;; count of children should be less or equal 2
        ;; first child would be a single application
        ;; or a container that's either tabbed or stacked
        (when left-node
          (update-left-node left-node node))

        ;; continue updating layout for the new right node
        (let ((stree (sway-tree-node-find (sway-tree-id node))))
          (map (lambda (n)
                 (unless (null? (sway-tree-nodes n))
                   (layout-alternating-update-layout n node)))
               (sway-tree-nodes stree)))))))

(define (update-left-node node parent)
  ;; container layout should be stacked or tabbed
  ;; or the container should be an application
  (unless (or (member (sway-tree-layout node) '("stacked" "tabbed"))
              (sway-tree-app-id node))
    ;; if that's not the case, we should fix the layout by removing
    ;; all sub containers and keeping only first application
    ;; the other application containers should move to the parent
    ;; (format #t "getting children of ~a\n" (sway-tree-id node))
    (let* ((children (sway-tree-app-nodes node))
           (target-node (last (sway-tree-nodes parent))))
      ;; skip first child, we would like to keep it as is
      ;; (sway-tree-move-node (car children) node)
      ;; (format #t "got children of ~a\n" children)

      ;; the other children should become now siblings of parent
      ;; they will be fixed later, but an app layout should only have one
      ;; application if it's not stacked or tabbed
      (map (lambda (n) (sway-tree-move-node n target-node))
            (cdr children)))))

(define (update-right-node node parent)
  (format #t "updating right node container ~a\n" (sway-tree-id node))
  ;; container layout should be vertical or horizontal
  ;; the container shouldn't be an application
  (unless (member (sway-tree-layout node) `(,SWAY-LAYOUT-SPLITH ,SWAY-LAYOUT-SPLITV))
          (format #t "right node ~a is not a valid container, dispatching ..\n" (sway-tree-id node))
          (sway-dispatch-command
           (format #f "~a ~a"
                   (sway-criteria #:con-id (sway-tree-id node))
                   (sway-split-container (if (equal? (sway-tree-layout parent)
                                                     SWAY-LAYOUT-SPLITV)
                                             SWAY-SPLIT-HORIZONTAL
                                             SWAY-SPLIT-VERTICAL) #:exec #f)))))

(define (window-changed window-event)
  "Triggered when window has changed. This function processes the tree
to apply alternating tiling like experience."
  (let* ((tree (sway-get-tree)))
    ;; (display "window changed\n")

    ;; remove nested containers in the workspace
    (sway-tree-nodes-optimize
     (sway-tree-node-workspace (sway-tree-node-focused)))

    ;; update layout of current workspace only
    ;; the tree of the current workspace is passed for the update-layout
    ;; function so it gets processed and updated based on the layout definition
    (layout-alternating-update-layout (sway-tree-node-workspace (sway-tree-node-focused)))))
