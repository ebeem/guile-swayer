;; use example:
;; TODO

(define-module (guile-swayer modules layout-manual)
  #:use-module (guile-swayer swayipc)
  #:use-module (ice-9 pretty-print)
  #:use-module (oop goops)

  #:export (layout-manual-init
            layout-manual-configure
            layout-manual-empty-window-command))

(define layout-manual-empty-window-command "~/dotfiles/.config/sway/empty-window.py")
(define layout-manual-empty-window-id "empty-window.py")

(define (process-tree tree)
  ;; if only one node is available and orientation is either horizontal
  ;; or vertical, then an empty window should be created 
  (when tree
    (when (and (member (sway-tree-orientation tree) (list "horizontal" "vertical"))
               (equal? (sway-tree-type tree) "con")
               (equal? (length (sway-tree-nodes tree)) 1))
      (sway-focus-container-criteria (sway-criteria #:con-id (sway-tree-id tree)))
      ;; (sway-exec (string-append layout-manual-empty-window-command))
      (format #t "creating an empty window in container ~a\n" (sway-tree-id tree)))

    ;; if more than one node exist in the tree and one of them is empty
    ;; window, then delete it
    (when (and (> (length (sway-tree-nodes tree)) 1)
              (equal? (sway-tree-type tree) "con")
              (member layout-manual-empty-window-id
                    (map (lambda (node) (sway-tree-app-id node))
                          (sway-tree-nodes tree))))
      ;; TODO
      (for-each
       (lambda (n) (system (format #f "kill -9 ~a > /dev/null 2>&1" (sway-tree-pid n))))
       (filter (lambda (n) (equal? (sway-tree-app-id n) layout-manual-empty-window-id))
              (sway-tree-nodes tree)))
      (display "kill app-id from current tree\n"))

  (map process-tree (sway-tree-nodes tree))))


(define* (layout-manual-configure #:key empty-window-command)
  "Configure manual tiling.
Parameters:
	- empty-window-command: the command that spawns an empty transparent window."
  (when empty-window-command (set! layout-manual-empty-window-command empty-window-command)))

(define (window-changed window-event)
  "Triggered when window has changed. This function processes the tree
to apply manual tiling like experience."
  (let* ((tree (sway-get-tree)))
    (display "window changed\n")
    ;; (pretty-print tree)
    (process-tree tree)))

(define (layout-manual-init)
  "Initialize the manual tiling module."
  (add-hook! sway-window-hook window-changed))
