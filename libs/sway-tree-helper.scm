;; This is a helper library that provides functions to simplify
;; dealing with a sway tree record

(define-module (libs sway-tree-helper)
  #:use-module (swayipc)

  #:export (sway-tree-node-parent
            sway-tree-node-workspace
            sway-tree-node-focused
            sway-tree-node-find
            sway-tree-remove-container
            sway-tree-nodes-optimize
            sway-tree-move-node
            sway-tree-nodes-flat
            sway-tree-nodes-apps))

(define (list-or lst)
  "Return first non false item from a list LST"
  (cond
    ((null? lst) #f)
    ((car lst) (car lst))
    (else (list-or (cdr lst)))))

(define* (sway-tree-node-find id #:optional (tree (sway-get-tree)))
  "Find the node of a given ID, a sway TREE can be
optionally passed, default value is sway's current tree"
  (define* (sway-tree-node-find-loop stree #:optional parent)
    (cond
     ((null? stree) #f)
     ((equal? (sway-tree-id stree) id) stree)
     ((null? (sway-tree-nodes stree)) #f)
     (else (list-or (map (lambda (n) (sway-tree-node-find-loop n stree))
                         (sway-tree-nodes stree))))))

  (sway-tree-node-find-loop tree))

(define* (sway-tree-node-parent node #:optional (tree (sway-get-tree)))
  "Find the parent of a given NODE, a sway TREE can be
optionally passed, default value is sway's current tree"
  (define* (sway-tree-node-parent-loop stree #:optional parent)
    (cond
     ((null? stree) #f)
     ((equal? (sway-tree-id stree) (sway-tree-id node)) parent)
     ((null? (sway-tree-nodes stree)) #f)
     (else (list-or (map (lambda (n) (sway-tree-node-parent-loop n stree))
                         (sway-tree-nodes stree))))))

  (sway-tree-node-parent-loop tree))

(define* (sway-tree-node-workspace node #:optional (tree (sway-get-tree)))
  "Find the parent workspace of a given NODE, a sway TREE can be
optionally passed, default value is sway current tree"
  (define* (sway-tree-node-workspace-loop child)
    (let ((parent (sway-tree-node-parent child tree)))
      (cond
       ((null? parent) #f)
       ((equal? (sway-tree-type parent) "workspace") parent)
       (else (sway-tree-node-workspace-loop parent)))))

  (sway-tree-node-workspace-loop node))

(define* (sway-tree-node-focused #:optional (tree (sway-get-tree)))
  "Find the currently focused container in a TREE, the TREE can be
optionally passed, default value is sway current tree"
  (define* (sway-tree-node-focused-loop node)
    (cond
     ((sway-tree-focused node) node)
     ((null? (sway-tree-nodes node)) #f)
     (else (list-or (map (lambda (n) (sway-tree-node-focused-loop n))
                         (sway-tree-nodes node))))))

  (sway-tree-node-focused-loop tree))

(define (sway-tree-remove-container node)
  "Remove the container NODE while keeping its children
The children will move to the parent of NODE"
  (for-each (lambda (n) (sway-move-container-to))
            (sway-tree-nodes node)))

(define (sway-tree-remove-children-layouts node)
  "Remove the children layouts from NODE while keeping its apps
The children will move to the parent of NODE"
  (for-each (lambda (n) (sway-move-container-to))
            (sway-tree-nodes node)))

(define (sway-tree-nodes-flat node)
  "Return a list of all containers (flat) under NODE"
  (cond
   ((null? (sway-tree-nodes node)) node)
   (else (cons node
               (map (lambda (n) (sway-tree-nodes-flat node))
                    (sway-tree-nodes node))))))

(define (sway-tree-nodes-apps node)
  "Return a list of all apps under NODE"
  (filter (lambda (n) (sway-tree-app-id node))
          (sway-tree-nodes-flat node)))

(define (sway-tree-move-node node new-sibling-node)
  "Move the NODE to the NEW-PARENT-NODE"
  (let ((mark (format #f "temp-~a" (sway-tree-id new-sibling-node))))
    ;; set a mark on the new sibling
    (sway-dispatch-command
        (format #f "~a ~a"
                (sway-criteria #:con-id (sway-tree-id new-sibling-node))
                (sway-mark mark #:exec #f)))

    ;; move the node to the sibling mark
    (sway-dispatch-command
        (format #f "~a ~a"
                (sway-criteria #:con-id (sway-tree-id node))
                (sway-move-container-to-mark mark #:exec #f)))))

(define (sway-tree-nodes-optimize node)
  "Remove any vertical/horizontal container that has only one child.
This function is necessary for strict some auto tiling layouts
It shouldn't be used for manual tiling."
  (let ((children (sway-tree-nodes node)))
    (cond
     ;; no children: nothing to do
     ((null? children) #t)

     ;; has exactly one child and vertical or horizontal layout
     ;; and the child also has vertical or horizontal layout
     ((and (= 1 (length children))
           (equal? (sway-tree-type node) "con")
           (member (sway-tree-layout node) `(,SWAY-LAYOUT-SPLITH ,SWAY-LAYOUT-SPLITV)))

      (sway-dispatch-command
        (format #f "~a ~a"
                (sway-criteria #:con-id (sway-tree-id (car children)))
                (sway-split-container SWAY-SPLIT-NONE #:exec #f)))

      ;; continue checking the child
      (sway-tree-nodes-optimize (car children)))

     ;; otherwise, continue checking children
     (else (map (lambda (n) (sway-tree-nodes-optimize n))
                (sway-tree-nodes node))))))
