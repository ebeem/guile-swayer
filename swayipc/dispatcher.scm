(define-module (swayipc dispatcher)
  #:use-module (swayipc connection)
  #:use-module (swayipc records)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (sjson)

  #:export (SWAY-ORIENTATION-HORIZONTAL
            SWAY-ORIENTATION-VERTICAL
            SWAY-ORIENTATION-AUTO
            SWAY-LAYOUT-DEFAULT
            SWAY-LAYOUT-STACKING
            SWAY-LAYOUT-TABBED
            SWAY-XWAYLAND-ENABLE
            SWAY-XWAYLAND-DISABLE
            SWAY-XWAYLAND-FORCE
            SWAY-BORDER-NONE
            SWAY-BORDER-NORMAL
            SWAY-BORDER-CSD
            SWAY-BORDER-PIXEL
            SWAY-FLOATING-ENABLED
            SWAY-FLOATING-DISABLED
            SWAY-FLOATING-TOGGLE
            SWAY-DIRECTION-UP
            SWAY-DIRECTION-RIGHT
            SWAY-DIRECTION-DOWN
            SWAY-DIRECTION-LEFT
            SWAY-SIBLING-NEXT
            SWAY-SIBLING-PREV
            SWAY-HIERARCHY-CHILD
            SWAY-HIERARCHY-PARENT
            SWAY-FULLSCREEN-ENABLED
            SWAY-FULLSCREEN-DISABLED
            SWAY-FULLSCREEN-TOGGLE
            SWAY-GAPS-OPTION-INNER
            SWAY-GAPS-OPTION-OUTER
            SWAY-GAPS-OPTION-HORIZONTAL
            SWAY-GAPS-OPTION-VERTICAL
            SWAY-GAPS-OPTION-TOP
            SWAY-GAPS-OPTION-RIGHT
            SWAY-GAPS-OPTION-BOTTOM
            SWAY-GAPS-OPTION-LEFT
            SWAY-GAPS-WORKSPACE-ALL
            SWAY-GAPS-WORKSPACE-CURRENT
            SWAY-GAPS-TYPE-SET
            SWAY-GAPS-TYPE-PLUS
            SWAY-GAPS-TYPE-MINUS
            SWAY-GAPS-TYPE-TOGGLE
            SWAY-MOUSE-WARPING-OUTPUT
            SWAY-MOUSE-WARPING-CONTAINER
            SWAY-MOUSE-WARPING-NONE
            SWAY-POPUP-TYPE-OUTPUTSMART
            SWAY-POPUP-TYPE-IGNORE
            SWAY-POPUP-TYPE-LEAVE-FULLSCREEN
            SWAY-PRIMARY-SELECTION-ENABLED
            SWAY-PRIMARY-SELECTION-DISABLED
            SWAY-SHOW-MARKS-YES
            SWAY-SHOW-MARKS-NO
            SWAY-OPACITY-SET
            SWAY-OPACITY-PLUS
            SWAY-OPACITY-MINUS
            SWAY-TILING-DRAG-ENABLE
            SWAY-TILING-DRAG-DISABLE
            SWAY-TILING-DRAG-TOGGLE
            SWAY-TILING-ALIGN-LEFT
            SWAY-TILING-ALIGN-CENTER
            SWAY-TILING-ALIGN-RIGHT
            SWAY-WORKSPACE-AUTO-BACK-AND-FORTH-OPTION-YES
            SWAY-WORKSPACE-AUTO-BACK-AND-FORTH-OPTION-NO
            SWAY-FLOATING-MODIFIER-TYPE-NORMAL
            SWAY-FLOATING-MODIFIER-TYPE-INVERSE
            SWAY-FOCUS-FOLLOW-MOUSE-FLAG-YES
            SWAY-FOCUS-FOLLOW-MOUSE-FLAG-NO
            SWAY-FOCUS-FOLLOW-MOUSE-FLAG-ALWAYS
            SWAY-FOCUS-ON-WINDOW-ACTIVATION-FLAG-SMART
            SWAY-FOCUS-ON-WINDOW-ACTIVATION-FLAG-URGENT
            SWAY-FOCUS-ON-WINDOW-ACTIVATION-FLAG-FOCUS
            SWAY-FOCUS-ON-WINDOW-ACTIVATION-FLAG-NONE
            SWAY-FOCUS-WRAPPING-FLAG-YES
            SWAY-FOCUS-WRAPPING-FLAG-NO
            SWAY-FOCUS-WRAPPING-FLAG-FORCE
            SWAY-FOCUS-WRAPPING-FLAG-WORKSPACE
            SWAY-EDGE-BORDER-TYPE-NONE
            SWAY-EDGE-BORDER-TYPE-VERTICAL
            SWAY-EDGE-BORDER-TYPE-HORIZONTAL
            SWAY-EDGE-BORDER-TYPE-BOTH
            SWAY-EDGE-BORDER-TYPE-SMART
            SWAY-EDGE-BORDER-TYPE-SMART-NO-GAPS
            SWAY-SMART-BORDERS-ON
            SWAY-SMART-BORDERS-OFF
            SWAY-SMART-BORDERS-NO-GAPS
            SWAY-SMART-GAPS-ON
            SWAY-SMART-GAPS-OFF
            SWAY-SMART-GAPS-TOGGLE
            SWAY-SMART-GAPS-INVERSE-OUTER
            SWAY-RESIZE-TYPE-SHRINK
            SWAY-RESIZE-TYPE-GROW
            SWAY-RESIZE-DIRECTION-HEIGHT
            SWAY-RESIZE-DIRECTION-WIDTH
            SWAY-SIZE-UNIT-PX
            SWAY-SIZE-UNIT-PPT
            SWAY-SPLIT-VERTICAL
            SWAY-SPLIT-HORIZONTAL
            SWAY-SPLIT-NONE
            SWAY-SPLIT-TOGGLE
            SWAY-STICKY-ENABLE
            SWAY-STICKY-DISABLE
            SWAY-STICKY-TOGGLE
            SWAY-SWAY-CONTAINER-TYPE-ID
            SWAY-SWAY-CONTAINER-TYPE-CONTAINER-ID
            SWAY-SWAY-CONTAINER-TYPE-MARK
            SWAY-URGENT-ENABLE
            SWAY-URGENT-DISABLE
            SWAY-URGENT-ALLOW
            SWAY-URGENT-DENY
            SWAY-BORDER-STYLE-NONE
            SWAY-BORDER-STYLE-NORMAL
            SWAY-BORDER-STYLE-PIXEL
            SWAY-INHIBIT-IDLE-FOCUS
            SWAY-INHIBIT-IDLE-FULLSCREEN
            SWAY-INHIBIT-IDLE-OPEN
            SWAY-INHIBIT-IDLE-NONE
            SWAY-INHIBIT-IDLE-VISIBLE
            SWAY-LAYOUT-SPLITH
            SWAY-LAYOUT-SPLITV
            SWAY-LAYOUT-TOGGLE-ALL
            SWAY-LAYOUT-TOGGLE-SPLIT
            SWAY-WORKSPACE-PREVIOUS
            SWAY-WORKSPACE-NEXT
            SWAY-WORKSPACE-CURRENT
            SWAY-WORKSPACE-PREVIOUS-ON-OUTPUT
            SWAY-WORKSPACE-NEXT-ON-OUTPUT
            SWAY-WORKSPACE-BACK-AND-FORTH
            SWAY-OUTPUT-CURRENT
            SWAY-OUTPUT-UP
            SWAY-OUTPUT-RIGHT
            SWAY-OUTPUT-DOWN
            SWAY-OUTPUT-LEFT

            sway-dispatch-command
            sway-dispatch-commands
            sway-default-orientation
            sway-include
            sway-swaybg-command
            sway-swaynag-command
            sway-workspace-layout
            sway-xwayland
            sway-border
            sway-border-toggle
            sway-exit
            sway-floating
            sway-mark
            sway-focus-container-criteria
            sway-focus-container
            sway-focus-container-sibling
            sway-focus-container-child
            sway-focus-container-parent
            sway-focus-output-direction
            sway-focus-output-name
            sway-focus-container-tiling
            sway-focus-container-floating
            sway-fullscreen
            sway-gaps
            sway-inhibit-idle
            sway-layout
            sway-layout-toggle
            sway-move-container
            sway-move-container-absolute-position
            sway-move-container-absolute-center
            sway-move-container-cursor
            sway-move-container-to-mark
            sway-move-container-to-workspace
            sway-move-container-to-output
            sway-move-container-to-scratchpad
            sway-move-workspace-to-output
            sway-nop
            sway-reload
            sway-rename-workspace
            sway-rename-current-workspace
            sway-resize
            sway-resize-height
            sway-resize-width
            sway-show-scratchpad
            sway-shortcuts-inhibitor
            sway-split-container
            sway-sticky
            sway-swap-container
            sway-title-format
            sway-assign-to-workspace
            sway-assign-to-output
            sway-bindsym
            sway-bindcode
            sway-bindswitch
            sway-unbindswitch
            sway-unbindsym
            sway-unbindcode
            sway-unmark
            sway-urgent
            sway-client-background
            sway-client-focused-color
            sway-client-focused-inactive-color
            sway-client-focused-tab-title-color
            sway-client-placeholder-color
            sway-client-unfocused-color
            sway-client-urgent-color
            sway-default-border-style
            sway-default-floating-border-style
            sway-exec
            sway-exec-always
            sway-floating-maximum-size
            sway-floating-minimum-size
            sway-floating-modifier
            sway-focus-follow-mouse
            sway-focus-on-window-activation
            sway-focus-wrapping
            sway-font
            sway-force-display-urgency-hint
            sway-titlebar-border-thickness
            sway-titlebar-padding
            sway-for-window
            sway-default-gaps
            sway-hide-edge-borders
            sway-input
            sway-seat
            sway-kill
            sway-smart-borders
            sway-smart-gaps
            sway-mode
            sway-mode-subcommand
            sway-mouse-warping
            sway-no-focus
            sway-output
            sway-popup-during-fullscreen
            sway-primary-selection
            sway-show-marks
            sway-opacity
            sway-tiling-drag
            sway-tiling-drag-threshold
            sway-tiling-align
            sway-switch-workspace-id
            sway-switch-workspace
            sway-switch-workspace-on-output
            sway-workspace-auto-back-and-forth
            sway-workspace-gaps
            sway-criteria))

(define (custom-exception-handler exc)
  (display "An error occurred while dispatching the command\n"))

(define (catch-all thunk)
  (with-exception-handler
    (lambda (exn)
      (format (current-error-port)
              "Uncaught exception: ~s\n" exn)
      #f)
    thunk
    #:unwind? #t))

(define (sway-dispatch-command command)
  "Parses and runs the payload as sway command.
Parameters:
	- a sway command
Response:
    An  array of objects corresponding to each command that was parsed. Each
    object has the property success."
  (sway-dispatch-commands command))

(define (sway-dispatch-commands . commands)
  "Parses and runs the payload as sway commands
Parameters:
	- list of sway commands
Response:
    An  array of objects corresponding to each command that was parsed. Each
    object has the property success."
  (format #t "dispatching: ~a\n" (string-join commands "\n"))

  (catch-all
   (lambda ()
      (begin
        ;; write the commands message
        (sway-write-msg SWAY-COMMAND-SOCKET SWAY-MSG-ID-RUN-COMMMAND (string-join commands " "))
        ;; read response from socket
        (map
         (lambda (res)
           (scm->sway-tick res))
         (vector->list
          (json-string->scm
           (list-ref (sway-read-msg SWAY-COMMAND-SOCKET) 1))))))))

;;        bar [<bar-id>] <bar-subcommands...>
;;            For details on bar subcommands, see sway-bar(5).

(define SWAY-ORIENTATION-HORIZONTAL "horizontal")
(define SWAY-ORIENTATION-VERTICAL "vertical")
(define SWAY-ORIENTATION-AUTO "auto")

(define* (sway-default-orientation orientation #:key (exec #t))
  "Sets the default container layout for tiled containers.
  parameters:
    - orientation: `SWAY-ORIENTATION-HORIZONTAL`, `SWAY-ORIENTATION-VERTICAL`, `SWAY-ORIENTATION-AUTO`"
  (let ((command (format #f "default_orientation ~a" orientation)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-include file-path #:key (exec #t))
  "Includes another configuration file from path (not scheme file).
  parameters:
    - file-path: string"
  (let ((command (format #f "include ~a" file-path)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-swaybg-command command #:key (exec #t))
  "Executes custom background command.  Default  is  swaybg.
  parameters:
    - command: string"
  (let ((command (format #f "swaybg_command ~a" command)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-swaynag-command command #:key (exec #t))
  "Executes custom command for swaynag. Default is swaynag.
  parameters:
    - command: string"
  (let ((command (format #f "swaynag_command ~a" command)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-LAYOUT-DEFAULT "default")
(define SWAY-LAYOUT-STACKING "stacking")
(define SWAY-LAYOUT-TABBED "tabbed")

(define* (sway-workspace-layout layout #:key (exec #t))
  "Specifies the initial layout for new containers in  an  empty  workspace.
  parameters:
    - layout: `SWAY-LAYOUT-DEFAULT`, `SWAY-LAYOUT-STACKING`, `SWAY-LAYOUT-TABBED`"
  (let ((command (format #f "workspace_layout ~a" layout)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-XWAYLAND-ENABLE "enable")
(define SWAY-XWAYLAND-DISABLE "disable")
(define SWAY-XWAYLAND-FORCE "force")

(define* (sway-xwayland option #:key (exec #t))
  "Enables or disables Xwayland support, which allows X11 applications to be used.
  parameters:
    - option: `SWAY-XWAYLAND-ENABLE`, `SWAY-XWAYLAND-DISABLE`, `SWAY-XWAYLAND-FORCE`"
  (let* ((option (cond
                  ((equal? #t option) SWAY-XWAYLAND-ENABLE)
                  ((equal? #f option) SWAY-XWAYLAND-DISABLE)
                  (else option)))
         (command (format #f "xwayland ~a" option)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-BORDER-NONE "none")
(define SWAY-BORDER-NORMAL "normal")
(define SWAY-BORDER-CSD "csd")
(define SWAY-BORDER-PIXEL "pixel")

(define* (sway-border option thickness #:key (exec #t))
  "Enables  or disables Xwayland support, which allows X11 applications to be used.
  parameters:
    - option: `SWAY-BORDER-NONE`, `SWAY-BORDER-NORMAL`, `SWAY-BORDER-CSD`, `SWAY-BORDER-PIXEL`
	- thickness: int"
  (let ((command (format #f "border ~a ~a" option thickness)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-border-toggle #:key (exec #t))
  "Cycles through the available border styles."
  (let ((command (format #f "border toggle")))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-exit #:key (exec #t))
  "Exit sway and end your Wayland session."
  (let ((command (format #f "exit")))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-FLOATING-ENABLED "enabled")
(define SWAY-FLOATING-DISABLED "disabled")
(define SWAY-FLOATING-TOGGLE "toggle")

(define* (sway-floating option #:key (exec #t))
  "Make focused view floating, non-floating, or the opposite of what it is now.
  parameters:
    - layout: `SWAY-FLOATING-ENABLED`, `SWAY-FLOATING-DISABLED`, `SWAY-FLOATING-TOGGLE`"
  (let* ((option (cond
                  ((equal? #t option) SWAY-FLOATING-ENABLED)
                  ((equal? #f option) SWAY-FLOATING-DISABLED)
                  (else option)))
         (command (format #f "floating ~a" option)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-focus-container-criteria criteria #:key (exec #t))
  "Moves focus to the container that matches the specified criteria.
  parameters:
    - criteria: sway criteria"
  (let ((command (format #f "~a focus" criteria)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-DIRECTION-UP "up")
(define SWAY-DIRECTION-RIGHT "right")
(define SWAY-DIRECTION-DOWN "down")
(define SWAY-DIRECTION-LEFT "left")
(define SWAY-SIBLING-NEXT "next")
(define SWAY-SIBLING-PREV "prev")
(define SWAY-HIERARCHY-CHILD "child")
(define SWAY-HIERARCHY-PARENT "parent")

(define* (sway-focus-container direction #:key (exec #t))
  "Moves focus to the next container in the specified direction.
  parameters:
    - direction: `SWAY-DIRECTION-UP`, `SWAY-DIRECTION-RIGHT`, `SWAY-DIRECTION-DOWN`, `SWAY-DIRECTION-LEFT`"
  (let ((command (format #f "focus ~a" direction)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-focus-container-sibling sibling #:key (exec #t))
  "Moves focus to the previous or next container in the current layout.
  parameters:
    - sibling: `SWAY-SIBLING-NEXT`, `SWAY-SIBLING-PREV`"
  (let ((command (format #f "focus ~a" sibling)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-focus-container-child #:key (exec #t))
  "Moves focus to the last-focused child of the focused container."
  (let ((command (format #f "focus child")))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-focus-container-parent #:key (exec #t))
  "Moves focus to the last-focused parent of the focused container."
  (let ((command (format #f "focus parent")))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-focus-output-direction direction #:key (exec #t))
  "Moves focus to the next output in the specified direction.
  parameters:
    - direction: `SWAY-DIRECTION-UP`, `SWAY-DIRECTION-RIGHT`, `SWAY-DIRECTION-DOWN`, `SWAY-DIRECTION-LEFT`"
  (let ((command (format #f "focus output ~a" direction)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-focus-output-name name #:key (exec #t))
  "Moves focus to the named output.
  parameters:
    - name: string, output name"
  (let ((command (format #f "focus output ~a" name)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-focus-container-tiling #:key (exec #t))
  "Sets focus to the last focused tiling container."
  (let ((command (format #f "focus tiling")))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-focus-container-floating #:key (exec #t))
  "Sets focus to the last focused floating container."
  (let ((command (format #f "focus floating")))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-FULLSCREEN-ENABLED "enabled")
(define SWAY-FULLSCREEN-DISABLED "disabled")
(define SWAY-FULLSCREEN-TOGGLE "toggle")

(define* (sway-fullscreen option #:key global (exec #t))
  "Makes focused view fullscreen, non-fullscreen, or the opposite of current.
  parameters:
    - option: `SWAY-FULLSCREEN-ENABLED`, `SWAY-FULLSCREEN-DISABLED`, `SWAY-FULLSCREEN-TOGGLE`
    - global: #t, #f"
  (let* ((option (cond
                  ((equal? #t option) SWAY-FULLSCREEN-ENABLED)
                  ((equal? #f option) SWAY-FULLSCREEN-DISABLED)
                  (else option)))
         (global (if global "global" ""))
         (command (format #f "fullscreen ~a ~a" option global)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-GAPS-OPTION-INNER "inner")
(define SWAY-GAPS-OPTION-OUTER "outer")
(define SWAY-GAPS-OPTION-HORIZONTAL "horizontal")
(define SWAY-GAPS-OPTION-VERTICAL "vertical")
(define SWAY-GAPS-OPTION-TOP "top")
(define SWAY-GAPS-OPTION-RIGHT "right")
(define SWAY-GAPS-OPTION-BOTTOM "bottom")
(define SWAY-GAPS-OPTION-LEFT "left")

(define SWAY-GAPS-WORKSPACE-ALL "all")
(define SWAY-GAPS-WORKSPACE-CURRENT "current")

(define SWAY-GAPS-TYPE-SET "set")
(define SWAY-GAPS-TYPE-PLUS "plus")
(define SWAY-GAPS-TYPE-MINUS "minus")
(define SWAY-GAPS-TYPE-TOGGLE "toggle")

(define* (sway-gaps option workspace type amount #:key (exec #t))
  "Changes the inner or outer gaps for either all workspaces or the current workspace.
  parameters:
    - option: `SWAY-GAPS-OPTION-INNER`, `SWAY-GAPS-OPTION-OUTER`, `SWAY-GAPS-OPTION-HORIZONTAL`,
              `SWAY-GAPS-OPTION-VERTICAL`, `SWAY-GAPS-OPTION-TOP`, `SWAY-GAPS-OPTION-RIGHT`,
              `SWAY-GAPS-OPTION-BOTTOM`, `SWAY-GAPS-OPTION-LEFT`
    - workspace: `SWAY-GAPS-WORKSPACE-ALL`, `SWAY-GAPS-WORKSPACE-CURRENT`
    - type: `SWAY-GAPS-TYPE-SET`, `SWAY-GAPS-TYPE-PLUS`,
			`SWAY-GAPS-TYPE-MINUS`, `SWAY-GAPS-TYPE-TOGGLE`
    - amount: amount of gap (number)"
  (let* ((command (format #f "gaps ~a ~a ~a ~a" option workspace type amount)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-INHIBIT-IDLE-FOCUS "focus")
(define SWAY-INHIBIT-IDLE-FULLSCREEN "fullscreen")
(define SWAY-INHIBIT-IDLE-OPEN "open")
(define SWAY-INHIBIT-IDLE-NONE "none")
(define SWAY-INHIBIT-IDLE-VISIBLE "visible")

(define* (sway-inhibit-idle option #:key (exec #t))
  "Set/unset an idle inhibitor for the view.
  parameters:
    - option: `SWAY-INHIBIT-IDLE-FOCUS`, `SWAY-INHIBIT-IDLE-FULLSCREEN`, `SWAY-INHIBIT-IDLE-OPEN`,
              `SWAY-INHIBIT-IDLE-NONE`, `SWAY-INHIBIT-IDLE-VISIBLE`"
  (let* ((command (format #f "inhibit_idle ~a" option)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-LAYOUT-SPLITH "splith")
(define SWAY-LAYOUT-SPLITV "splitv")

(define* (sway-layout option #:key (exec #t))
  "Set/unset an idle inhibitor for the view.
  parameters:
    - option: `SWAY-LAYOUT-DEFAULT`, `SWAY-LAYOUT-SPLITH`, `SWAY-LAYOUT-SPLITV`,
              `SWAY-LAYOUT-STACKING`, `SWAY-LAYOUT-TABBED`"
  (let* ((command (format #f "layout ~a" option)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-LAYOUT-TOGGLE-ALL "all")
(define SWAY-LAYOUT-TOGGLE-SPLIT "split")

(define* (sway-layout-toggle #:key option (exec #t))
  "Cycles the layout mode of the focused container though a preset list of layouts.
  parameters:
    - option: `SWAY-LAYOUT-TOGGLE-ALL`, `SWAY-LAYOUT-TOGGLE-SPLIT`"
  (let* ((option (if option (format #f " ~a" option) ""))
         (command (format #f "layout toggle~a" option)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-move-container direction #:key amount (exec #t))
  "Moves the focused container in the direction specified.
  parameters:
    - direction: `SWAY-DIRECTION-UP`, `SWAY-DIRECTION-RIGHT`, `SWAY-DIRECTION-DOWN`, `SWAY-DIRECTION-LEFT`
    - amount: int"
  (let* ((amount (or amount ""))
         (command (format #f "move ~a ~a" direction amount)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-move-container-absolute-position x y #:key (exec #t))
  "Moves the focused container to the specified position in the workspace.
  parameters:
    - x: int
    - y: int"
  (let* ((command (format #f "move absolute position ~a ~a" x y)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-move-container-absolute-center #:key (exec #t))
  "Moves the focused container to be centered on the workspace."
  (let* ((command (format #f "move absolute position center")))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-move-container-cursor #:key (exec #t))
  "Moves the focused container to be centered on the cursor."
  (let* ((command (format #f "move position cursor")))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-move-container-to-mark mark #:key (exec #t))
  "Moves the focused container to the specified mark."
  (let* ((command (format #f "move container to mark ~a" mark)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-WORKSPACE-PREVIOUS "prev")
(define SWAY-WORKSPACE-NEXT "next")
(define SWAY-WORKSPACE-CURRENT "current")
(define SWAY-WORKSPACE-PREVIOUS-ON-OUTPUT "prev_on_output")
(define SWAY-WORKSPACE-NEXT-ON-OUTPUT "next_on_output")
(define SWAY-WORKSPACE-BACK-AND-FORTH "back_and_forth")

(define* (sway-move-container-to-workspace workspace #:key (exec #t))
  "Moves the focused container to the workspace name
  parameters:
    - workspace: workspace name, `SWAY-WORKSPACE-PREVIOUS`, `SWAY-WORKSPACE-NEXT`, `SWAY-WORKSPACE-CURRENT`,
                 `SWAY-WORKSPACE-PREVIOUS-ON-OUTPUT`, `SWAY-WORKSPACE-NEXT-ON-OUTPUT`, `SWAY-WORKSPACE-BACK-AND-FORTH`"
  (let* ((command (format #f "move container to workspace \"~a\"" workspace)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-OUTPUT-CURRENT "current")
(define SWAY-OUTPUT-UP "up")
(define SWAY-OUTPUT-RIGHT "right")
(define SWAY-OUTPUT-DOWN "down")
(define SWAY-OUTPUT-LEFT "left")

(define* (sway-move-container-to-output output #:key (exec #t))
  "Moves the focused container to the specified output id|name|direction.
  parameters:
    - workspace: output name, output id, `SWAY-OUTPUT-CURRENT`, `SWAY-OUTPUT-UP`,
                 `SWAY-OUTPUT-RIGHT`, `SWAY-OUTPUT-DOWN`, `SWAY-OUTPUT-LEFT`"
  (let* ((command (format #f "move container to output \"~a\"" output)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-move-container-to-scratchpad #:key (exec #t))
  "Moves the focused container to the scratchpad."
  (let* ((command (format #f "move container to scratchpad")))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-move-workspace-to-output output #:key (exec #t))
  "Moves the focused workspace to the specified output id|name|direction.
  parameters:
    - workspace: output name, output id, `SWAY-OUTPUT-CURRENT`, `SWAY-OUTPUT-UP`,
                 `SWAY-OUTPUT-RIGHT`, `SWAY-OUTPUT-DOWN`, `SWAY-OUTPUT-LEFT`"
  (let* ((command (format #f "move workspace to output ~a" output)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-nop #:key (comment "") (exec #t))
  "A no operation command that can be used to override default behaviour.
  parameters:
    - comment: optional comment argument is ignored, but logged for debugging purposes."
  (let* ((command (format #f "nop ~a" comment)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-reload #:key (exec #t))
  "Reloads the sway config file and applies any changes."
  (let* ((command (format #f "reload")))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-rename-workspace old-name new-name #:key (exec #t))
  "Rename workspace <old_name> to the <new_name>
  parameters:
    - old-name: old workspace name (str).
    - new-name: new workspace name (str)."
  (let* ((command (format #f "rename workspace ~a to ~a" old-name new-name)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-rename-current-workspace new-name #:key (exec #t))
  "Rename current workspace to the <new_name>
  parameters:
    - new-name: new workspace name (str)."
  (let* ((command (format #f "rename workspace to ~a" new-name)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-RESIZE-TYPE-SHRINK "shrink")
(define SWAY-RESIZE-TYPE-GROW "grow")
(define SWAY-RESIZE-DIRECTION-HEIGHT "height")
(define SWAY-RESIZE-DIRECTION-WIDTH "width")

(define SWAY-SIZE-UNIT-PX "px")
(define SWAY-SIZE-UNIT-PPT "ppt")

(define* (sway-resize type direction amount #:key unit (exec #t))
  "Resizes the currently focused container by amount, specified in pixels or percentage points.
If the units are omitted, floating containers are resized in px and tiled containers by ppt.
  parameters:
    - type: `SWAY-RESIZE-TYPE-SHRINK`, `SWAY-RESIZE-TYPE-GROW`
    - direction: `SWAY-RESIZE-DIRECTION-HEIGHT`, `SWAY-RESIZE-DIRECTION-WIDTH`
    - amount: number
    - unit: `SWAY-SIZE-UNIT-PX`, `SWAY-SIZE-UNIT-PPT`"
  (let* ((unit (or unit ""))
         (command (format #f "resize ~a ~a ~a ~a" type direction amount unit)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-resize-height amount #:key unit (exec #t))
  "Sets the height of the container to height, specified in pixels or percentage points."
  (let* ((unit (or unit ""))
         (command (format #f "resize set height ~a ~a" amount unit)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-resize-width amount #:key unit (exec #t))
  "Sets the width of the container to width, specified in pixels or percentage points."
  (let* ((unit (or unit ""))
         (command (format #f "resize set width ~a ~a" amount unit)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-show-scratchpad #:key (exec #t))
  "Shows a window from the scratchpad."
  (let* ((command (format #f "scratchpad show")))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-shortcuts-inhibitor flag #:key (exec #t))
  "Enables or disables the ability of clients to inhibit keyboard shortcuts for a view."
  (let* ((flag (if flag "enabled" "disabled"))
         (command (format #f "shortcuts_inhibitor ~a" flag)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-SPLIT-VERTICAL "vertical")
(define SWAY-SPLIT-HORIZONTAL "horizontal")
(define SWAY-SPLIT-NONE "none")
(define SWAY-SPLIT-TOGGLE "toggle")

(define* (sway-split-container option #:key (exec #t))
  "Splits the current container, vertically or horizontally. When none is specified,
   the effect of a previous split is undone.
  parameters:
    - option: `SWAY-SPLIT-VERTICAL`, `SWAY-SPLIT-HORIZONTAL`, `SWAY-SPLIT-NONE`, `SWAY-SPLIT-TOGGLE`"
  (let* ((command (format #f "split ~a" option)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-STICKY-ENABLE "enable")
(define SWAY-STICKY-DISABLE "disable")
(define SWAY-STICKY-TOGGLE "toggle")

(define* (sway-sticky flag #:key (exec #t))
  "Sticks a floating window to the current output so that it shows up on all workspaces.
  parameters:
    - flag: `SWAY-STICKY-ENABLE`, `SWAY-STICKY-DISABLE`, `SWAY-STICKY-TOGGLE`"
  (let* ((flag (cond
                ((equal? #t flag) SWAY-STICKY-ENABLE)
                ((equal? #f flag) SWAY-STICKY-DISABLE)
                (else flag)))
         (command (format #f "sticky ~a" flag)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-SWAY-CONTAINER-TYPE-ID "id")
(define SWAY-SWAY-CONTAINER-TYPE-CONTAINER-ID "con_id")
(define SWAY-SWAY-CONTAINER-TYPE-MARK "mark")

(define* (sway-swap-container type arg #:key (exec #t))
  "Swaps the position, geometry, and fullscreen status of focused container with another
   target container.
  parameters:
    - type: `SWAY-SWAY-CONTAINER-TYPE-ID`, `SWAY-SWAY-CONTAINER-TYPE-CONTAINER-ID`,
			`SWAY-SWAY-CONTAINER-TYPE-MARK`
    - arg: argument passed (based on selected type)"
  (let* ((command (format #f "swap container with ~a ~a" type arg)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-title-format iformat #:key (exec #t))
  "Sets the format of window titles.
  parameters:
    - format: a string that can use some placehodlers to display windows title format
        %title - The title supplied by the window
        %app_id  -  The  wayland app ID (applicable to wayland windows only)
        %class - The X11  classname  (applicable  to  xwayland windows only)
        %instance  -  The X11 instance (applicable to xwayland windows only)
        %shell - The protocol the window is  using  (typically xwayland or xdg_shell)"
  (let* ((command (format #f "title_format ~a" iformat)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-assign-to-workspace criteria workspace #:key (exec #t))
  "Assigns views matching criteria to workspace.
  parameters:
    - criteria: a criteria string, use (sway-criteria) to build a one
    - workspace: workspace name"
  (let* ((command (format #f "assign ~a workspace ~a" criteria workspace)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-assign-to-output criteria output #:key (exec #t))
  "Assigns views matching criteria to output.
  parameters:
    - criteria: a criteria string, use (sway-criteria) to build a one
    - output: output name"
  (let* ((command (format #f "assign ~a output ~a" criteria output)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-bindsym key command #:key whole-window border exclude-titlebar
                       release locked to-code input-device no-warn
                       no-repeat inhibited group (exec #t))
  "Binds key combo to execute the sway command command when pressed.
  parameters:
    - key: a string that represents the key to bind
    - command: a string sway command to execute option receiving the key
    - whole-window: affect the region in which the mouse bindings can be triggered.
    - border: affect the region in which the mouse bindings can be triggered.
    - exclude-titlebar: affect the region in which the mouse bindings can be triggered.
    - release: command is executed when the key combo is released.
    - locked: run command also when screen locking program is active
    - to-code: the keysyms will be translated into the corresponding keycodes
               this will make them layout independant
    - input-device: the binding will only be executed for specified input device
    - no-warn: silence sway warning when overriding a keybinding
    - no-repeat: the command will not be run repeatedly when the key is held
    - inhibited: keyboard shortcuts run also when inhibitor is active for the currently focused window.
    - group: binding will only be available for specified group."
  (let* ((options (string-join
                   (filter (lambda (x) (> (string-length x) 0))
                           (list
                            (if whole-window "--whole-window" "")
                            (if border "--border" "")
                            (if exclude-titlebar "--exclude-titlebar" "")
                            (if exclude-titlebar "--exclude-titlebar" "")
                            (if release "--release" "")
                            (if locked "--locked" "")
                            (if to-code "--to-code" "")
                            (if input-device (string-append "--input-device=" input-device) "")
                            (if no-warn "--no-warn" "")
                            (if no-repeat "--no-repeat" "")
                            (if inhibited "--inhibited" "")
                            (if group "--group" "")))
                   " "))
         (command (format #f "bindsym ~a ~a ~a" options key command)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-bindcode code command #:key whole-window border exclude-titlebar
                       release locked input-device no-warn
                       no-repeat inhibited group (exec #t))
  "for binding with key/button codes instead of key/button names.
  parameters:
    - key: a string that represents the key to bind
    - command: a string sway command to execute option receiving the key
    - whole-window: affect the region in which the mouse bindings can be triggered.
    - border: affect the region in which the mouse bindings can be triggered.
    - exclude-titlebar: affect the region in which the mouse bindings can be triggered.
    - release: command is executed when the key combo is released.
    - locked: run command also when screen locking program is active
    - input-device: the binding will only be executed for specified input device
    - no-warn: silence sway warning when overriding a keybinding
    - no-repeat: the command will not be run repeatedly when the key is held
    - inhibited: keyboard shortcuts run also when inhibitor is active for the currently focused window.
    - group: binding will only be available for specified group."
  (let* ((options (string-join
                   (filter (lambda (x) (> (string-length x) 0))
                           (list
                            (if whole-window "--whole-window" "")
                            (if border "--border" "")
                            (if exclude-titlebar "--exclude-titlebar" "")
                            (if exclude-titlebar "--exclude-titlebar" "")
                            (if release "--release" "")
                            (if locked "--locked" "")
                            (if input-device (string-append "--input-device=" input-device) "")
                            (if no-warn "--no-warn" "")
                            (if no-repeat "--no-repeat" "")
                            (if inhibited "--inhibited" "")
                            (if group "--group" "")))
                   " "))
         (command (format #f "bindcode ~a ~a ~a" options code command)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-bindswitch switch state command #:key locked no-warn reload (exec #t))
  "Binds <switch> to execute the sway command command on state changes.
  parameters:
    - switch: Supported switches are lid (laptop lid) and tablet (tablet mode) switches.
    - state: valid values are on, off and toggle.
    - command: a string sway command to execute option receiving the key
    - locked: run command also when screen locking program is active
    - no-warn: silence sway warning when overriding a keybinding
    - reload: the binding should also be executed when the config is reloaded."
  (let* ((options (string-join
                   (filter (lambda (x) (> (string-length x) 0))
                           (list
                            (if locked "--locked" "")
                            (if no-warn "--no-warn" "")
                            (if reload "--reload" "")))
                   " "))
         (command (format #f "bindswitch ~a ~a:~a ~a" options switch state command)))
    (if exec (sway-dispatch-command command)
        command)))

;; TODO
;;        bindgesture  [--exact]   [--input-device=<device>]   [--no-warn]   <ges‐
;;        ture>[:<fingers>][:directions] <command>
;;            Binds  gesture  to  execute  the sway command command when detected.
;;            Currently supports the hold, pinch or swipe gesture. Optionally  can
;;            be limited to bind to a certain number of fingers or, for a pinch or
;;            swipe gesture, to certain directions.

;;        ┌───────┬─────────┬────────────────────────────────────────────────────┐
;;        │ type  │ fingers │ direction                                          │
;;        ├───────┼─────────┼────────────────────────────────────────────────────┤
;;        │ hold  │  1 - 5  │ none                                               │
;;        ├───────┼─────────┼────────────────────────────────────────────────────┤
;;        │ swipe │  3 - 5  │ up, down, left, right                              │
;;        ├───────┼─────────┼────────────────────────────────────────────────────┤
;;        │ pinch │  2 - 5  │ all  above  + inward, outward, clockwise, counter‐ │
;;        │       │         │ clockwise                                          │
;;        └───────┴─────────┴────────────────────────────────────────────────────┘

;;            The fingers can be limited to any sensible number or left  empty  to
;;            accept  any  finger  counts. Valid directions are up, down, left and
;;            right, as well as inward, outward, clockwise,  counterclockwise  for
;;            the pinch gesture. Multiple directions can be combined by a plus.

;;            If  a  input-device  is given, the binding will only be executed for
;;            that input device and will be executed instead of any  binding  that
;;            is  generic  to all devices. By default, if you overwrite a binding,
;;            swaynag will give you a warning. To silence this, use the  --no-warn
;;            flag.

;;            The  --exact  flag can be used to ensure a binding only matches when
;;            exactly all specified directions are matched and  nothing  more.  If
;;            there is matching binding with --exact, it will be preferred.

;;            The priority for matching bindings is as follows: input device, then
;;            exact  matches followed by matches with the highest number of match‐
;;            ing directions.

;;            Gestures executed while the pointer is above a bar are  not  handled
;;            by sway. See the respective documentation, e.g. bindgesture in sway-
;;            bar(5).

;;            Example:
;;                      # Allow switching between workspaces with left and right swipes
;;                      bindgesture swipe:right workspace prev
;;                      bindgesture swipe:left workspace next

;;                      # Allow container movements by pinching them
;;                      bindgesture pinch:inward+up move up
;;                      bindgesture pinch:inward+down move down
;;                      bindgesture pinch:inward+left move left
;;                      bindgesture pinch:inward+right move right

(define* (sway-unbindswitch switch state #:key (exec #t))
  "Removes a binding for when <switch> changes to <state>.
  parameters:
    - switch: Supported switches are lid (laptop lid) and tablet (tablet mode) switches.
    - state: valid values are on, off and toggle."
  (let* ((command (format #f "unbindswitch ~a:~a" switch state)))
    (if exec (sway-dispatch-command command)
        command)))

;; TODO
;;        unbindgesture   [--exact]   [--input-device=<device>]   <gesture>[:<fin‐
;;        gers>][:directions]
;;            Removes a binding for the specified gesture, fingers and  directions
;;            combination.

(define* (sway-unbindsym key #:key whole-window border exclude-titlebar
                       release locked to-code input-device (exec #t))
  "Removes the binding for key combo that was previously bound with the given flags.
  parameters:
    - key: a string that represents the key to bind
    - whole-window: affect the region in which the mouse bindings can be triggered.
    - border: affect the region in which the mouse bindings can be triggered.
    - exclude-titlebar: affect the region in which the mouse bindings can be triggered.
    - release: command is executed when the key combo is released.
    - locked: run command also when screen locking program is active
    - to-code: the keysyms will be translated into the corresponding keycodes
               this will make them layout independant
    - input-device: the binding will only be executed for specified input device"
  (let* ((options (string-join
                   (filter (lambda (x) (> (string-length x) 0))
                           (list
                            (if whole-window "--whole-window" "")
                            (if border "--border" "")
                            (if exclude-titlebar "--exclude-titlebar" "")
                            (if exclude-titlebar "--exclude-titlebar" "")
                            (if release "--release" "")
                            (if locked "--locked" "")
                            (if to-code "--to-code" "")
                            (if input-device (string-append "--input-device=" input-device) "")))
                   " "))
         (command (format #f "unbindsym ~a ~a" options key)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-unbindcode code #:key whole-window border exclude-titlebar
                       release locked input-device (exec #t))
  "Removes the binding for code that was previously bound with the given flags.
  parameters:
    - key: a string that represents the key to bind
    - whole-window: affect the region in which the mouse bindings can be triggered.
    - border: affect the region in which the mouse bindings can be triggered.
    - exclude-titlebar: affect the region in which the mouse bindings can be triggered.
    - release: command is executed when the key combo is released.
    - locked: run command also when screen locking program is active
    - input-device: the binding will only be executed for specified input device"
  (let* ((options (string-join
                   (filter (lambda (x) (> (string-length x) 0))
                           (list
                            (if whole-window "--whole-window" "")
                            (if border "--border" "")
                            (if exclude-titlebar "--exclude-titlebar" "")
                            (if exclude-titlebar "--exclude-titlebar" "")
                            (if release "--release" "")
                            (if locked "--locked" "")
                            (if input-device (string-append "--input-device=" input-device) "")))
                   " "))
         (command (format #f "unbindcode ~a ~a" options code)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-unmark identifier #:key (exec #t))
  "remove identifier from the list of current marks on a window.
  Parameters:
   - mark: string mark."
  (let* ((command (format #f "unmark ~a" identifier)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-URGENT-ENABLE "enable")
(define SWAY-URGENT-DISABLE "disable")
(define SWAY-URGENT-ALLOW "allow")
(define SWAY-URGENT-DENY "deny")

(define* (sway-urgent flag #:key (exec #t))
  "Using enable or disable manually sets or unsets the window's urgent state.
  Parameters:
   - flag: `SWAY-URGENT-ENABLE`, `SWAY-URGENT-DISABLE`,
           `SWAY-URGENT-ALLOW`, `SWAY-URGENT-DENY`"
  (let* ((command (format #f "urgent ~a" flag)))
    (if exec (sway-dispatch-command command)
        command)))

;; The meaning of each color is:
;; border: The border around the title bar.
;; background: The background of the title bar.
;; text: The text color of the title bar.
;; indicator: The  color  used  to  indicate  where a new view will open.
;; child_border: The border around the view itself.
(define* (sway-client-background color #:key (exec #t))
  "This command is ignored and is only present for i3 compatibility.
  parameters:
    - color: color code to be used (str)"
  (let* ((command (format #f "client.background ~a" color)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-client-focused-color border-color background-color text-color
                                   #:key indictor-color child-border-color (exec #t))
  "Configures the color of window borders and title bars of the window that has focus.
  parameters:
    - border-color: color code to be used for border (str)
    - background-color: color code to be used for background (str)
    - text-color: color code to be used for text (str)
    - indictor-color: color code to be used for indicator (str)
    - child-border-color: color code to be used for child border (str)"
  (let* ((indictor-color (or indictor-color ""))
         (child-border-color (or child-border-color ""))
         (command (format #f "client.focused ~a ~a ~a ~a ~a"
                          border-color background-color text-color
                          indictor-color child-border-color)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-client-focused-inactive-color border-color background-color text-color
                                   #:key indictor-color child-border-color (exec #t))
  "Configures the color of window borders and title bars of the most
   recently focused view within a container which  is  not focused.
  parameters:
    - border-color: color code to be used for border (str)
    - background-color: color code to be used for background (str)
    - text-color: color code to be used for text (str)
    - indictor-color: color code to be used for indicator (str)
    - child-border-color: color code to be used for child border (str)"
  (let* ((indictor-color (or indictor-color ""))
         (child-border-color (or child-border-color ""))
         (command (format #f "client.focused_inactive ~a ~a ~a ~a ~a"
                          border-color background-color text-color
                          indictor-color child-border-color)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-client-focused-tab-title-color border-color background-color text-color #:key (exec #t))
  "Configures the color of window borders and title bars of a
   view that has focused descendant container.
  parameters:
    - border-color: color code to be used for border (str)
    - background-color: color code to be used for background (str)
    - text-color: color code to be used for text (str)
    - indictor-color: color code to be used for indicator (str)
    - child-border-color: color code to be used for child border (str)"
  (let* ((command (format #f "client.focused_tab_title ~a ~a ~a"
                          border-color background-color text-color)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-client-placeholder-color border-color background-color text-color
                                   #:key indictor-color child-border-color (exec #t))
  "Ignored (present for i3 compatibility).
  parameters:
    - border-color: color code to be used for border (str)
    - background-color: color code to be used for background (str)
    - text-color: color code to be used for text (str)
    - indictor-color: color code to be used for indicator (str)
    - child-border-color: color code to be used for child border (str)"
  (let* ((indictor-color (or indictor-color ""))
         (child-border-color (or child-border-color ""))
         (command (format #f "client.placeholder ~a ~a ~a ~a ~a"
                          border-color background-color text-color
                          indictor-color child-border-color)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-client-unfocused-color border-color background-color text-color
                                   #:key indictor-color child-border-color (exec #t))
  "Configures the color of window borders and title bars of a
   view that does not have focus.
  parameters:
    - border-color: color code to be used for border (str)
    - background-color: color code to be used for background (str)
    - text-color: color code to be used for text (str)
    - indictor-color: color code to be used for indicator (str)
    - child-border-color: color code to be used for child border (str)"
  (let* ((indictor-color (or indictor-color ""))
         (child-border-color (or child-border-color ""))
         (command (format #f "client.unfocused ~a ~a ~a ~a ~a"
                          border-color background-color text-color
                          indictor-color child-border-color)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-client-urgent-color border-color background-color text-color
                                   #:key indictor-color child-border-color (exec #t))
  "Configures the color of window borders and title bars of a
   view with an urgency hint..
  parameters:
    - border-color: color code to be used for border (str)
    - background-color: color code to be used for background (str)
    - text-color: color code to be used for text (str)
    - indictor-color: color code to be used for indicator (str)
    - child-border-color: color code to be used for child border (str)"
  (let* ((indictor-color (or indictor-color ""))
         (child-border-color (or child-border-color ""))
         (command (format #f "client.urgent ~a ~a ~a ~a ~a"
                          border-color background-color text-color
                          indictor-color child-border-color)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-BORDER-STYLE-NONE "none")
(define SWAY-BORDER-STYLE-NORMAL "normal")
(define SWAY-BORDER-STYLE-PIXEL "pixel")

(define* (sway-default-border-style type #:key n (exec #t))
  "Set default border style for new tiled windows.
  parameters:
    - type: `SWAY-BORDER-STYLE-NONE`, `SWAY-BORDER-STYLE-NORMAL`, `SWAY-BORDER-STYLE-PIXEL`
    - n: units in case pixel is chosen (number)"
  (let* ((n (or n ""))
         (command (format #f "default_border ~a ~a" type n)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-default-floating-border-style type #:key n (exec #t))
  "Set default border style for new tiled windows.
  parameters:
    - type: color code to be used for border (str)
    - n: units in case pixel is chosen (number)"
  (let* ((n (or n ""))
         (command (format #f "default_floating_border ~a ~a" type n)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-exec command #:key (exec #t))
  "Executes shell command with sh.
  parameters:
    - command: command to be executed (str)"
  (let* ((command (format #f "exec ~a" command)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-exec-always command #:key (exec #t))
  "Like exec, but the shell command will be executed again after reload.
  parameters:
    - command: command to be executed (str)"
  (let* ((command (format #f "exec_always ~a" command)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-floating-maximum-size width height #:key (exec #t))
  "Specifies the maximum size of floating windows.
  parameters:
    - width: target size width (number)
    - height: target size height (number)"
  (let* ((command (format #f "floating_maximum_size ~a ~a" width height)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-floating-minimum-size width height #:key (exec #t))
  "Specifies the minimum size of floating windows.
  parameters:
    - width: target size width (number)
    - height: target size height (number)"
  (let* ((command (format #f "floating_minimum_size ~a ~a" width height)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-FLOATING-MODIFIER-TYPE-NORMAL "normal")
(define SWAY-FLOATING-MODIFIER-TYPE-INVERSE "inverse")

(define* (sway-floating-modifier modifier type #:key (exec #t))
  "When the modifier key is held down, you may hold left click to move windows,
   and right click to resize them.
  parameters:
    - modifier: the modifier key (str)
    - type: `SWAY-FLOATING-MODIFIER-TYPE-NORMAL`, `SWAY-FLOATING-MODIFIER-TYPE-INVERSE`"
  (let* ((command (format #f "floating_modifier ~a ~a" modifier type)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-FOCUS-FOLLOW-MOUSE-FLAG-YES "yes")
(define SWAY-FOCUS-FOLLOW-MOUSE-FLAG-NO "no")
(define SWAY-FOCUS-FOLLOW-MOUSE-FLAG-ALWAYS "always")

(define* (sway-focus-follow-mouse flag #:key (exec #t))
  "If set to yes, moving your mouse over a window will focus that window.
   If set to always, the window under the cursor will always be
   focused, even after switching between workspaces.
  parameters:
    - flag: `SWAY-FOCUS-FOLLOW-MOUSE-FLAG-YES`, `SWAY-FOCUS-FOLLOW-MOUSE-FLAG-NO`,
			`SWAY-FOCUS-FOLLOW-MOUSE-FLAG-ALWAYS`"
  (let* ((command (format #f "focus_follows_mouse ~a" flag)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-FOCUS-ON-WINDOW-ACTIVATION-FLAG-SMART "smart")
(define SWAY-FOCUS-ON-WINDOW-ACTIVATION-FLAG-URGENT "urgent")
(define SWAY-FOCUS-ON-WINDOW-ACTIVATION-FLAG-FOCUS "focus")
(define SWAY-FOCUS-ON-WINDOW-ACTIVATION-FLAG-NONE "none")

(define* (sway-focus-on-window-activation flag #:key (exec #t))
  "This option determines what to do when a client requests window activation.
  parameters:
    - flag: `SWAY-FOCUS-ON-WINDOW-ACTIVATION-FLAG-SMART`, `SWAY-FOCUS-ON-WINDOW-ACTIVATION-FLAG-URGENT`,
			`SWAY-FOCUS-ON-WINDOW-ACTIVATION-FLAG-FOCUS`, `SWAY-FOCUS-ON-WINDOW-ACTIVATION-FLAG-NONE`"
  (let* ((command (format #f "focus_on_window_activation ~a" flag)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-FOCUS-WRAPPING-FLAG-YES "yes")
(define SWAY-FOCUS-WRAPPING-FLAG-NO "no")
(define SWAY-FOCUS-WRAPPING-FLAG-FORCE "force")
(define SWAY-FOCUS-WRAPPING-FLAG-WORKSPACE "workspace")

(define* (sway-focus-wrapping flag #:key (exec #t))
  "This option determines what to do when a client requests window activation.
  parameters:
    - flag: `SWAY-FOCUS-WRAPPING-FLAG-YES`, `SWAY-FOCUS-WRAPPING-FLAG-NO`,
			`SWAY-FOCUS-WRAPPING-FLAG-FORCE`, `SWAY-FOCUS-WRAPPING-FLAG-WORKSPACE`"
  (let* ((command (format #f "focus_wrapping ~a" flag)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-font font #:key pango (exec #t))
  "Sets font to use for the title bars. To enable support for pango markup,
   preface the font name with pango:
  parameters:
    - font: font name (str)
    - pango: whether to use pango or not (boolean)"
  (let* ((pango (if pango "pango:" ""))
         (command (format #f "font ~a~a" pango font)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-force-display-urgency-hint timeout #:key (exec #t))
  "If an application on another workspace sets an urgency hint.
  parameters:
    - timeout: urgency timeout (number)"
  (let* ((command (format #f "force_display_urgency_hint ~a" timeout)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-titlebar-border-thickness thickness #:key (exec #t))
  "Thickness of the titlebar border in pixels.
  parameters:
    - thickness: thickness of border (number)"
  (let* ((command (format #f "titlebar_border_thickness ~a" thickness)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-titlebar-padding horizontal vertical #:key (exec #t))
  "Padding of the text in the titlebar.
  parameters:
    - horizontal: horizontal padding (number)
    - vertical: vertical padding (number)"
  (let* ((command (format #f "titlebar_padding ~a ~a" horizontal vertical)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-for-window criteria commands #:key (exec #t))
  "Whenever a window that matches criteria appears, run list of commands.
  parameters:
    - criteria: a criteria string, use (sway-criteria) to build a one
    - command: list of commands to execute (string comma seperated)"
  (let* ((command (format #f "for_window ~a ~a" criteria commands)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-default-gaps option amount #:key (exec #t))
  "Sets default amount pixels of inner or outer gap.
  parameters:
    - option: `SWAY-GAPS-OPTION-INNER`, `SWAY-GAPS-OPTION-OUTER`, `SWAY-GAPS-OPTION-HORIZONTAL`,
              `SWAY-GAPS-OPTION-VERTICAL`, `SWAY-GAPS-OPTION-TOP`, `SWAY-GAPS-OPTION-RIGHT`,
              `SWAY-GAPS-OPTION-BOTTOM`, `SWAY-GAPS-OPTION-LEFT`
    - amount: amount of gap (number)"
  (let* ((command (format #f "gaps ~a ~a" option amount)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-EDGE-BORDER-TYPE-NONE "none")
(define SWAY-EDGE-BORDER-TYPE-VERTICAL "vertical")
(define SWAY-EDGE-BORDER-TYPE-HORIZONTAL "horizontal")
(define SWAY-EDGE-BORDER-TYPE-BOTH "both")
(define SWAY-EDGE-BORDER-TYPE-SMART "smart")
(define SWAY-EDGE-BORDER-TYPE-SMART-NO-GAPS "smart_no_gaps")

(define* (sway-hide-edge-borders type #:key i3 (exec #t))
  "Hides window borders adjacent to the screen edges.
  parameters:
    - type: `SWAY-EDGE-BORDER-TYPE-NONE`, `SWAY-EDGE-BORDER-TYPE-VERTICAL`, `SWAY-EDGE-BORDER-TYPE-HORIZONTAL`,
              `SWAY-EDGE-BORDER-TYPE-BOTH`, `SWAY-EDGE-BORDER-TYPE-SMART`, `SWAY-EDGE-BORDER-TYPE-SMART-NO-GAPS`
    - i3: enables i3-compatible behavior to hide the title bar on tabbed and stacked containers with one child"
  (let* ((i3 (if i3 "--i3" ""))
         (command (format #f "hide_edge_borders ~a ~a" i3 type)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-input device subcommands #:key (exec #t))
  "For details on input subcommands, see sway-input(5).
  parameters:
    - device: the name of the target device
    - subcommands: list of commands to execute (string)"
  (let* ((command (format #f "input ~a ~a" device subcommands)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-seat seat subcommands #:key (exec #t))
  "For details on input subcommands, see sway-input(5).
  parameters:
    - seat: the name of the seat device
    - subcommands: list of commands to execute (string)"
  (let* ((command (format #f "seat ~a ~a" seat subcommands)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-kill #:key (exec #t))
  "Kills (closes) the currently focused container and all of its children."
  (let* ((command (format #f "kill")))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-SMART-BORDERS-ON "on")
(define SWAY-SMART-BORDERS-OFF "off")
(define SWAY-SMART-BORDERS-NO-GAPS "no_gaps")

(define* (sway-smart-borders flag #:key (exec #t))
  "If smart_borders are on, borders will only be enabled if the
   workspace has more than one visible child.
  parameters:
    - flag: `SWAY-SMART-BORDERS-ON`, `SWAY-SMART-BORDERS-OFF`, `SWAY-SMART-BORDERS-NO-GAPS`"
  (let* ((flag (cond
                ((equal? #t flag) SWAY-SMART-BORDERS-ON)
                ((equal? #f flag) SWAY-SMART-BORDERS-OFF)
                (else flag)))
         (command (format #f "smart_borders ~a" flag)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-SMART-GAPS-ON "on")
(define SWAY-SMART-GAPS-OFF "off")
(define SWAY-SMART-GAPS-TOGGLE "toggle")
(define SWAY-SMART-GAPS-INVERSE-OUTER "inverse_outer")

(define* (sway-smart-gaps flag #:key (exec #t))
  "If smart_gaps are on gaps will only be enabled if a
   workspace has more than one child.
  parameters:
    - flag: `SWAY-SMART-GAPS-ON`, `SWAY-SMART-GAPS-OFF`,
			`SWAY-SMART-GAPS-TOGGLE`, `SWAY-SMART-GAPS-INVERSE-OUTER`"
  (let* ((flag (cond
                ((equal? #t flag) SWAY-SMART-GAPS-ON)
                ((equal? #f flag) SWAY-SMART-GAPS-OFF)
                (else flag)))
         (command (format #f "smart_gaps ~a" flag)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-mark identifier #:key add toggle (exec #t))
  "Marks are arbitrary labels that can be used to identify certain
windows and then jump to them at a later time.
  parameters:
    - add: add identifier to the list of current marks for that window.
    - toggle: remove identifier if it is already marked.
    - identifier: label that can be used to identify that window."
  (let* ((command (format #f "mark ~a ~a ~a"
                          (if add "--add" "")
                          (if toggle "--toggle" "")
                          identifier)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-mode mode #:key (exec #t))
  "Switches to the specified mode. The default mode is default.
  parameters:
    - mode: name of the mode (str)"
  (let* ((command (format #f "mode \"~a\"" mode)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-mode-subcommand mode subcommand #:key (exec #t))
  "The only valid mode-subcommands are bindsym, bindcode, bindswitch, and set.
  parameters:
    - mode: name of the mode (str)
    - subcommand: list of subcommands (str)"
  (let* ((command (format #f "mode \"~a\" ~a" mode subcommand)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-MOUSE-WARPING-OUTPUT "output")
(define SWAY-MOUSE-WARPING-CONTAINER "container")
(define SWAY-MOUSE-WARPING-NONE "none")

(define* (sway-mouse-warping mode #:key (exec #t))
  "If output is specified, the mouse will be moved to new outputs as you move focus between them.
   If container is specified, the mouse will be moved to the middle of the container on switch.
  parameters:
    - mode: `SWAY-MOUSE-WARPING-OUTPUT`, `SWAY-MOUSE-WARPING-CONTAINER`, `SWAY-MOUSE-WARPING-NONE`"
  (let* ((command (format #f "mouse_warping ~a" mode)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-no-focus criteria #:key (exec #t))
  "Prevents windows matching <criteria> from being focused automatically when they're created.
  parameters:
    - criteria: a criteria string, use (sway-criteria) to build a one"
  (let* ((command (format #f "no_focus ~a" criteria)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-output output subcommands #:key (exec #t))
  "For details on output subcommands, see sway-output(5).
  parameters:
    - output: name of the output (str)
    - subcommand: list of subcommands (str)"
  (let* ((command (format #f "output ~a ~a" output subcommands)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-POPUP-TYPE-OUTPUTSMART "outputsmart")
(define SWAY-POPUP-TYPE-IGNORE "ignore")
(define SWAY-POPUP-TYPE-LEAVE-FULLSCREEN "leave_fullscreen")

(define* (sway-popup-during-fullscreen type #:key (exec #t))
  "Determines what to do when a fullscreen view opens a dialog.
  parameters:
    - type: `SWAY-POPUP-TYPE-OUTPUTSMART`, `SWAY-POPUP-TYPE-IGNORE`, `SWAY-POPUP-TYPE-LEAVE-FULLSCREEN`"
  (let* ((command (format #f "popup_during_fullscreen ~a" type)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-PRIMARY-SELECTION-ENABLED "enabled")
(define SWAY-PRIMARY-SELECTION-DISABLED "disabled")

(define* (sway-primary-selection type #:key (exec #t))
  "Enable or disable the primary selection clipboard. May only be configured at launch. Default is enabled.
  parameters:
    - type: `SWAY-PRIMARY-SELECTION-ENABLED`, `SWAY-PRIMARY-SELECTION-DISABLED`"
  (let* ((type (cond
                ((equal? type #t) SWAY-PRIMARY-SELECTION-ENABLED)
                ((equal? type #f) SWAY-PRIMARY-SELECTION-DISABLED)
                (else type)))
         (command (format #f "primary_selection ~a" type)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-SHOW-MARKS-YES "yes")
(define SWAY-SHOW-MARKS-NO "no")

(define* (sway-show-marks flag #:key (exec #t))
  "If show_marks is yes, marks will be displayed in the window borders.
  parameters:
    - flag: `SWAY-SHOW-MARKS-YES`, `SWAY-SHOW-MARKS-NO`"
  (let* ((flag (cond
                   ((equal? flag #t) SWAY-SHOW-MARKS-YES)
                   ((equal? flag #f) SWAY-SHOW-MARKS-NO)
                   (else flag)))
         (command (format #f "show_marks ~a" flag)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-OPACITY-SET "set")
(define SWAY-OPACITY-PLUS "plus")
(define SWAY-OPACITY-MINUS "minus")

(define* (sway-opacity type value #:key (exec #t))
  "Adjusts the opacity of the window between 0 (completely transparent) and 1 (completely opaque)
  parameters:
    - type: `SWAY-OPACITY-SET`, `SWAY-OPACITY-PLUS`, `SWAY-OPACITY-MINUS`
	- value: opacity value (number) should be between 0 and 1"
  (let* ((command (format #f "opacity ~a ~a" type value)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-TILING-DRAG-ENABLE "enable")
(define SWAY-TILING-DRAG-DISABLE "disable")
(define SWAY-TILING-DRAG-TOGGLE "toggle")

(define* (sway-tiling-drag flag #:key (exec #t))
  "Sets whether or not tiling containers can be dragged with the mouse.
  parameters:
    - flag: `SWAY-TILING-DRAG-ENABLE`, `SWAY-TILING-DRAG-DISABLE`, `SWAY-TILING-DRAG-TOGGLE`"
  (let* ((flag (cond
                ((equal? flag #t) SWAY-TILING-DRAG-ENABLE)
                ((equal? flag #f) SWAY-TILING-DRAG-DISABLE)
                (else flag)))
         (command (format #f "tiling_drag ~a" flag)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-tiling-drag-threshold threshold #:key (exec #t))
  "Sets whether or not tiling containers can be dragged with the mouse.
  parameters:
    - threshold: threshold value (number)"
  (let* ((command (format #f "tiling_drag_threshold ~a" threshold)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-TILING-ALIGN-LEFT "left")
(define SWAY-TILING-ALIGN-CENTER "center")
(define SWAY-TILING-ALIGN-RIGHT "right")

(define* (sway-tiling-align type #:key (exec #t))
  "Sets the title alignment.
  parameters:
    - type: `SWAY-TILING-ALIGN-LEFT`, `SWAY-TILING-ALIGN-CENTER`, `SWAY-TILING-ALIGN-RIGHT`"
  (let* ((command (format #f "title_align ~a" type)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-switch-workspace-id id #:key auto-back-and-forth (exec #t))
  "switch to the workspace with the provided id.
  parameters:
	- id: workspace id (number)
    - auto-back-and-forth: enable/disable auto back and forth"
  (let* ((back-forth (if auto-back-and-forth "--no-auto-back-and-forth" ""))
         (command (format #f "workspace number ~a ~a" back-forth id)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-switch-workspace workspace #:key auto-back-and-forth (exec #t))
  "switch to the workspace with the provided name.
  parameters:
	- workspace: workspace name (str)
    - auto-back-and-forth: enable/disable auto back and forth"
  (let* ((back-forth (if auto-back-and-forth "--no-auto-back-and-forth" ""))
         (command (format #f "workspace ~a ~a" back-forth workspace)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-switch-workspace-on-output workspace output #:key (exec #t))
  "assigns workspace to output.
  parameters:
	- workspace: workspace name (str)
    - output: output name"
  (let* ((command (format #f "workspace ~a output ~a" workspace output)))
    (if exec (sway-dispatch-command command)
        command)))

(define SWAY-WORKSPACE-AUTO-BACK-AND-FORTH-OPTION-YES "yes")
(define SWAY-WORKSPACE-AUTO-BACK-AND-FORTH-OPTION-NO "no")

(define* (sway-workspace-auto-back-and-forth option #:key (exec #t))
  "When yes, repeating a workspace switch command will switch back to the prior workspace.
  parameters:
	- option: `SWAY-WORKSPACE-AUTO-BACK-AND-FORTH-OPTION-YES`, `SWAY-WORKSPACE-AUTO-BACK-AND-FORTH-OPTION-NO`"
  (let* ((option (cond
                   ((equal? option #t) SWAY-WORKSPACE-AUTO-BACK-AND-FORTH-OPTION-YES)
                   ((equal? option #f) SWAY-WORKSPACE-AUTO-BACK-AND-FORTH-OPTION-NO)
                   (else option)))
         (command (format #f "workspace_auto_back_and_forth ~a" option)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-workspace-gaps workspace option amount #:key (exec #t))
  "Specifies that workspace name should have the given gaps settings when it is created.
  This command does not affect existing workspaces. To alter the gaps of an existing workspace,
  use the `sway-gaps` command.
  parameters:
	- workspace: workspace name (str)
    - option: `SWAY-GAPS-OPTION-INNER`, `SWAY-GAPS-OPTION-OUTER`, `SWAY-GAPS-OPTION-HORIZONTAL`,
              `SWAY-GAPS-OPTION-VERTICAL`, `SWAY-GAPS-OPTION-TOP`, `SWAY-GAPS-OPTION-RIGHT`,
              `SWAY-GAPS-OPTION-BOTTOM`, `SWAY-GAPS-OPTION-LEFT`
	- amount: the amount of gap (number)"
  (let* ((command (format #f "workspace ~a ~a ~a" workspace option amount)))
    (if exec (sway-dispatch-command command)
        command)))

(define* (sway-criteria #:key app-id class con-id con-mark
                        floating id instance pid shell tiling title urgent
                        window-role window-type workspace (exec #t))
  "Generate a string that contains one or more (space separated) attribute/value pairs."
  (string-append
   "["
   (string-join
    (filter (lambda (x) (and x (> (string-length x) 0)))
            (list
             (if class (format #f "class=\"~a\"" class) "")
             (if con-id (format #f "con_id=~a" con-id) "")
             (if con-mark (format #f "con_mark=\"~a\"" con-mark) "")
             (if floating (format #f "floating=\"~a\"" floating) "")
             (if id (format #f "id=~a" id) "")
             (if instance (format #f "instance=\"~a\"" instance) "")
             (if pid (format #f "pid=~a" pid) "")
             (if shell (format #f "shell=\"~a\"" shell) "")
             (if tiling (format #f "tiling=\"~a\"" tiling) "")
             (if title (format #f "title=\"~a\"" title) "")
             (if urgent (format #f "urgent=\"~a\"" urgent) "")
             (if window-role (format #f "window_role=\"~a\"" window-role) "")
             (if window-type (format #f "window_type=\"~a\"" window-type) "")
             (if workspace (format #f "workspace=\"~a\"" workspace) "")))
    " ")
   "]"))
