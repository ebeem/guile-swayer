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
            SWAY-GAPS-WORKSPACE-SET
            SWAY-GAPS-WORKSPACE-PLUS
            SWAY-GAPS-WORKSPACE-MINUS
            SWAY-GAPS-WORKSPACE-TOGGLE
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
            SWAY-RESIZE-TYPE-GROW-WIDTH
            SWAY-RESIZE-TYPE-GROW-HEIGHT
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

(define (sway-default-orientation orientation)
  "Sets the default container layout for tiled containers.
  parameters:
    - orientation: `SWAY-ORIENTATION-HORIZONTAL`, `SWAY-ORIENTATION-VERTICAL`, `SWAY-ORIENTATION-AUTO`"
  (sway-dispatch-command
   (string-append "default_orientation " orientation)))

(define (sway-include file-path)
  "Includes another configuration file from path (not scheme file).
  parameters:
    - file-path: string"
  (sway-dispatch-command
   (string-append "include " file-path)))

(define (sway-swaybg-command command)
  "Executes custom background command.  Default  is  swaybg.
  parameters:
    - command: string"
  (sway-dispatch-command
   (string-append "swaybg_command " command)))

(define (sway-swaynag-command command)
  "Executes custom command for swaynag. Default is swaynag.
  parameters:
    - command: string"
  (sway-dispatch-command
   (string-append "swaynag_command " command)))

(define SWAY-LAYOUT-DEFAULT "default")
(define SWAY-LAYOUT-STACKING "stacking")
(define SWAY-LAYOUT-TABBED "tabbed")

(define (sway-workspace-layout layout)
  "Specifies the initial layout for new containers in  an  empty  workspace.
  parameters:
    - layout: `SWAY-LAYOUT-DEFAULT`, `SWAY-LAYOUT-STACKING`, `SWAY-LAYOUT-TABBED`"
  (sway-dispatch-command
   (string-append "workspace_layout " layout)))

(define SWAY-XWAYLAND-ENABLE "enable")
(define SWAY-XWAYLAND-DISABLE "disable")
(define SWAY-XWAYLAND-FORCE "force")

(define (sway-xwayland option)
  "Enables or disables Xwayland support, which allows X11 applications to be used.
  parameters:
    - option: `SWAY-XWAYLAND-ENABLE`, `SWAY-XWAYLAND-DISABLE`, `SWAY-XWAYLAND-FORCE`"
  (sway-dispatch-command
   (string-append "xwayland " (cond
                               ((equal? #t option) SWAY-XWAYLAND-ENABLE)
                               ((equal? #f option) SWAY-XWAYLAND-DISABLE)
                               (else option)))))

(define SWAY-BORDER-NONE "none")
(define SWAY-BORDER-NORMAL "normal")
(define SWAY-BORDER-CSD "csd")
(define SWAY-BORDER-PIXEL "pixel")

(define (sway-border option thickness)
  "Enables  or disables Xwayland support, which allows X11 applications to be used.
  parameters:
    - option: `SWAY-BORDER-NONE`, `SWAY-BORDER-NORMAL`, `SWAY-BORDER-CSD`, `SWAY-BORDER-PIXEL`
	- thickness: int"
  (sway-dispatch-command
   (string-append "border " option (number->string thickness))))

(define (sway-border-toggle)
  "Cycles through the available border styles."
  (sway-dispatch-command
   (string-append "border toggle")))

(define (sway-exit)
  "Exit sway and end your Wayland session."
  (sway-dispatch-command
   (string-append "exit")))

(define SWAY-FLOATING-ENABLED "enabled")
(define SWAY-FLOATING-DISABLED "disabled")
(define SWAY-FLOATING-TOGGLE "toggle")

(define (sway-floating option)
  "Make focused view floating, non-floating, or the opposite of what it is now.
  parameters:
    - layout: `SWAY-FLOATING-ENABLED`, `SWAY-FLOATING-DISABLED`, `SWAY-FLOATING-TOGGLE`"
  (sway-dispatch-command
   (string-append "floating " (cond
                               ((equal? #t option) SWAY-FLOATING-ENABLED)
                               ((equal? #f option) SWAY-FLOATING-DISABLED)
                               (else option)))))

(define (sway-focus-container-criteria criteria)
  "Moves focus to the container that matches the specified criteria.
  parameters:
    - criteria: sway criteria"
  (sway-dispatch-command
   (string-append criteria " focus")))

(define SWAY-DIRECTION-UP "up")
(define SWAY-DIRECTION-RIGHT "right")
(define SWAY-DIRECTION-DOWN "down")
(define SWAY-DIRECTION-LEFT "left")
(define SWAY-SIBLING-NEXT "next")
(define SWAY-SIBLING-PREV "prev")
(define SWAY-HIERARCHY-CHILD "child")
(define SWAY-HIERARCHY-PARENT "parent")

(define (sway-focus-container direction)
  "Moves focus to the next container in the specified direction.
  parameters:
    - direction: `SWAY-DIRECTION-UP`, `SWAY-DIRECTION-RIGHT`, `SWAY-DIRECTION-DOWN`, `SWAY-DIRECTION-LEFT`"
  (sway-dispatch-command
   (string-append "focus " direction)))

(define (sway-focus-container-sibling sibling)
  "Moves focus to the previous or next container in the current layout.
  parameters:
    - sibling: `SWAY-SIBLING-NEXT`, `SWAY-SIBLING-PREV`"
  (sway-dispatch-command
   (string-append "focus " sibling)))

(define (sway-focus-container-child)
  "Moves focus to the last-focused child of the focused container."
  (sway-dispatch-command
   (string-append "focus child")))

(define (sway-focus-container-parent)
  "Moves focus to the last-focused parent of the focused container."
  (sway-dispatch-command
   (string-append "focus parent")))

(define (sway-focus-output-direction direction)
  "Moves focus to the next output in the specified direction.
  parameters:
    - direction: `SWAY-DIRECTION-UP`, `SWAY-DIRECTION-RIGHT`, `SWAY-DIRECTION-DOWN`, `SWAY-DIRECTION-LEFT`"
  (sway-dispatch-command
   (string-append "focus output " direction)))

(define (sway-focus-output-name name)
  "Moves focus to the named output.
  parameters:
    - name: string, output name"
  (sway-dispatch-command
   (string-append "focus output " name)))

(define (sway-focus-container-tiling)
  "Sets focus to the last focused tiling container."
  (sway-dispatch-command
   (string-append "focus tiling")))

(define (sway-focus-container-floating)
  "Sets focus to the last focused floating container."
  (sway-dispatch-command
   (string-append "focus floating")))

(define SWAY-FULLSCREEN-ENABLED "enabled")
(define SWAY-FULLSCREEN-DISABLED "disabled")
(define SWAY-FULLSCREEN-TOGGLE "toggle")

(define* (sway-fullscreen option #:key global)
  "Makes focused view fullscreen, non-fullscreen, or the opposite of current.
  parameters:
    - option: `SWAY-FULLSCREEN-ENABLED`, `SWAY-FULLSCREEN-DISABLED`, `SWAY-FULLSCREEN-TOGGLE`
    - global: #t, #f"
  (sway-dispatch-command
   (string-append "fullscreen " (cond
                                 ((equal? #t option) SWAY-FULLSCREEN-ENABLED)
                                 ((equal? #f option) SWAY-FULLSCREEN-DISABLED)
                                 (else option))
                  (if global " global" ""))))

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
(define SWAY-GAPS-WORKSPACE-SET "set")
(define SWAY-GAPS-WORKSPACE-PLUS "plus")
(define SWAY-GAPS-WORKSPACE-MINUS "minus")
(define SWAY-GAPS-WORKSPACE-TOGGLE "toggle")

(define (sway-gaps option workspace amount)
  "Changes the inner or outer gaps for either all workspaces or the current workspace.
  parameters:
    - option: `SWAY-GAPS-OPTION-INNER`, `SWAY-GAPS-OPTION-OUTER`, `SWAY-GAPS-OPTION-HORIZONTAL`,
              `SWAY-GAPS-OPTION-VERTICAL`, `SWAY-GAPS-OPTION-TOP`, `SWAY-GAPS-OPTION-RIGHT`,
              `SWAY-GAPS-OPTION-BOTTOM`, `SWAY-GAPS-OPTION-LEFT`
    - workspace: `SWAY-GAPS-WORKSPACE-ALL`, `SWAY-GAPS-WORKSPACE-CURRENT`, `SWAY-GAPS-WORKSPACE-SET`,
                 `SWAY-GAPS-WORKSPACE-PLUS`, `SWAY-GAPS-WORKSPACE-MINUS`, `SWAY-GAPS-WORKSPACE-TOGGLE`
    - amount: amount of gap (number)"
  (sway-dispatch-command
   (string-append "gaps " option " " workspace " " (number->string amount))))

(define SWAY-INHIBIT-IDLE-FOCUS "focus")
(define SWAY-INHIBIT-IDLE-FULLSCREEN "fullscreen")
(define SWAY-INHIBIT-IDLE-OPEN "open")
(define SWAY-INHIBIT-IDLE-NONE "none")
(define SWAY-INHIBIT-IDLE-VISIBLE "visible")

(define (sway-inhibit-idle option)
  "Set/unset an idle inhibitor for the view.
  parameters:
    - option: `SWAY-INHIBIT-IDLE-FOCUS`, `SWAY-INHIBIT-IDLE-FULLSCREEN`, `SWAY-INHIBIT-IDLE-OPEN`,
              `SWAY-INHIBIT-IDLE-NONE`, `SWAY-INHIBIT-IDLE-VISIBLE`"
  (sway-dispatch-command
   (string-append "inhibit_idle " option)))

(define SWAY-LAYOUT-SPLITH "splith")
(define SWAY-LAYOUT-SPLITV "splitv")
(define SWAY-LAYOUT-STACKING "stacking")

(define (sway-layout option)
  "Set/unset an idle inhibitor for the view.
  parameters:
    - option: `SWAY-LAYOUT-DEFAULT`, `SWAY-LAYOUT-SPLITH`, `SWAY-LAYOUT-SPLITV`,
              `SWAY-LAYOUT-STACKING`, `SWAY-LAYOUT-TABBED`"
  (sway-dispatch-command
   (string-append "layout " option)))

(define SWAY-LAYOUT-TOGGLE-ALL "all")
(define SWAY-LAYOUT-TOGGLE-SPLIT "split")

(define* (sway-layout-toggle #:key option)
  "Cycles the layout mode of the focused container though a preset list of layouts.
  parameters:
    - option: `SWAY-LAYOUT-TOGGLE-ALL`, `SWAY-LAYOUT-TOGGLE-SPLIT`"
  (sway-dispatch-command
   (string-append "layout toggle" (if option (string-append " " option) ""))))

(define* (sway-move-container direction #:key amount)
  "Moves the focused container in the direction specified.
  parameters:
    - direction: `SWAY-DIRECTION-UP`, `SWAY-DIRECTION-RIGHT`, `SWAY-DIRECTION-DOWN`, `SWAY-DIRECTION-LEFT`
    - amount: int"
  (sway-dispatch-command
   (string-append "move " direction
                  (if amount (string-append " " (number->string amount)) ""))))

(define (sway-move-container-absolute-position x y)
  "Moves the focused container to the specified position in the workspace.
  parameters:
    - x: int
    - y: int"
  (sway-dispatch-command
   (string-append "move absolute position " (number->string x) " " (number->string y))))

(define (sway-move-container-absolute-center)
  "Moves the focused container to be centered on the workspace."
  (sway-dispatch-command
   (string-append "move absolute position center")))

(define (sway-move-container-cursor)
  "Moves the focused container to be centered on the cursor."
  (sway-dispatch-command
   (string-append "move position cursor")))

(define (sway-move-container-to-mark mark)
  "Moves the focused container to the specified mark."
  (sway-dispatch-command
   (string-append "move container to mark " mark)))

(define SWAY-WORKSPACE-PREVIOUS "prev")
(define SWAY-WORKSPACE-NEXT "next")
(define SWAY-WORKSPACE-CURRENT "current")
(define SWAY-WORKSPACE-PREVIOUS-ON-OUTPUT "prev_on_output")
(define SWAY-WORKSPACE-NEXT-ON-OUTPUT "next_on_output")
(define SWAY-WORKSPACE-BACK-AND-FORTH "back_and_forth")

(define (sway-move-container-to-workspace workspace)
  "Moves the focused container to the workspace name
  parameters:
    - workspace: workspace name, `SWAY-WORKSPACE-PREVIOUS`, `SWAY-WORKSPACE-NEXT`, `SWAY-WORKSPACE-CURRENT`,
                 `SWAY-WORKSPACE-PREVIOUS-ON-OUTPUT`, `SWAY-WORKSPACE-NEXT-ON-OUTPUT`, `SWAY-WORKSPACE-BACK-AND-FORTH`"
  (sway-dispatch-command
   (string-append "move container to workspace \"" workspace "\"")))

(define SWAY-OUTPUT-CURRENT "current")
(define SWAY-OUTPUT-UP "up")
(define SWAY-OUTPUT-RIGHT "right")
(define SWAY-OUTPUT-DOWN "down")
(define SWAY-OUTPUT-LEFT "left")

(define (sway-move-container-to-output output)
  "Moves the focused container to the specified output id|name|direction.
  parameters:
    - workspace: output name, output id, `SWAY-OUTPUT-CURRENT`, `SWAY-OUTPUT-UP`,
                 `SWAY-OUTPUT-RIGHT`, `SWAY-OUTPUT-DOWN`, `SWAY-OUTPUT-LEFT`"
  (sway-dispatch-command
   (string-append "move container to output " (or (and (number? output)
                                                          (number->string output))
                                                     output))))

(define (sway-move-container-to-scratchpad)
  "Moves the focused container to the scratchpad."
  (sway-dispatch-command
   (string-append "move container to scratchpad")))

(define (sway-move-workspace-to-output output)
  "Moves the focused workspace to the specified output id|name|direction.
  parameters:
    - workspace: output name, output id, `SWAY-OUTPUT-CURRENT`, `SWAY-OUTPUT-UP`,
                 `SWAY-OUTPUT-RIGHT`, `SWAY-OUTPUT-DOWN`, `SWAY-OUTPUT-LEFT`"
  (sway-dispatch-command
   (string-append "move workspace to output " (or (and (number? output)
                                                          (number->string output))
                                                     output))))

(define* (sway-nop #:key (comment ""))
  "A no operation command that can be used to override default behaviour.
  parameters:
    - comment: optional comment argument is ignored, but logged for debugging purposes."
  (sway-dispatch-command
   (string-append "nop " comment)))

(define (sway-reload)
  "Reloads the sway config file and applies any changes."
  (sway-dispatch-command
   (string-append "reload")))

(define (sway-rename-workspace old-name new-name)
  "Rename workspace <old_name> to the <new_name>
  parameters:
    - old-name: old workspace name (str).
    - new-name: new workspace name (str)."
  (sway-dispatch-command
   (string-append "rename workspace " old-name " to " new-name)))

(define (sway-rename-current-workspace new-name)
  "Rename current workspace to the <new_name>
  parameters:
    - new-name: new workspace name (str)."
  (sway-dispatch-command
   (string-append "rename workspace to " new-name)))

(define SWAY-RESIZE-TYPE-SHRINK "shrink")
(define SWAY-RESIZE-TYPE-GROW-WIDTH "grow height")
(define SWAY-RESIZE-TYPE-GROW-HEIGHT "grow width")

(define SWAY-SIZE-UNIT-PX "px")
(define SWAY-SIZE-UNIT-PPT "ppt")

(define* (sway-resize type amount #:key unit)
 "Resizes the currently focused container by amount, specified in pixels or percentage points.
If the units are omitted, floating containers are resized in px and tiled containers by ppt.
  parameters:
    - type: `SWAY-RESIZE-TYPE-SHRINK`, `SWAY-RESIZE-TYPE-GROW-WIDTH`, `SWAY-RESIZE-TYPE-GROW-HEIGHT`
    - amount: number
    - unit: `SWAY-SIZE-UNIT-PX`, `SWAY-SIZE-UNIT-PPT`"
  (sway-dispatch-command
   (string-append "resize " type " "
                  (number->string amount)
                  (if unit (string-append " " unit) ""))))

(define* (sway-resize-height amount #:key unit)
 "Sets the height of the container to height, specified in pixels or percentage points."
  (sway-dispatch-command
   (string-append "resize height " (number->string amount)
                  (if unit (string-append " " unit) ""))))

(define* (sway-resize-width amount #:key unit)
 "Sets the width of the container to width, specified in pixels or percentage points."
  (sway-dispatch-command
   (string-append "resize width " (number->string amount)
                  (if unit (string-append " " unit) ""))))

(define (sway-show-scratchpad)
 "Shows a window from the scratchpad."
  (sway-dispatch-command
   (string-append "scratchpad show")))

(define (sway-shortcuts-inhibitor flag)
 "Enables or disables the ability of clients to inhibit keyboard shortcuts for a view."
  (sway-dispatch-command
   (string-append "scratchpad " (if flag "enabled" "disabled"))))

(define SWAY-SPLIT-VERTICAL "vertical")
(define SWAY-SPLIT-HORIZONTAL "horizontal")
(define SWAY-SPLIT-NONE "none")
(define SWAY-SPLIT-TOGGLE "toggle")

(define (sway-split-container option)
  "Splits the current container, vertically or horizontally. When none is specified,
   the effect of a previous split is undone.
  parameters:
    - option: `SWAY-SPLIT-VERTICAL`, `SWAY-SPLIT-HORIZONTAL`, `SWAY-SPLIT-NONE`, `SWAY-SPLIT-TOGGLE`"
  (sway-dispatch-command
   (string-append "split " option)))

(define SWAY-STICKY-ENABLE "enable")
(define SWAY-STICKY-DISABLE "disable")
(define SWAY-STICKY-TOGGLE "toggle")

(define (sway-sticky flag)
  "Sticks a floating window to the current output so that it shows up on all workspaces.
  parameters:
    - flag: `SWAY-STICKY-ENABLE`, `SWAY-STICKY-DISABLE`, `SWAY-STICKY-TOGGLE`"
  (sway-dispatch-command
   (string-append "sticky " (cond
                               ((equal? #t flag) SWAY-STICKY-ENABLE)
                               ((equal? #f flag) SWAY-STICKY-DISABLE)
                               (else flag)))))

(define SWAY-SWAY-CONTAINER-TYPE-ID "id")
(define SWAY-SWAY-CONTAINER-TYPE-CONTAINER-ID "con_id")
(define SWAY-SWAY-CONTAINER-TYPE-MARK "mark")

(define (sway-swap-container type arg)
  "Swaps the position, geometry, and fullscreen status of focused container with another
   target container.
  parameters:
    - type: `SWAY-SWAY-CONTAINER-TYPE-ID`, `SWAY-SWAY-CONTAINER-TYPE-CONTAINER-ID`,
			`SWAY-SWAY-CONTAINER-TYPE-MARK`
    - arg: argument passed (based on selected type)"
  (sway-dispatch-command
   (string-append "swap container with " type " "
                  (if (number? arg)
                      (number->string arg)
                      arg))))

(define (sway-title-format format)
  "Sets the format of window titles.
  parameters:
    - format: a string that can use some placehodlers to display windows title format
        %title - The title supplied by the window
        %app_id  -  The  wayland app ID (applicable to wayland windows only)
        %class - The X11  classname  (applicable  to  xwayland windows only)
        %instance  -  The X11 instance (applicable to xwayland windows only)
        %shell - The protocol the window is  using  (typically xwayland or xdg_shell)"

  (sway-dispatch-command
   (string-append "title_format " format)))

(define (sway-assign-to-workspace criteria workspace)
  "Assigns views matching criteria to workspace.
  parameters:
    - criteria: a criteria string, use (sway-criteria) to build a one
    - workspace: workspace name"
  (sway-dispatch-command
   (string-append "assign " criteria " workspace " (or (and (number? workspace)
                                                          (number->string workspace))
                                                     workspace))))

(define (sway-assign-to-output criteria output)
  "Assigns views matching criteria to output.
  parameters:
    - criteria: a criteria string, use (sway-criteria) to build a one
    - output: output name"
  (sway-dispatch-command
   (string-append "assign " criteria " output " output)))

(define* (sway-bindsym key command #:key whole-window border exclude-titlebar
                       release locked to-code input-device no-warn
                       no-repeat inhibited group)
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
  (sway-dispatch-command
   (string-append "bindsym "
                  (string-join
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
                   " ")
                  key " " command)))

(define* (sway-bindcode code command #:key whole-window border exclude-titlebar
                       release locked input-device no-warn
                       no-repeat inhibited group)
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
  (sway-dispatch-command
   (string-append "bindcode "
                  (string-join
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
                   " ")
                  code " " command)))

(define* (sway-bindswitch switch state command #:key locked no-warn reload)
  "Binds <switch> to execute the sway command command on state changes.
  parameters:
    - switch: Supported switches are lid (laptop lid) and tablet (tablet mode) switches.
    - state: valid values are on, off and toggle.
    - command: a string sway command to execute option receiving the key
    - locked: run command also when screen locking program is active
    - no-warn: silence sway warning when overriding a keybinding
    - reload: the binding should also be executed when the config is reloaded."
  (sway-dispatch-command
   (string-append "bindswitch "
                  (string-join
                   (filter (lambda (x) (> (string-length x) 0))
                           (list
                            (if locked "--locked" "")
                            (if no-warn "--no-warn" "")
                            (if reload "--reload" "")))
                   " ")
                  switch ":" state " " command)))

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

(define* (sway-unbindswitch switch state)
  "Removes a binding for when <switch> changes to <state>.
  parameters:
    - switch: Supported switches are lid (laptop lid) and tablet (tablet mode) switches.
    - state: valid values are on, off and toggle."
  (sway-dispatch-command
   (string-append "unbindswitch " switch ":" state)))

;; TODO
;;        unbindgesture   [--exact]   [--input-device=<device>]   <gesture>[:<fin‐
;;        gers>][:directions]
;;            Removes a binding for the specified gesture, fingers and  directions
;;            combination.

(define* (sway-unbindsym key #:key whole-window border exclude-titlebar
                       release locked to-code input-device)
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
  (sway-dispatch-command
   (string-append "unbindsym "
                  (string-join
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
                   " ")
                  key)))

(define* (sway-unbindcode code #:key whole-window border exclude-titlebar
                       release locked input-device)
  "Removes the binding for code that was previously bound with the given flags.
  parameters:
    - key: a string that represents the key to bind
    - whole-window: affect the region in which the mouse bindings can be triggered.
    - border: affect the region in which the mouse bindings can be triggered.
    - exclude-titlebar: affect the region in which the mouse bindings can be triggered.
    - release: command is executed when the key combo is released.
    - locked: run command also when screen locking program is active
    - input-device: the binding will only be executed for specified input device"
  (sway-dispatch-command
   (string-append "unbindcode "
                  (string-join
                   (filter (lambda (x) (> (string-length x) 0))
                           (list
                            (if whole-window "--whole-window" "")
                            (if border "--border" "")
                            (if exclude-titlebar "--exclude-titlebar" "")
                            (if exclude-titlebar "--exclude-titlebar" "")
                            (if release "--release" "")
                            (if locked "--locked" "")
                            (if input-device (string-append "--input-device=" input-device) "")))
                   " ")
                  code)))

(define* (sway-unmark identifier)
  "remove identifier from the list of current marks on a window.
  Parameters:
   - mark: string mark."
  (sway-dispatch-command
   (string-append "unmark " identifier)))

(define SWAY-URGENT-ENABLE "enable")
(define SWAY-URGENT-DISABLE "disable")
(define SWAY-URGENT-ALLOW "allow")
(define SWAY-URGENT-DENY "deny")

(define* (sway-urgent flag)
  "Using enable or disable manually sets or unsets the window's urgent state.
  Parameters:
   - flag: `SWAY-URGENT-ENABLE`, `SWAY-URGENT-DISABLE`,
           `SWAY-URGENT-ALLOW`, `SWAY-URGENT-DENY`"
  (sway-dispatch-command
   (string-append "urgent " flag)))

;; The meaning of each color is:
;; border: The border around the title bar.
;; background: The background of the title bar.
;; text: The text color of the title bar.
;; indicator: The  color  used  to  indicate  where a new view will open.
;; child_border: The border around the view itself.
(define (sway-client-background color)
  "This command is ignored and is only present for i3 compatibility.
  parameters:
    - color: color code to be used (str)"
  (sway-dispatch-command
    (string-append "client.background " color)))

(define* (sway-client-focused-color border-color background-color text-color
                                   #:key indictor-color child-border-color)
  "Configures the color of window borders and title bars of the window that has focus.
  parameters:
    - border-color: color code to be used for border (str)
    - background-color: color code to be used for background (str)
    - text-color: color code to be used for text (str)
    - indictor-color: color code to be used for indicator (str)
    - child-border-color: color code to be used for child border (str)"
  (sway-dispatch-command
   (string-append "client.focused " border-color " " background-color " " text-color
                  (if indictor-color (string-append " " indictor-color) "")
                  (if child-border-color (string-append " " child-border-color) ""))))

(define* (sway-client-focused-inactive-color border-color background-color text-color
                                   #:key indictor-color child-border-color)
  "Configures the color of window borders and title bars of the most
   recently focused view within a container which  is  not focused.
  parameters:
    - border-color: color code to be used for border (str)
    - background-color: color code to be used for background (str)
    - text-color: color code to be used for text (str)
    - indictor-color: color code to be used for indicator (str)
    - child-border-color: color code to be used for child border (str)"
  (sway-dispatch-command
   (string-append "client.focused_inactive " border-color " " background-color " " text-color
                  (if indictor-color (string-append " " indictor-color) "")
                  (if child-border-color (string-append " " child-border-color) ""))))

(define (sway-client-focused-tab-title-color border-color background-color text-color)
  "Configures the color of window borders and title bars of a
   view that has focused descendant container.
  parameters:
    - border-color: color code to be used for border (str)
    - background-color: color code to be used for background (str)
    - text-color: color code to be used for text (str)
    - indictor-color: color code to be used for indicator (str)
    - child-border-color: color code to be used for child border (str)"
  (sway-dispatch-command
   (string-append "client.focused_tab_title " border-color " " background-color " " text-color)))

(define* (sway-client-placeholder-color border-color background-color text-color
                                   #:key indictor-color child-border-color)
  "Ignored (present for i3 compatibility).
  parameters:
    - border-color: color code to be used for border (str)
    - background-color: color code to be used for background (str)
    - text-color: color code to be used for text (str)
    - indictor-color: color code to be used for indicator (str)
    - child-border-color: color code to be used for child border (str)"
  (sway-dispatch-command
   (string-append "client.placeholder " border-color " " background-color " " text-color
                  (if indictor-color (string-append " " indictor-color) "")
                  (if child-border-color (string-append " " child-border-color) ""))))

(define* (sway-client-unfocused-color border-color background-color text-color
                                   #:key indictor-color child-border-color)
  "Configures the color of window borders and title bars of a
   view that does not have focus.
  parameters:
    - border-color: color code to be used for border (str)
    - background-color: color code to be used for background (str)
    - text-color: color code to be used for text (str)
    - indictor-color: color code to be used for indicator (str)
    - child-border-color: color code to be used for child border (str)"
  (sway-dispatch-command
   (string-append "client.unfocused " border-color " " background-color " " text-color
                  (if indictor-color (string-append " " indictor-color) "")
                  (if child-border-color (string-append " " child-border-color) ""))))

(define* (sway-client-urgent-color border-color background-color text-color
                                   #:key indictor-color child-border-color)
  "Configures the color of window borders and title bars of a
   view with an urgency hint..
  parameters:
    - border-color: color code to be used for border (str)
    - background-color: color code to be used for background (str)
    - text-color: color code to be used for text (str)
    - indictor-color: color code to be used for indicator (str)
    - child-border-color: color code to be used for child border (str)"
  (sway-dispatch-command
   (string-append "client.urgent " border-color " " background-color " " text-color
                  (if indictor-color (string-append " " indictor-color) "")
                  (if child-border-color (string-append " " child-border-color) ""))))

(define SWAY-BORDER-STYLE-NONE "none")
(define SWAY-BORDER-STYLE-NORMAL "normal")
(define SWAY-BORDER-STYLE-PIXEL "pixel")

(define* (sway-default-border-style type #:key n)
  "Set default border style for new tiled windows.
  parameters:
    - type: `SWAY-BORDER-STYLE-NONE`, `SWAY-BORDER-STYLE-NORMAL`, `SWAY-BORDER-STYLE-PIXEL`
    - n: units in case pixel is chosen (number)"
  (sway-dispatch-command
   (string-append "default_border " type " " (if n (number->string n) ""))))

(define* (sway-default-floating-border-style type #:key n)
  "Set default border style for new tiled windows.
  parameters:
    - type: color code to be used for border (str)
    - n: units in case pixel is chosen (number)"
  (sway-dispatch-command
   (string-append "default_floating_border " type " " (if n (number->string n) ""))))

(define (sway-exec command)
  "Executes shell command with sh.
  parameters:
    - command: command to be executed (str)"
  (sway-dispatch-command
   (string-append "exec " command)))

(define (sway-exec-always command)
  "Like exec, but the shell command will be executed again after reload.
  parameters:
    - command: command to be executed (str)"
  (sway-dispatch-command
   (string-append "exec_always " command)))

(define (sway-floating-maximum-size width height)
  "Specifies the maximum size of floating windows.
  parameters:
    - width: target size width (number)
    - height: target size height (number)"
  (sway-dispatch-command
   (string-append "floating_maximum_size "
                  (number->string width) " x "
                  (number->string height))))

(define (sway-floating-minimum-size width height)
  "Specifies the minimum size of floating windows.
  parameters:
    - width: target size width (number)
    - height: target size height (number)"
  (sway-dispatch-command
   (string-append "floating_minimum_size "
                  (number->string width) " x "
                  (number->string height))))

(define SWAY-FLOATING-MODIFIER-TYPE-NORMAL "normal")
(define SWAY-FLOATING-MODIFIER-TYPE-INVERSE "inverse")

(define (sway-floating-modifier modifier type)
  "When the modifier key is held down, you may hold left click to move windows,
   and right click to resize them.
  parameters:
    - modifier: the modifier key (str)
    - type: `SWAY-FLOATING-MODIFIER-TYPE-NORMAL`, `SWAY-FLOATING-MODIFIER-TYPE-INVERSE`"
  (sway-dispatch-command
   (string-append "floating_modifier" modifier " x " type)))

(define SWAY-FOCUS-FOLLOW-MOUSE-FLAG-YES "yes")
(define SWAY-FOCUS-FOLLOW-MOUSE-FLAG-NO "no")
(define SWAY-FOCUS-FOLLOW-MOUSE-FLAG-ALWAYS "always")

(define (sway-focus-follow-mouse flag)
  "If set to yes, moving your mouse over a window will focus that window.
   If set to always, the window under the cursor will always be
   focused, even after switching between workspaces.
  parameters:
    - flag: `SWAY-FOCUS-FOLLOW-MOUSE-FLAG-YES`, `SWAY-FOCUS-FOLLOW-MOUSE-FLAG-NO`,
			`SWAY-FOCUS-FOLLOW-MOUSE-FLAG-ALWAYS`"
  (sway-dispatch-command
   (string-append "focus_follows_mouse " flag)))

(define SWAY-FOCUS-ON-WINDOW-ACTIVATION-FLAG-SMART "smart")
(define SWAY-FOCUS-ON-WINDOW-ACTIVATION-FLAG-URGENT "urgent")
(define SWAY-FOCUS-ON-WINDOW-ACTIVATION-FLAG-FOCUS "focus")
(define SWAY-FOCUS-ON-WINDOW-ACTIVATION-FLAG-NONE "none")

(define (sway-focus-on-window-activation flag)
  "This option determines what to do when a client requests window activation.
  parameters:
    - flag: `SWAY-FOCUS-ON-WINDOW-ACTIVATION-FLAG-SMART`, `SWAY-FOCUS-ON-WINDOW-ACTIVATION-FLAG-URGENT`,
			`SWAY-FOCUS-ON-WINDOW-ACTIVATION-FLAG-FOCUS`, `SWAY-FOCUS-ON-WINDOW-ACTIVATION-FLAG-NONE`"
  (sway-dispatch-command
   (string-append "focus_on_window_activation " flag)))

(define SWAY-FOCUS-WRAPPING-FLAG-YES "yes")
(define SWAY-FOCUS-WRAPPING-FLAG-NO "no")
(define SWAY-FOCUS-WRAPPING-FLAG-FORCE "force")
(define SWAY-FOCUS-WRAPPING-FLAG-WORKSPACE "workspace")

(define (sway-focus-wrapping flag)
  "This option determines what to do when a client requests window activation.
  parameters:
    - flag: `SWAY-FOCUS-WRAPPING-FLAG-YES`, `SWAY-FOCUS-WRAPPING-FLAG-NO`,
			`SWAY-FOCUS-WRAPPING-FLAG-FORCE`, `SWAY-FOCUS-WRAPPING-FLAG-WORKSPACE`"
  (sway-dispatch-command
   (string-append "focus_wrapping " flag)))

(define* (sway-font font #:key pango)
  "Sets font to use for the title bars. To enable support for pango markup,
   preface the font name with pango:
  parameters:
    - font: font name (str)
    - pango: whether to use pango or not (boolean)"
  (sway-dispatch-command
   (string-append "font " (if pango "pango:" "") font)))

(define (sway-force-display-urgency-hint timeout)
  "If an application on another workspace sets an urgency hint.
  parameters:
    - timeout: urgency timeout (number)"
  (sway-dispatch-command
   (string-append "force_display_urgency_hint " (number->string timeout))))

(define (sway-titlebar-border-thickness thickness)
  "Thickness of the titlebar border in pixels.
  parameters:
    - thickness: thickness of border (number)"
  (sway-dispatch-command
   (string-append "titlebar_border_thickness " (number->string thickness))))

(define (sway-titlebar-padding horizontal vertical)
  "Padding of the text in the titlebar.
  parameters:
    - horizontal: horizontal padding (number)
    - vertical: vertical padding (number)"
  (sway-dispatch-command
   (string-append "titlebar_padding " (number->string horizontal)
                  " " (number->string vertical))))

;; TODO: it should be possible to get commands as strings instead of dispatching them immediately
(define (sway-for-window criteria commands)
  "Whenever a window that matches criteria appears, run list of commands.
  parameters:
    - criteria: a criteria string, use (sway-criteria) to build a one
    - command: list of commands to execute (string)"
  (sway-dispatch-command
   (string-append "for_window " criteria " " commands)))

(define (sway-default-gaps option amount)
  "Sets default amount pixels of inner or outer gap.
  parameters:
    - option: `SWAY-GAPS-OPTION-INNER`, `SWAY-GAPS-OPTION-OUTER`, `SWAY-GAPS-OPTION-HORIZONTAL`,
              `SWAY-GAPS-OPTION-VERTICAL`, `SWAY-GAPS-OPTION-TOP`, `SWAY-GAPS-OPTION-RIGHT`,
              `SWAY-GAPS-OPTION-BOTTOM`, `SWAY-GAPS-OPTION-LEFT`
    - amount: amount of gap (number)"
  (sway-dispatch-command
   (string-append "gaps " option " " (number->string amount))))

(define SWAY-EDGE-BORDER-TYPE-NONE "none")
(define SWAY-EDGE-BORDER-TYPE-VERTICAL "vertical")
(define SWAY-EDGE-BORDER-TYPE-HORIZONTAL "horizontal")
(define SWAY-EDGE-BORDER-TYPE-BOTH "both")
(define SWAY-EDGE-BORDER-TYPE-SMART "smart")
(define SWAY-EDGE-BORDER-TYPE-SMART-NO-GAPS "smart_no_gaps")

(define* (sway-hide-edge-borders type #:key i3)
  "Hides window borders adjacent to the screen edges.
  parameters:
    - type: `SWAY-EDGE-BORDER-TYPE-NONE`, `SWAY-EDGE-BORDER-TYPE-VERTICAL`, `SWAY-EDGE-BORDER-TYPE-HORIZONTAL`,
              `SWAY-EDGE-BORDER-TYPE-BOTH`, `SWAY-EDGE-BORDER-TYPE-SMART`, `SWAY-EDGE-BORDER-TYPE-SMART-NO-GAPS`
    - i3: enables i3-compatible behavior to hide the title bar on tabbed and stacked containers with one child"
  (sway-dispatch-command
   (string-append "hide_edge_borders " (if i3 "--i3 " "") type)))

(define (sway-input device subcommands)
  "For details on input subcommands, see sway-input(5).
  parameters:
    - device: the name of the target device
    - subcommands: list of commands to execute (string)"
  (sway-dispatch-command
   (string-append "input " device " " subcommands)))

(define (sway-seat seat subcommands)
  "For details on input subcommands, see sway-input(5).
  parameters:
    - seat: the name of the seat device
    - subcommands: list of commands to execute (string)"
  (sway-dispatch-command
   (string-append "seat " seat " " subcommands)))

(define (sway-kill)
  "Kills (closes) the currently focused container and all of its children."
  (sway-dispatch-command
   (string-append "kill")))

(define SWAY-SMART-BORDERS-ON "on")
(define SWAY-SMART-BORDERS-OFF "off")
(define SWAY-SMART-BORDERS-NO-GAPS "no_gaps")

(define (sway-smart-borders flag)
  "If smart_borders are on, borders will only be enabled if the
   workspace has more than one visible child.
  parameters:
    - flag: `SWAY-SMART-BORDERS-ON`, `SWAY-SMART-BORDERS-OFF`, `SWAY-SMART-BORDERS-NO-GAPS`"
  (sway-dispatch-command
   (string-append "smart_borders " (cond
                   ((equal? flag #t) SWAY-SMART-BORDERS-ON)
                   ((equal? flag #f) SWAY-SMART-BORDERS-OFF)
                   (else flag)))))

(define SWAY-SMART-GAPS-ON "on")
(define SWAY-SMART-GAPS-OFF "off")
(define SWAY-SMART-GAPS-TOGGLE "toggle")
(define SWAY-SMART-GAPS-INVERSE-OUTER "inverse_outer")

(define (sway-smart-gaps flag)
  "If smart_gaps are on gaps will only be enabled if a
   workspace has more than one child.
  parameters:
    - flag: `SWAY-SMART-GAPS-ON`, `SWAY-SMART-GAPS-OFF`,
			`SWAY-SMART-GAPS-TOGGLE`, `SWAY-SMART-GAPS-INVERSE-OUTER`"
  (sway-dispatch-command
   (string-append "smart_gaps " (cond
                   ((equal? flag #t) SWAY-SMART-GAPS-ON)
                   ((equal? flag #f) SWAY-SMART-GAPS-OFF)
                   (else flag)))))

;;        mark --add|--replace [--toggle] <identifier>
;;            Marks are arbitrary labels that can be used to identify certain win‐
;;            dows and then jump to them at a later time. Each identifier can only
;;            be set on a single window at a time since they act as a unique iden‐
;;            tifier.  By default, mark sets identifier as the only mark on a win‐
;;            dow. --add will instead add identifier to the list of current  marks
;;            for  that  window. If --toggle is specified mark will remove identi‐
;;            fier if it is already marked.

(define (sway-mode mode)
  "Switches to the specified mode. The default mode is default.
  parameters:
    - mode: name of the mode (str)"
  (sway-dispatch-command
   (string-append "mode \"" mode "\"")))

(define (sway-mode-subcommand mode subcommand)
  "The only valid mode-subcommands are bindsym, bindcode, bindswitch, and set.
  parameters:
    - mode: name of the mode (str)
    - subcommand: list of subcommands (str)"
  (sway-dispatch-command
   (string-append "mode \"" mode "\" " subcommand)))

(define SWAY-MOUSE-WARPING-OUTPUT "output")
(define SWAY-MOUSE-WARPING-CONTAINER "container")
(define SWAY-MOUSE-WARPING-NONE "none")

(define (sway-mouse-warping mode)
  "If output is specified, the mouse will be moved to new outputs as you move focus between them.
   If container is specified, the mouse will be moved to the middle of the container on switch.
  parameters:
    - mode: `SWAY-MOUSE-WARPING-OUTPUT`, `SWAY-MOUSE-WARPING-CONTAINER`, `SWAY-MOUSE-WARPING-NONE`"
  (sway-dispatch-command
   (string-append "mouse_warping " mode)))

(define (sway-no-focus criteria)
  "Prevents windows matching <criteria> from being focused automatically when they're created.
  parameters:
    - criteria: a criteria string, use (sway-criteria) to build a one"
  (sway-dispatch-command
   (string-append "no_focus " criteria)))

(define (sway-output output subcommands)
  "For details on output subcommands, see sway-output(5).
  parameters:
    - output: name of the output (str)
    - subcommand: list of subcommands (str)"
  (sway-dispatch-command
   (string-append "output " output " " subcommands)))

(define SWAY-POPUP-TYPE-OUTPUTSMART "outputsmart")
(define SWAY-POPUP-TYPE-IGNORE "ignore")
(define SWAY-POPUP-TYPE-LEAVE-FULLSCREEN "leave_fullscreen")

(define (sway-popup-during-fullscreen type)
  "Determines what to do when a fullscreen view opens a dialog.
  parameters:
    - type: `SWAY-POPUP-TYPE-OUTPUTSMART`, `SWAY-POPUP-TYPE-IGNORE`, `SWAY-POPUP-TYPE-LEAVE-FULLSCREEN`"
  (sway-dispatch-command
   (string-append "popup_during_fullscreen " type)))

(define SWAY-PRIMARY-SELECTION-ENABLED "enabled")
(define SWAY-PRIMARY-SELECTION-DISABLED "disabled")

(define (sway-primary-selection type)
  "Enable or disable the primary selection clipboard. May only be configured at launch. Default is enabled.
  parameters:
    - type: `SWAY-PRIMARY-SELECTION-ENABLED`, `SWAY-PRIMARY-SELECTION-DISABLED`"
  (sway-dispatch-command
   (string-append "primary_selection " (cond
                   ((equal? type #t) SWAY-PRIMARY-SELECTION-ENABLED)
                   ((equal? type #f) SWAY-PRIMARY-SELECTION-DISABLED)
                   (else type)))))

(define SWAY-SHOW-MARKS-YES "yes")
(define SWAY-SHOW-MARKS-NO "no")

(define (sway-show-marks flag)
  "If show_marks is yes, marks will be displayed in the window borders.
  parameters:
    - flag: `SWAY-SHOW-MARKS-YES`, `SWAY-SHOW-MARKS-NO`"
  (sway-dispatch-command
   (string-append "show_marks " (cond
                   ((equal? flag #t) SWAY-SHOW-MARKS-YES)
                   ((equal? flag #f) SWAY-SHOW-MARKS-NO)
                   (else flag)))))

(define SWAY-OPACITY-SET "set")
(define SWAY-OPACITY-PLUS "plus")
(define SWAY-OPACITY-MINUS "minus")

(define (sway-opacity type value)
  "Adjusts the opacity of the window between 0 (completely transparent) and 1 (completely opaque)
  parameters:
    - type: `SWAY-OPACITY-SET`, `SWAY-OPACITY-PLUS`, `SWAY-OPACITY-MINUS`
	- value: opacity value (number) should be between 0 and 1"
  (sway-dispatch-command
   (string-append "opacity " type " " (number->string value))))

(define SWAY-TILING-DRAG-ENABLE "enable")
(define SWAY-TILING-DRAG-DISABLE "disable")
(define SWAY-TILING-DRAG-TOGGLE "toggle")

(define (sway-tiling-drag flag)
  "Sets whether or not tiling containers can be dragged with the mouse.
  parameters:
    - flag: `SWAY-TILING-DRAG-ENABLE`, `SWAY-TILING-DRAG-DISABLE`, `SWAY-TILING-DRAG-TOGGLE`"
  (sway-dispatch-command
   (string-append "tiling_drag " (cond
                   ((equal? flag #t) SWAY-TILING-DRAG-ENABLE)
                   ((equal? flag #f) SWAY-TILING-DRAG-DISABLE)
                   (else flag)))))

(define (sway-tiling-drag-threshold threshold)
  "Sets whether or not tiling containers can be dragged with the mouse.
  parameters:
    - threshold: threshold value (number)"
  (sway-dispatch-command
   (string-append "tiling_drag_threshold " (number->string threshold))))

(define SWAY-TILING-ALIGN-LEFT "left")
(define SWAY-TILING-ALIGN-CENTER "center")
(define SWAY-TILING-ALIGN-RIGHT "right")

(define (sway-tiling-align type)
  "Sets the title alignment.
  parameters:
    - type: `SWAY-TILING-ALIGN-LEFT`, `SWAY-TILING-ALIGN-CENTER`, `SWAY-TILING-ALIGN-RIGHT`"
  (sway-dispatch-command
   (string-append "title_align " type)))

(define* (sway-switch-workspace-id id #:key auto-back-and-forth)
  "switch to the workspace with the provided id.
  parameters:
	- id: workspace id (number)
    - auto-back-and-forth: enable/disable auto back and forth"
  (sway-dispatch-command
   (string-append "workspace number "
                  (unless auto-back-and-forth "--no-auto-back-and-forth ")
                  (number->string id))))

(define* (sway-switch-workspace workspace #:key auto-back-and-forth)
  "switch to the workspace with the provided name.
  parameters:
	- workspace: workspace name (str)
    - auto-back-and-forth: enable/disable auto back and forth"
  (sway-dispatch-command
   (string-append "workspace "
                  (unless auto-back-and-forth "--no-auto-back-and-forth ")
                  workspace)))

(define (sway-switch-workspace-on-output workspace output)
  "assigns workspace to output.
  parameters:
	- workspace: workspace name (str)
    - output: output name"
  (sway-dispatch-command
   (string-append "workspace " workspace " output " output)))

(define SWAY-WORKSPACE-AUTO-BACK-AND-FORTH-OPTION-YES "yes")
(define SWAY-WORKSPACE-AUTO-BACK-AND-FORTH-OPTION-NO "no")

(define (sway-workspace-auto-back-and-forth option)
  "When yes, repeating a workspace switch command will switch back to the prior workspace.
  parameters:
	- option: `SWAY-WORKSPACE-AUTO-BACK-AND-FORTH-OPTION-YES`, `SWAY-WORKSPACE-AUTO-BACK-AND-FORTH-OPTION-NO`"
  (sway-dispatch-command
   (string-append "workspace_auto_back_and_forth "
                  (cond
                   ((equal? option #t) SWAY-WORKSPACE-AUTO-BACK-AND-FORTH-OPTION-YES)
                   ((equal? option #f) SWAY-WORKSPACE-AUTO-BACK-AND-FORTH-OPTION-NO)
                   (else option)))))

(define (sway-workspace-gaps workspace option amount)
  "Specifies that workspace name should have the given gaps settings when it is created.
  This command does not affect existing workspaces. To alter the gaps of an existing workspace,
  use the `sway-gaps` command.
  parameters:
	- workspace: workspace name (str)
    - option: `SWAY-GAPS-OPTION-INNER`, `SWAY-GAPS-OPTION-OUTER`, `SWAY-GAPS-OPTION-HORIZONTAL`,
              `SWAY-GAPS-OPTION-VERTICAL`, `SWAY-GAPS-OPTION-TOP`, `SWAY-GAPS-OPTION-RIGHT`,
              `SWAY-GAPS-OPTION-BOTTOM`, `SWAY-GAPS-OPTION-LEFT`
	- amount: the amount of gap (number)"
  (sway-dispatch-command
   (string-append "workspace " workspace option (number->string amount))))

(define* (sway-criteria #:key app-id class con-id con-mark
                        floating id instance pid shell tiling title urgent
                        window-role window-type workspace)
  "Generate a string that contains one or more (space separated) attribute/value pairs."
  (string-append
   "["
   (string-join
    (filter (lambda (x) (> (string-length x) 0))
            (list
             (if app-id (string-append "app_id=" app-id) "")
             (if class (string-append "class=" class) "")
             (if con-id (string-append "con_id=" con-id) "")
             (if con-mark (string-append "con_mark=" con-mark) "")
             (if floating (string-append "floating=" floating) "")
             (if id (string-append "id=" id) "")
             (if instance (string-append "instance=" instance) "")
             (if pid (string-append "pid=" pid) "")
             (if shell (string-append "shell=" shell) "")
             (if tiling (string-append "tiling=" tiling) "")
             (if title (string-append "title=" title) "")
             (if urgent (string-append "urgent=" urgent) "")
             (if window-role (string-append "window_role=" window-role) "")
             (if window-type (string-append "window_type=" window-type) "")
             (if workspace (string-append "workspace=" workspace)) ""))
    " ")
   "]"))
