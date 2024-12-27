(define-module (swayipc records)
  #:use-module (oop goops)
  #:use-module (sjson)

  #:export (<sway-rect>
            scm->sway-rect
            json->sway-rect
            sway-rect-x
            sway-rect-y
            sway-rect-width
            sway-rect-height

            <sway-workspace>
            scm->sway-workspace
            json->sway-workspace
            sway-workspace-num
            sway-workspace-name
            sway-workspace-visible
            sway-workspace-focused
            sway-workspace-urgent
            sway-workspace-rect
            sway-workspace-output

            <sway-mode>
            scm->sway-mode
            json->sway-mode
            sway-mode-width
            sway-mode-height
            sway-mode-refresh
            sway-mode-picture-aspect-ratio

            <sway-output>
            scm->sway-output
            json->sway-output
            sway-output-name
            sway-output-make
            sway-output-model
            sway-output-serial
            sway-output-active
            sway-output-power
            sway-output-primary
            sway-output-focused
            sway-output-scale
            sway-output-subpixel-hinting
            sway-output-transform
            sway-output-current-workspace
            sway-output-modes
            sway-output-current-mode
            sway-output-rect

            <sway-window-property>
            scm->sway-window-property
            json->sway-window-property
            class
            instance
            title
            transient-for

            <sway-tree>
            scm->sway-tree
            json->sway-tree
            sway-tree-id
            sway-tree-name
            sway-tree-type
            sway-tree-border
            sway-tree-current-border-width
            sway-tree-layout
            sway-tree-orientation
            sway-tree-percent
            sway-tree-rect
            sway-tree-window-rect
            sway-tree-deco-rect
            sway-tree-geometry
            sway-tree-urgent
            sway-tree-sticky
            sway-tree-marks
            sway-tree-focused
            sway-tree-focus
            sway-tree-nodes
            sway-tree-floating-nodes
            sway-tree-representation
            sway-tree-fullscreen-mode
            sway-tree-app-id
            sway-tree-pid
            sway-tree-visible
            sway-tree-shell
            sway-tree-inhibit-idle
            sway-tree-idle-inhibitors
            sway-tree-window
            sway-tree-window-properties

            <sway-bar-color>
            scm->sway-bar-color
            json->sway-bar-color
            sway-bar-color-background
            sway-bar-color-status-line
            sway-bar-color-separator
            sway-bar-color-focused-background
            sway-bar-color-focused-statusline
            sway-bar-color-focused-separator
            sway-bar-color-focused-workspace-text
            sway-bar-color-focused-workspace-background
            sway-bar-color-focused-workspace-border
            sway-bar-color-active-workspace-text
            sway-bar-color-active-workspace-background
            sway-bar-color-active-workspace-border
            sway-bar-color-inactive-workspace-text
            sway-bar-color-inactive-workspace-background
            sway-bar-color-inactive-workspace-border
            sway-bar-color-urgent-workspace-text
            sway-bar-color-urgent-workspace-background
            sway-bar-color-urgent-workspace-border
            sway-bar-color-binding-mode-text
            sway-bar-color-binding-mode-background
            sway-bar-color-binding-mode-border

            <sway-bar-gap>
            scm->sway-bar-gap
            json->sway-bar-gap
            sway-bar-gap-top
            sway-bar-gap-right
            sway-bar-gap-bottom
            sway-bar-gap-left

            <sway-bar-config>
            scm->sway-bar-config
            json->sway-bar-config
            sway-bar-config-id
            sway-bar-config-mode
            sway-bar-config-position
            sway-bar-config-status-command
            sway-bar-config-font
            sway-bar-config-workspace-buttons
            sway-bar-config-workspace-min-width
            sway-bar-config-binding-mode-indicator
            sway-bar-config-verbose
            sway-bar-config-colors
            sway-bar-config-gaps
            sway-bar-config-bar-height
            sway-bar-config-status-padding
            sway-bar-config-status-edge-padding

            <sway-version>
            scm->sway-version
            json->sway-version
            sway-version-major
            sway-version-minor
            sway-version-patch
            sway-version-human-readable
            sway-version-loaded-config-file-name

            <sway-config>
            scm->sway-config
            json->sway-config
            sway-config-config

            <sway-tick>
            scm->sway-tick
            json->sway-tick
            sway-tick-success
            sway-tick-parse-error
            sway-tick-error

            <sway-sync>
            scm->sway-sync
            json->sway-sync
            sway-sync-success

            <sway-binding-state>
            scm->sway-binding-state
            json->sway-binding-state
            sway-binding-state-name

            <sway-lib-input>
            scm->sway-lib-input
            json->sway-lib-input
            sway-lib-input-send-events
            sway-lib-input-tap
            sway-lib-input-tap-button-map
            sway-lib-input-tap-drag
            sway-lib-input-tap-drag-lock
            sway-lib-input-accel-speed
            sway-lib-input-accel-profile
            sway-lib-input-natural-scroll
            sway-lib-input-left-handed
            sway-lib-input-click-method
            sway-lib-input-middle-emulation
            sway-lib-input-scroll-method
            sway-lib-input-scroll-button
            sway-lib-input-scroll-button-lock
            sway-lib-input-dwt
            sway-lib-input-dwtp
            sway-lib-input-calibration-matrix

            <sway-input>
            scm->sway-input
            json->sway-input
            sway-input-identifier
            sway-input-name
            sway-input-vendor
            sway-input-product
            sway-input-type
            sway-input-xkb-active-layout-name
            sway-input-xkb-layout-names
            sway-input-scroll-factor
            sway-input-libinput

            <sway-seat>
            scm->sway-seat
            json->sway-seat
            sway-seat-name
            sway-seat-capabilities
            sway-seat-focus
            sway-seat-devices

            <sway-workspace-event>
            scm->sway-workspace-event
            json->sway-workspace-event
            sway-workspace-event-change
            sway-workspace-event-old
            sway-workspace-event-current

            <sway-output-event>
            scm->sway-output-event
            json->sway-output-event
            sway-output-event-change

            <sway-mode-event>
            scm->sway-mode-event
            json->sway-mode-event
            sway-mode-event-change
            sway-mode-event-pango-markup

            <sway-window-event>
            scm->sway-window-event
            json->sway-window-event
            sway-window-event-change
            sway-window-event-container

            <sway-binding-event>
            scm->sway-binding-event
            json->sway-binding-event
            sway-binding-event-change
            sway-binding-event-binding

            <sway-binding-event-binding>
            scm->sway-binding-event-binding
            json->sway-binding-event-binding
            sway-binding-event-binding-command
            sway-binding-event-binding-event-state-mask
            sway-binding-event-binding-input-code
            sway-binding-event-binding-sybmol
            sway-binding-event-binding-input-type

            <sway-shutdown-event>
            scm->sway-shutdown-event
            json->sway-shutdown-event
            sway-shutdown-event-change

            <sway-tick-event>
            scm->sway-tick-event
            json->sway-tick-event
            sway-tick-event-first
            sway-tick-event-paylaod

            <sway-bar-state-update-event>
            scm->sway-bar-state-update-event
            json->sway-bar-state-update-event
            sway-bar-state-update-event-id
            sway-bar-state-update-event-visible-by-modifier

            <sway-input-event>
            scm->sway-input-event
            json->sway-input-event
            sway-input-event-change
            sway-input-event-input))

(define-json-type <sway-rect>
  (x)
  (y)
  (width)
  (height))

(define-json-type <sway-workspace>
  (num)
  (name)
  (visible)
  (focused)
  (urgent)
  (rect "rect" <sway-rect>)
  (output))

(define-json-type <sway-mode>
  (picture-aspect-ratio "picture_aspect_ratio")
  (refresh)
  (height)
  (width))

(define-json-type <sway-output>
  (name)
  (make)
  (model)
  (serial)
  (active)
  (power)
  (primary)
  (focused)
  (scale)
  (subpixel-hinting "subpixel_hinting")
  (transform)
  (current-workspace "current_workspace")
  (modes "modes" #(<sway-mode>))
  (current-mode "current_mode" <sway-mode>)
  (rect "rect" <sway-rect>))

(define-json-type <sway-window-property>
  (class)
  (instance)
  (title)
  (transient-for "transient_for"))

(define-json-type <sway-tree>
  (id)
  (name)
  (type)
  (border)
  (current-border-width "current_border_width")
  (layout)
  (orientation)
  (percent)
  (rect "rect" <sway-rect>)
  (window-rect "window_rect" <sway-rect>)
  (deco-rect "deco_rect" <sway-rect>)
  (geometry "geometry" <sway-rect>)
  (urgent)
  (sticky)
  (marks)
  (focused)
  (focus)
  (nodes "nodes" #(<sway-tree>))
  (floating-nodes "floating_nodes" #(<sway-tree>))
  (representation)
  (fullscreen-mode "fullscreen_mode")
  (app-id "app_id")
  (pid)
  (visible)
  (shell)
  (inhibit-idle "inhibit_idle")
  (idle-inhibitors "idle_inhibitors")
  (window)
  (window-properties "window_properties" <sway-window-property>))

(define-json-type <sway-bar-color>
  (background)
  (status-line "statusline")
  (separator)
  (focused-background "focused_background")
  (focused-statusline "focused_statusline")
  (focused-separator "focused_separator")
  (focused-workspace-text "focused_workspace_text")
  (focused-workspace-background "focused_workspace_bg")
  (focused-workspace-border "focused_workspace_border")
  (active-workspace-text "active_workspace_text")
  (active-workspace-background "active_workspace_bg")
  (active-workspace-border "active_workspace_border")
  (inactive-workspace-text "inactive_workspace_text")
  (inactive-workspace-background "inactive_workspace_bg")
  (inactive-workspace-border "inactive_workspace_border")
  (urgent-workspace-text "urgent_workspace_text")
  (urgent-workspace-background "urgent_workspace_bg")
  (urgent-workspace-border "urgent_workspace_border")
  (binding-mode-text "binding_mode_text")
  (binding-mode-background "binding_mode_bg")
  (binding-mode-border "binding_mode_border"))

(define-json-type <sway-bar-gap>
  (top)
  (right)
  (bottom)
  (left))

(define-json-type <sway-bar-config>
  (id)
  (mode)
  (position)
  (status-command "status_command")
  (font)
  (workspace-buttons "workspace_buttons")
  (workspace-min-width "workspace_min_width")
  (binding-mode-indicator)
  (verbose)
  (colors "colors" <sway-bar-color>)
  (gaps "gaps" <sway-bar-gap>)
  (bar-height "bar_height")
  (status-padding "status_padding")
  (status-edge-padding "status_edge_padding"))

(define-json-type <sway-version>
  (major)
  (minor)
  (patch)
  (human-readable "human_readable")
  (loaded-config-file-name "loaded_config_file_name"))

(define-json-type <sway-config>
  (config))

(define-json-type <sway-tick>
  (success)
  (parse-error "parse_error")
  (error))

(define-json-type <sway-sync>
  (success))

(define-json-type <sway-binding-state>
  (name))

(define-json-type <sway-lib-input>
  (send-events "send_events")
  (tap)
  (tap-button-map "tap_button_map")
  (tap-drag "tap_drag")
  (tap-drag-lock "tap_drag_lock")
  (accel-speed "accel_speed")
  (accel-profile "accel_profile")
  (natural-scroll "natural_scroll")
  (left-handed "left_handed")
  (click-method "click_method")
  (middle-emulation "middle_emulation")
  (scroll-method "scroll_method")
  (scroll-button "scroll_button")
  (scroll-button-lock "scroll_button_lock")
  (dwt)
  (dwtp)
  (calibration-matrix "calibration_matrix"))

(define-json-type <sway-input>
  (identifier)
  (name)
  (vendor)
  (product)
  (type)
  (xkb-active-layout-name "xkb_active_layout_name")
  (xkb-layout-names "xkb_layout_names")
  (scroll-factor "scroll_factor")
  (libinput <sway-lib-input>))

(define-json-type <sway-seat>
  (name)
  (capabilities)
  (focus)
  (devices "devices" #(<sway-input>)))

(define-json-type <sway-workspace-event>
  (change)
  (old "old" <sway-tree>)
  (current "current" <sway-tree>))

(define-json-type <sway-output-event>
  (change))

(define-json-type <sway-mode-event>
  (change)
  (pango-markup "pango_markup"))

(define-json-type <sway-window-event>
  (change)
  (container "container" <sway-tree))

(define-json-type <sway-binding-event>
  (change)
  (binding "binding" <sway-binding-event-binding))

(define-json-type <sway-binding-event-binding>
  (command)
  (event-state-mask "event_state_mask")
  (input-code "input_code")
  (sybmol)
  (input-type "input_type"))

(define-json-type <sway-shutdown-event>
  (change))

(define-json-type <sway-tick-event>
  (first)
  (paylaod))

(define-json-type <sway-bar-state-update-event>
  (id)
  (visible-by-modifier "visible_by_modifier"))

(define-json-type <sway-input-event>
  (change)
  (input "input" <sway-lib-input>))
