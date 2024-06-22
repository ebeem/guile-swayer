(use-modules (modules kbd)
             (modules general)
             (swayipc info)
             (ice-9 popen)
             (srfi srfi-18)
             (ice-9 textual-ports))

(define (exec command)
  "execute given shell command"
  (format #t "running: ~a\n" command)
  (thread-start! (make-thread (lambda () (system command)))))

(define (custom-sway-keybinding-translator key)
  "Translates keybindings, passing kbd function will enable emacs
   like key chords. The default implementation doesn't modify passed keybindings"
  (kbd key))

;; get focused workspace from a list of workspaces
(define* (focused-output-name #:optional (workspaces (sway-get-workspaces)))
  (cond
    ((null? workspaces) #f)
    ((equal? #t (sway-workspace-focused (car workspaces)))
     (sway-workspace-output (car workspaces)))
    (else (focused-output-name (cdr workspaces)))))

(define (keybindings-init)
  (kbd-init)

  (general-configure #:keybinding-translator custom-sway-keybinding-translator)
  (general-init)

  ;; define root keybindings
  (general-define-keys
   ;; media-keys
   `("XF86AudioLowerVolume" (exec "pactl set-sink-volume @DEFAULT_SINK@ -5%") #:wk "Decrease Volume")
   `("XF86AudioRaiseVolume" (exec "pactl set-sink-volume @DEFAULT_SINK@ +5%") #:wk "Increase Volume")
   `("s-[" (exec "pactl set-sink-volume @DEFAULT_SINK@ -5%") #:wk "Decrease Volume")
   `("s-]" (exec "pactl set-sink-volume @DEFAULT_SINK@ +5%") #:wk "Increase Volume")
   `("XF86AudioMute" (exec "pactl set-sink-mute @DEFAULT_SINK@ toggle") #:wk "Toggle Mute")
   `("XF86AudioNext" (exec "mpc next") #:wk "Next Song")
   `("XF86AudioPrev" (exec "mpc prev") #:wk "Previous Song")
   `("XF86AudioPlay" (exec "mpc toggle") #:wk "Toggle Player")

   ;; brightness-keys
   `("XF86MonBrightnessUp" (exec "brightnessctl set +10%") #:wk "Increase Brightness")
   `("XF86MonBrightnessDown" (exec "brightnessctl set 10%-") #:wk "Decrease Brightness")

   ;; window and group management
   `("s-f" (sway-fullscreen SWAY-FULLSCREEN-TOGGLE) #:wk "Toggle Fullscreen")

   ;; move focus
   `("s-h" (sway-focus-container SWAY-DIRECTION-LEFT) #:wk "Focus Container Left")
   `("s-j" (sway-focus-container SWAY-DIRECTION-DOWN) #:wk "Focus Container Down")
   `("s-k" (sway-focus-container SWAY-DIRECTION-UP) #:wk "Focus Container Up")
   `("s-l" (sway-focus-container SWAY-DIRECTION-RIGHT) #:wk "Focus Container Right")

   ;; move containers
   `("s-S-h" (sway-move-container SWAY-DIRECTION-LEFT) #:wk "Move Container Left")
   `("s-S-j" (sway-move-container SWAY-DIRECTION-DOWN) #:wk "Move Container Down")
   `("s-S-k" (sway-move-container SWAY-DIRECTION-UP) #:wk "Move Container Up")
   `("s-S-l" (sway-move-container SWAY-DIRECTION-RIGHT) #:wk "Move Container Right")

   ;; switch workspace
   `("s-C-h" (switch-workspace-left) #:wk "Switch Workspace Left")
   `("s-C-j" (switch-workspace-down) #:wk "Switch Workspace Down")
   `("s-C-k" (switch-workspace-up) #:wk "Switch Workspace Up")
   `("s-C-l" (switch-workspace-right) #:wk "Switch Workspace Right")

   ;; move container to workspace
   `("s-M-C-h" (move-container-to-workspace-left) #:wk "Move Container to Workspace Left")
   `("s-M-C-j" (move-container-to-workspace-down) #:wk "Move Container to Workspace Down")
   `("s-M-C-k" (move-container-to-workspace-up) #:wk "Move Container to Workspace Up")
   `("s-M-C-l" (move-container-to-workspace-right) #:wk "Move Container to Workspace Right")

   ;; Tab like cycling
   `("s-." (sway-focus-container-sibling SWAY-SIBLING-NEXT) #:wk "Cycle Tabs Next")
   `("s-," (sway-focus-container-sibling SWAY-SIBLING-PREV) #:wk "Cycle Tabs Previous")

   `("s-w" (sway-kill) #:wk "Kill Window")
   `("s-Return" (exec "alacritty") #:wk "Spawn Terminal")
   `("M-s-Space" (exec "~/.bin/switch-keyboard-layout") #:wk "Switch Keyboard Layout")
   `("C-s-Space" (exec "rofi -show drun")) #:wk "Application Launcher")

  ;; define leader keymap
  (general-define-keys
   #:prefix "s-Space" #:wk "Leader"
   `("o" (exec "rofi -show drun") #:wk "Applications")
   `("C-g" (sway-mode "default") #:wk "Abort")

   ;; rofi keymap
   `(general-define-keys
     #:prefix "r" #:wk "Rofi"
     ("p" (exec "~/.config/rofi/bin/password-manager") #:wk "Password Manager")
     ("m" (exec "rofi-mount") #:wk "Mount Drives")
     ("u" (exec "rofi-unmount") #:wk "Unmount Drives")
     ("w" (exec ".config/rofi/bin/wifi") #:wk "Wifi")
     ("b" (exec "~/.config/rofi/bin/bluetooth") #:wk "Bluetooth")
     ("f" (exec "~/.config/rofi/bin/finder") #:wk "Finder")
     ("k" (exec "~/.config/rofi/bin/keyboard-layout") #:wk "Keyboard Layouts")
     ("P" (exec "~/.config/rofi/bin/powermenu") #:wk "Power")
     ("s" (exec "~/.config/rofi/bin/sound-input") #:wk "Sound Input")
     ("S" (exec "~/.config/rofi/bin/sound-output") #:wk "Sound Output"))

   ;; screenshot keymap
   ;; flameshot is not performing well under wayland & multiple monitors
   ;; `(general-define-keys
   ;;   #:prefix "s" #:wk "Screenshot"
   ;;   ("d" (exec "export XDG_CURRENT_DESKTOP=sway && flameshot gui"))
   ;;   ("s" (exec "export XDG_CURRENT_DESKTOP=sway && flameshot screen"))
   ;;   ("f" (exec "export XDG_CURRENT_DESKTOP=sway && flameshot full"))
   ;;   ("m" (exec "export XDG_CURRENT_DESKTOP=sway && flameshot gui --last-region"))

   ;;   (general-define-keys
   ;;    #:prefix "d" #:wk "DelayScreenshot"
   ;;    ("d" (exec "export XDG_CURRENT_DESKTOP=sway && flameshot gui -d 2500"))
   ;;    ("s" (exec "export XDG_CURRENT_DESKTOP=sway && flameshot screen -d 2500"))
   ;;    ("f" (exec "export XDG_CURRENT_DESKTOP=sway && flameshot full -d 2500"))
   ;;    ("l" (exec "export XDG_CURRENT_DESKTOP=sway && flameshot gui -d 2500 --last-region"))))

   `(general-define-keys
     #:prefix "s" #:wk "Screenshot"
     ("g" (exec "slurp | grim -g - - | wl-copy") #:wk "Gui Screenshot")
     ("s" (exec (string-append "grim -o \"" (focused-output-name) "\" - | wl-copy")) #:wk "Current Screen")
     ("f" (exec "grim - | wl-copy") #:wk "All Screens")
     ("m" (exec "grim -g - - | wl-copy") #:wk "Last Region")

     (general-define-keys
      #:prefix "d" #:wk "DelayedScreenshot"
      ("g" (exec "sleep 2 && slurp | grim -g - - | wl-copy") #:wk "Gui Screenshot")
      ("s" (exec (string-append "sleep 2 && grim -o \"" (focused-output-name) "\" - | wl-copy")) #:wk "Current Screen")
      ("f" (exec "sleep 2 && grim - | wl-copy") #:wk "All Screens")
      ("m" (exec "sleep 2 && grim -g - - | wl-copy") #:wk "Last Region")))

   ;; session keymap
   `(general-define-keys
     #:prefix "q" #:wk "Session"
     ("q" (sway-exit) #:wk "Exit Sway")
     ("r" (sway-reload) #:wk "Reload Sway"))

   `(general-define-keys
     #:prefix "w" #:wk "Window"
     ("v" (sway-layout SWAY-LAYOUT-SPLITV) #:wk "Split Vertically")
     ("h" (sway-layout SWAY-LAYOUT-SPLITH) #:wk "Split Horizontally")
     ("f" (sway-fullscreen SWAY-FULLSCREEN-TOGGLE) #:wk "Fullscreen")
     ("d" (sway-layout SWAY-LAYOUT-DEFAULT) #:wk "Default Layout")
     ("t" (sway-layout SWAY-LAYOUT-TABBED) #:wk "Tabbed Layout"))))
