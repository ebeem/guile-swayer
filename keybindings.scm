(use-modules (modules kbd)
             (modules general)
             (ice-9 popen)
             (srfi srfi-18)
             (ice-9 textual-ports))

(define (exec command)
  "execute given shell command"
  (display (string-append "running " command "\n"))
  (thread-start! (make-thread (lambda () (system command)))))

(define (custom-sway-keybinding-translator key)
  "Translates keybindings, passing kbd function will enable emacs
   like key chords. The default implementation doesn't modify passed keybindings"
  (kbd key))

(define (keybindings-init)
  (kbd-init)

  (configure-sway-keybinding-translator custom-sway-keybinding-translator)
  (configure-sway-commander-path "~/.config/sway/commander")

  ;; define root keybindings
  (sway-define-keys
   ;; media-keys
   `("XF86AudioLowerVolume" (exec "pactl set-sink-volume @DEFAULT_SINK@ -5%"))
   `("XF86AudioRaiseVolume" (exec "pactl set-sink-volume @DEFAULT_SINK@ +5%"))
   `("s-[" (exec "pactl set-sink-volume @DEFAULT_SINK@ -5%"))
   `("s-]" (exec "pactl set-sink-volume @DEFAULT_SINK@ +5%"))
   `("XF86AudioMute" (exec "pactl set-sink-mute @DEFAULT_SINK@ toggle"))
   `("XF86AudioNext" (exec "mpc next"))
   `("XF86AudioPrev" (exec "mpc prev"))
   `("XF86AudioPlay" (exec "mpc toggle"))

   ;; brightness-keys
   `("XF86MonBrightnessUp" (exec "brightnessctl set +10%"))
   `("XF86MonBrightnessDown" (exec "brightnessctl set 10%-"))

   ;; window and group management
   `("s-f" (sway-fullscreen SWAY-FULLSCREEN-TOGGLE))

   ;; move focus
   `("s-h" (sway-focus-container SWAY-DIRECTION-LEFT))
   `("s-j" (sway-focus-container SWAY-DIRECTION-DOWN))
   `("s-k" (sway-focus-container SWAY-DIRECTION-UP))
   `("s-l" (sway-focus-container SWAY-DIRECTION-RIGHT))

   ;; move containers
   `("s-S-h" (sway-move-container SWAY-DIRECTION-LEFT))
   `("s-S-j" (sway-move-container SWAY-DIRECTION-DOWN))
   `("s-S-k" (sway-move-container SWAY-DIRECTION-UP))
   `("s-S-l" (sway-move-container SWAY-DIRECTION-RIGHT))

   ;; switch workspace
   `("s-C-h" (switch-workspace-left))
   `("s-C-j" (switch-workspace-down))
   `("s-C-k" (switch-workspace-up))
   `("s-C-l" (switch-workspace-right))

   ;; move container to workspace
   `("s-M-C-h" (move-container-to-workspace-left))
   `("s-M-C-j" (move-container-to-workspace-down))
   `("s-M-C-k" (move-container-to-workspace-up))
   `("s-M-C-l" (move-container-to-workspace-right))

   ;; Tab like cycling
   `("s-." (sway-focus-container-sibling SWAY-SIBLING-NEXT))
   `("s-," (sway-focus-container-sibling SWAY-SIBLING-PREV))

   `("s-w" (sway-kill))
   `("s-Return" (exec "alacritty"))
   `("M-s-Space" (exec "~/.bin/switch-keyboard-layout"))
   `("C-s-Space" (exec "rofi -show drun")))

  ;; define leader keymap
  (sway-define-keys
   #:prefix "s-Space" #:wk "Leader"
   `("o" (exec "rofi -show drun"))
   `("C-g" (sway-mode "default") #:wk "abort")

   ;; rofi keymap
   `(sway-define-keys
     #:prefix "r" #:wk "Rofi"
     ("p" (exec "~/.config/rofi/bin/password-manager"))
     ("m" (exec "rofi-mount"))
     ("u" (exec "rofi-unmount"))
     ("w" (exec ".config/rofi/bin/wifi"))
     ("b" (exec "~/.config/rofi/bin/bluetooth"))
     ("f" (exec "~/.config/rofi/bin/finder"))
     ("k" (exec "~/.config/rofi/bin/keyboard-layout"))
     ("P" (exec "~/.config/rofi/bin/powermenu"))
     ("s" (exec "~/.config/rofi/bin/sound-input"))
     ("S" (exec "~/.config/rofi/bin/sound-output")))

   ;; screenshot keymap
   `(sway-define-keys
     #:prefix "s" #:wk "Screenshot"
     ("d" (exec "export XDG_CURRENT_DESKTOP=sway && flameshot gui"))
     ("s" (exec "export XDG_CURRENT_DESKTOP=sway && flameshot screen"))
     ("f" (exec "export XDG_CURRENT_DESKTOP=sway && flameshot full"))
     ("m" (exec "export XDG_CURRENT_DESKTOP=sway && flameshot gui --last-region"))

     (sway-define-keys
      #:prefix "d" #:wk "DelayScreenshot"
      ("d" (exec "export XDG_CURRENT_DESKTOP=sway && flameshot gui -d 2500"))
      ("s" (exec "export XDG_CURRENT_DESKTOP=sway && flameshot screen -d 2500"))
      ("f" (exec "export XDG_CURRENT_DESKTOP=sway && flameshot full -d 2500"))
      ("l" (exec "export XDG_CURRENT_DESKTOP=sway && flameshot gui -d 2500 --last-region"))))

   ;; session keymap
   `(sway-define-keys
     #:prefix "q" #:wk "Session"
     ("q" (sway-exit))
     ("r" (sway-reload)))

   `(sway-define-keys
     #:prefix "w" #:wk "Window"
     ("v" (sway-layout SWAY-LAYOUT-SPLITV))
     ("h" (sway-layout SWAY-LAYOUT-SPLITH))
     ("f" (sway-fullscreen SWAY-FULLSCREEN-TOGGLE))
     ("d" (sway-layout SWAY-LAYOUT-DEFAULT))
     ("t" (sway-layout SWAY-LAYOUT-TABBED)))))
