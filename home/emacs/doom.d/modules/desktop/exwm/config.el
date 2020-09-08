;;; desktop/exwm/config.el -*- lexical-binding: t; -*-
;;emacs window manager !!

;;; Code:

(require 'exwm)
(require 'exwm-config)
(exwm-config-example)

(require 'exwm-randr)

(setq
 exwm-workspace-number 10
 exwm-workspace-show-all-buffers t
 exwm-layout-show-all-buffers t
 exwm-manage-force-tiling t)

(defun exwm-change-screen-hook ()
  "Opens EXWM on additional monitors as they're plugged in."
  (interactive) ;; for convenience of testing
  (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
        default-output
        cur-monitor
        last-monitor)
    (with-temp-buffer
      (call-process "xrandr" nil t nil)
      (goto-char (point-min))
      (re-search-forward xrandr-output-regexp nil 'noerror)
      (setq default-output (match-string 1))
      (setq last-monitor default-output)
      (setq exwm-randr-workspace-monitor-plist (list 0 default-output
                                                     1 "DP-1"
                                                     2 "DP-1"
                                                     3 "DP-1"
                                                     4 "DP-1"
                                                     5 "DP-1"
                                                     6 "DP-2"
                                                     7 "DP-2"
                                                     8 "DP-2"
                                                     9 "DP-2"))
      (forward-line)
      ;; if there is more than one thing to render:
      (if (re-search-forward xrandr-output-regexp nil 'noerror)
          (progn
            ;; render default monitor larger
            (message (concat "Rendering first monitor to " default-output))
            (call-process "xrandr" nil nil nil "--output" default-output "--primary" "--mode" "1920x1080" "--scale" "1x1" "--auto")
            (with-temp-buffer
              (call-process "xrandr" nil t nil)
              (goto-char (point-min))
              ;; for each active monitor:
              (while (not (eobp))
                ;; when the next monitor name is found:
                (when (and (re-search-forward xrandr-output-regexp nil 'noerror)
                           (not (string= (match-string 1) default-output)))
                  (progn
                    ;; add it to the screen to the right of the previous
                    (setq cur-monitor (match-string 1))
                    (call-process "xrandr" nil nil nil "--output" cur-monitor "--auto" "--right-of" last-monitor)
                    ;; add it to the list of current monitors
                    (message (concat "adding the monitor " cur-monitor))
                    (setq last-monitor cur-monitor)
                    ;; (add-to-list exwm-randr-workspace-monitor-plist (+ (/ (length exwm-randr-workspace-monitor-plist) 2) 1) cur-monitor)
                    )))))
        ;; else:
        (progn
          (message (concat "rendering to " default-output))
          (call-process "xrandr" nil nil nil "--output" default-output "--primary" "--mode" "3840x2160" "--scale" "1x1" "--auto")))
      (call-process "systemctl" nil nil nil "--user" "restart" "picom")
      (exwm-randr-refresh))))

(add-hook 'exwm-randr-screen-change-hook 'exwm-change-screen-hook)
(exwm-randr-enable)

(defun jethro/exwm-rename-buffer-to-title ()
  "Rename Firefox buffers to include their window titles."
  (exwm-workspace-rename-buffer (format "%s - %s" exwm-class-name exwm-title)))
(add-hook 'exwm-update-title-hook 'jethro/exwm-rename-buffer-to-title)
(add-hook 'exwm-update-class-hook
          (defun my-exwm-update-class-hook ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name)
                        (string= "Firefox" exwm-class-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (defun my-exwm-update-title-hook ()
            (cond ((or (not exwm-instance-name)
                       (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                       (string= "gimp" exwm-instance-name)
                       (string= "Firefox" exwm-class-name))
                   (exwm-workspace-rename-buffer exwm-title)))))

;; better firefox experience in exwm
(use-package! exwm-firefox-evil
  :config (add-hook 'exwm-manage-finish-hook 'exwm-firefox-evil-activate-if-firefox))

;; add something to firefox
(dolist (k `(escape))
  (cl-pushnew k exwm-input-prefix-keys))

(add-hook 'exwm-update-title-hook
          (defun pnh-exwm-title-hook ()
            (when (string-match "Firefox" exwm-class-name)
              (exwm-workspace-rename-buffer exwm-title))))

(setq browse-url-new-window-flag t
      browse-url-firefox-new-window-is-tab t)

;; Dmenu-adjacent
(exwm-input-set-key (kbd "s-SPC") #'counsel-linux-app)

;; jump to buffers with s-hjkl
(exwm-input-set-key (kbd "s-H") #'+evil/window-move-left)
(exwm-input-set-key (kbd "s-J") #'+evil/window-move-down)
(exwm-input-set-key (kbd "s-K") #'+evil/window-move-down)
(exwm-input-set-key (kbd "s-L") #'+evil/window-move-right)

;; move buffers with HJKL
(exwm-input-set-key (kbd "s-h") #'windmove-left)
(exwm-input-set-key (kbd "s-j") #'windmove-down)
(exwm-input-set-key (kbd "s-k") #'windmove-up)
(exwm-input-set-key (kbd "s-l") #'windmove-right)

;; grow and shrink windows
(exwm-input-set-key (kbd "s-[") 'shrink-window-horizontally)
(exwm-input-set-key (kbd "s-{") 'shrink-window)
(exwm-input-set-key (kbd "s-]") 'enlarge-window-horizontally)
(exwm-input-set-key (kbd "s-}") 'enlarge-window)

(defun j/screen-brightness-percentage ()
  "Get the brightness percentage of the screen."
  (interactive)
  (string-to-number (shell-command-to-string "light")))

(defun j/audio-status ()
  "Get the current audio status."
  (interactive)
  (split-string
   (shell-command-to-string "pulseaudio-ctl full-status") " "))

(defun j/get-audio-level ()
  "Get the current audio level."
  (string-to-number (car (j/audio-status))))

(defun j/source-is-muted? ()
  "Is the audio currently muted?"
  (string= (car (cdr (j/audio-status))) "yes"))

(defun j/sink-is-muted? ()
  "Is the audio currently muted?"
  (string= (car (cdr (cdr (j/audio-status)))) "yes"))

(defun j/change-volume (voldiff inc?)
  "Adjust the system's audio in the specified direction."
  (interactive)
  (let ((cmd (when inc? "up" "down"))
        (quantized-voldiff voldiff))
    (shell-command (format "pulseaudio-ctl %s %d" cmd quantized-voldiff))
    (if ((j/source-is-muted?)
          (message "Audio is currently muted. Unmute.")
          (message "Audio level is %d" (j/audio-status)))
        (kill-buffer "*Shell Command Output*"))))

(defun j/toggle-audio-mute ()
  "Toggle the audio's mute status."
  (interactive)
  (shell-command "pulseaudio-ctl mute")
  (if ((j/source-is-muted?)
        (message "Muted audio.")
        (message "Unmuted audio."))
  (kill-buffer "*Shell Command Output*")))

(defun j/change-brightness (brightdiff inc?)
  "Change the screen brightness by the number in the specified direction."
  (interactive)
  (let ((cmd (when inc? "-A" "-U"))
        (quantized-bdiff brightdiff))
    (shell-command (format "light %s %d" cmd quantized-bdiff))
    (message "Screen brightness is now %d" (j/screen-brightness-percentage))
    (kill-buffer "*Shell Command Output*")))

;; audio fix for computer
(exwm-input-set-key
 (kbd "<XF86AudioRaiseVolume>")
 (lambda () (interactive) (shell-command "pulseaudio-ctl up 10")))
(exwm-input-set-key
 (kbd "<XF86AudioLowerVolume>")
 (lambda () (interactive) (shell-command "pulseaudio-ctl down 10")))
(exwm-input-set-key
 (kbd "<XF86AudioMute>")
 (lambda () (interactive) (shell-command "pulseaudio-ctl mute")))

(exwm-input-set-key
 (kbd "<XF86MonBrightnessUp>")
 (lambda () (interactive) (shell-command "light -A 10")))
(exwm-input-set-key
 (kbd "<XF86MonBrightnessDown>")
 (lambda () (interactive) (shell-command "light -U 10")))

;; remappings for firefox
(evil-define-key 'normal exwm-firefox-evil-mode-map (kbd "t") 'exwm-firefox-core-window-new)
(push ?\C-w exwm-input-prefix-keys)
