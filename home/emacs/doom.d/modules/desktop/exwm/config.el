;;; desktop/exwm/config.el -*- lexical-binding: t; -*-
;;emacs window manager !!

;;; Code:

(require 'exwm)
(require 'exwm-config)
(exwm-config-example)
(require 'exwm-systemtray)
(require 'exwm-randr)

(setq
 exwm-workspace-number 10
 exwm-workspace-show-all-buffers t
 exwm-layout-show-all-buffers t
 ;; exwm-manage-force-tiling t
 exwm-systemtray-height 24)

;; show mode-line on floating windows.
(add-hook 'exwm-floating-setup-hook #'exwm-layout-show-mode-line)

;; Disable xrandr output named 'output'.
(defun my-exwm-xrandr-off (output)
  (if output (shell-command (concat "xrandr --output " output " --off"))))

;; Enable only one xrandr output named 'default'.
(defun my-exwm-xrandr-one-output (default)
  (shell-command (concat "xrandr --output " default " --auto")))

;; Update exwm-randr-workspace-output-plist with two outputs named
;; 'default' and 'other'.  If the 'other' output is same as 'default'
;; then all workspaces will be redirected to the 'default' output.
(defun my-exwm-xrandr-config (default other)
  (setq exwm-randr-workspace-output-plist
	(progn
	  (setq result (list 0 default))
	  (setq index 1)
	  (while (< index exwm-workspace-number)
	    (setq result (append result (list index other)))
	    (setq index (1+ index)))
	  result)))


;; Dynamically find the active xrandr outputs and update exwm
;; workspace configuration and enable xrandr outputs appropriately.
(defun my-exwm-xrandr-hook (default)
  (let* ((connected-cmd "xrandr -q|awk '/ connected/ {print $1}'")
	 (connected (process-lines "bash" "-lc" connected-cmd))
	 (previous (delete-dups (seq-remove
				 'integerp
				 exwm-randr-workspace-monitor-plist))))
    (cond ((member "DP-1" connected)
	   (progn (my-exwm-xrandr-config default "DP-1")
		  (my-exwm-xrandr-two-outputs default "DP-1")))
	  ((member "DP-2" connected)
	   (progn (my-exwm-xrandr-config default "DP-2")
		  (my-exwm-xrandr-two-outputs default "DP-2")))
	  ((member "HDMI-1" connected)
	   (progn (my-exwm-xrandr-config default "HDMI-1")
		  (my-exwm-xrandr-two-outputs default "HDMI-1")))
	  ((member "HDMI-2" connected)
	   (progn (my-exwm-xrandr-config default "HDMI-2")
		  (my-exwm-xrandr-two-outputs default "HDMI-2")))
	  (t (progn (my-exwm-xrandr-config default default)
		    (mapcar 'my-exwm-xrandr-off
			    (delete default previous)))))))

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

(defun j/shell-command (line)
  "Run a command and remove its newline."
  (replace-regexp-in-string "\n\\'" ""
                            (shell-command-to-string line)))

(defun j/screen-brightness-percentage ()
  "Get the brightness percentage of the screen."
  (string-to-number (shell-command-to-string "light")))

(defun j/audio-status ()
  "Get the current audio status."
  (split-string
   (j/shell-command "pulseaudio-ctl full-status") " "))

(defun j/get-audio-level ()
  "Get the current audio level."
  (string-to-number (car (j/audio-status))))

(defun j/source-is-muted ()
  "Is the audio currently muted?"
  (string= (car (cdr (j/audio-status))) "yes"))


(defun j/sink-is-muted ()
  "Is the audio currently muted?"
  (string= (car (cdr (cdr (j/audio-status)))) "yes"))

(defun j/change-volume (voldiff inc?)
  "Adjust the system's audio in the specified direction."
  (interactive)
  (let ((cmd (if inc? "up" "down"))
        (quantized-voldiff voldiff))
    (shell-command (format "pulseaudio-ctl %s %d" cmd quantized-voldiff))
    (if ((j/source-is-muted)
         (message "Audio is currently muted. Unmute.")
         (message "Audio level is %d" (j/audio-status)))
        (kill-buffer "*Shell Command Output*"))))

(defun j/toggle-audio-mute ()
  "Toggle the audio's mute status."
  (interactive)
  (shell-command "pulseaudio-ctl mute")
  (if ((j/source-is-muted)
       (message "Muted audio.")
       (message "Unmuted audio."))
      (kill-buffer "*Shell Command Output*")))

(defun j/change-brightness (brightdiff inc?)
  "Change the screen brightness by the number in the specified direction."
  (interactive)
  (let ((cmd (if inc? "-A" "-U"))
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

(defun j/run-sudo-command (cmd)
  "Run a command as root."
  (shell-command-to-string (concat "echo " (shell-quote-argument
                                            (read-passwd "Password? "))
                                   " | sudo -S " cmd)))


(defun j/run-sudo-cmd-with-output (cmd bufname)
  "Run a sudo command and show the output in a buffer."
  (setq buf (generate-new-buffer bufname))
  (with-output-to-temp-buffer buf
    (print (j/run-sudo-command "nixos-rebuild switch")))
  (switch-to-buffer buf))

(defun j/nixos-rebuild ()
  "Rebuild NixOS."
  (interactive)
  (j/run-sudo-cmd-with-output "nixos-rebuild switch" "*NixOS*"))

(defun j/restart-network-manager ()
  "Restart network-manager service."
  (interactive)
  (j/run-sudo-command "systemctl restart NetworkManager"))

(push ?\C-w exwm-input-prefix-keys)
