;;; desktop/exwm/config.el -*- lexical-binding: t; -*-
;;emacs window manager !!

;;; Code:

(require 'exwm)
(require 'exwm-config)
(exwm-config-example)

(require 'exwm-randr)

(setq MON1 "eDP-1"
      MON2 "DP-1"
      MON3 "DP-2")
(setq
 exwm-workspace-number 2
  exwm-workspace-show-all-buffers t
  exwm-layout-show-all-buffers t
  exwm-manage-force-tiling t)

;; (setq exwm-randr-workspace-monitor-plist '(1 MON1
;;                                            2 MON1
;;                                            3 MON1
;;                                            4 MON1
;;                                            5 MON1
;;                                            6 MON1
;;                                            7 MON1
;;                                            8 MON1
;;                                            9 MON3
;;                                            0 MON2))
;;
(setq exwm-randr-workspace-monitor-plist '(1 "eDP-1"
                                             2 "DP-2"))

;; (add-hook 'exwm-randr-screen-change-hook
;;           (lambda ()
;;             (start-process-shell-command
;;              "xrandr" nil "xrandr --output eDP-1 --output DP-1 --scale 2x2 --above eDP-1 --output DP-2 --scale 2x2 --right-of DP-1")))

;; (defun exwm-change-screen-hook ()
;;   (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
;;         default-output)
;;     (with-temp-buffer
;;       (call-process "xrandr" nil t nil)
;;       (goto-char (point-min))
;;       (re-search-forward xrandr-output-regexp nil 'noerror)
;;       (setq default-output (match-string 1))
;;       (forward-line)
;;       (if (not (re-search-forward xrandr-output-regexp nil 'noerror))
;;           (call-process "xrandr" nil nil nil "--output" default-output "--auto")
;;         (call-process
;;          "xrandr" nil nil nil
;;          "--output" (match-string 1) "--primary" "--auto"
;;          "--output" default-output "--off")
;;         (setq exwm-randr-workspace-monitor-plist (list 0 (match-string 1)))))))

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

(setq exwm-workspace-show-all-buffers t
      exwm-layout-show-all-buffers t)

;; system tray
(require 'exwm-systemtray)
(exwm-systemtray-enable)
(display-time-mode 1)
(display-battery-mode 1)

;; better firefox experience in exwm
(use-package! exwm-firefox-evil
              :config (add-hook 'exwm-manage-finish-hook 'exwm-firefox-evil-activate-if-firefox))

;; add something to firefox
(dolist (k `(escape))
  (cl-pushnew k exwm-input-prefix-keys))


;; Using ido to change "tabs" in Firefox!
;; from technomancy dude
;; For this to work properly you need to stop opening new tabs and open
;; everything in new windows. It sounds crazy, but then you can use ido
;; to switch between "tabs" and everything is wonderful.
;;
;; Step 1: about:config -> browser.tabs.opentabfor.middleclick -> false
;; Step 2: change whatever "open link in new tab" binding in Saka Key or
;;         whatever you use to open the link in a new window
;; Step 3: rebind ctrl-t to open a new window as well
;; Step 4: place the following in chrome/userChrome.css in your FF profile:
;;         #tabbrowser-tabs { visibility: collapse !important; }
;; Step 5: add this code to your exwm config:
;; Step 6: restart your browser and enjoy your new C-x b fanciness!
;; (defun pnh-trim-non-ff ()
;;   (delete-if-not (apply-partially 'string-match "- Mozilla Firefox$")
;;                   ido-temp-list))

;; (add-hook 'exwm-manage-finish-hook
;;           (defun pnh-exwm-manage-hook ()
;;             (when (string-match "Firefox" exwm-class-name)
;;               (exwm-workspace-move-window 3)
;;               (exwm-layout-hide-mode-line)
;;               (setq ido-make-buffer-list-hook 'pnh-trim-non-ff))
;;             (when (string-match "Chromium" exwm-class-name)
;;               (exwm-workspace-move-window 1)
;;               (exwm-layout-hide-mode-line))))

(add-hook 'exwm-update-title-hook
          (defun pnh-exwm-title-hook ()
            (when (string-match "Firefox" exwm-class-name)
              (exwm-workspace-rename-buffer exwm-title))))

; (setq browse-url-firefox-arguments '("-new-window"))

(setq browse-url-new-window-flag t
      browse-url-firefox-new-window-is-tab t)
;; browse-url-browser-function 'browse-url-firefox

(exwm-input-set-key (kbd "s-SPC") #'counsel-linux-app)

;; jump to buffers with s-hjkl
(exwm-input-set-key (kbd "s-h") #'windmove-left)
(exwm-input-set-key (kbd "s-j") #'windmove-down)
(exwm-input-set-key (kbd "s-k") #'windmove-up)
(exwm-input-set-key (kbd "s-l") #'windmove-right)

;; grow and shrink windows
(exwm-input-set-key (kbd "s-[") 'shrink-window-horizontally)
(exwm-input-set-key (kbd "s-{") 'shrink-window)
(exwm-input-set-key (kbd "s-]") 'enlarge-window-horizontally)
(exwm-input-set-key (kbd "s-}") 'enlarge-window)

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
(provide 'config);;;
