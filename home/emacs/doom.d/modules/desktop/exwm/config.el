;;; desktop/exwm/config.el -*- lexical-binding: t; -*-
;;emacs window manager !!

(use-package! exwm)
(use-package! exwm-config)
(exwm-config-default)
(use-package! exwm-randr)
(setq exwm-randr-workspace-output-plist '(0 "eDP-1"
                                          1 "eDP-1"
                                          2 "eDP-1"
                                          3 "eDP-1"
                                          4 "eDP-1"
                                          5 "eDP-1"
                                          6 "eDP-1"
                                          7 "eDP-1"
                                          8 "eDP-1"
                                          9 "eDP-1")
      exwm-workspace-show-all-buffers t
      exwm-layout-show-all-buffers t
      exwm-manage-force-tiling t)

(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output eDP-1 --scale 1x1")))
(exwm-randr-enable)

(defun jethro/exwm-rename-buffer-to-title () (exwm-workspace-rename-buffer (format "%s - %s" exwm-class-name exwm-title)))
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
(use-package! exwm-systemtray)
(exwm-systemtray-enable)

;; better firefox experience in exwm
(use-package! exwm-firefox-evil)
(add-hook 'exwm-manage-finish-hook 'exwm-firefox-evil-activate-if-firefox)
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

;; (add-hook 'exwm-update-title-hook
;;           (defun pnh-exwm-title-hook ()
;;             (when (string-match "Firefox" exwm-class-name)
;;               (exwm-workspace-rename-buffer exwm-title))))

(setq browse-url-firefox-arguments '("-new-window"))

(exwm-input-set-key (kbd "s-SPC") #'counsel-linux-app)

;; jump to buffers with s-hjkl
(exwm-input-set-key (kbd "s-h") #'windmove-left)
(exwm-input-set-key (kbd "s-j") #'windmove-down)
(exwm-input-set-key (kbd "s-k") #'windmove-up)
(exwm-input-set-key (kbd "s-l") #'windmove-right)

;; swap buffers with C-s-hjkl
(exwm-input-set-key
 (kbd "C-s-h")
 (lambda () (interactive) (aw-swap-window (window-in-direction 'left))))
(exwm-input-set-key
 (kbd "C-s-j")
 (lambda () (interactive) (aw-swap-window (window-in-direction 'below))))
(exwm-input-set-key
 (kbd "C-s-k")
 (lambda () (interactive) (aw-swap-window (window-in-direction 'above))))
(exwm-input-set-key
 (kbd "C-s-l")
 (lambda () (interactive) (aw-swap-window (window-in-direction 'right))))

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

;; clean exwm logout function
(defun exwm-logout ()
  (interactive)
  (recentf-save-list)
  (save-some-buffers)
  (start-process-shell-command "logout" nil "lxsession-logout"))
