;;; package --- Summary
;;; Code:
(defun doom-modeline-conditional-buffer-encoding ()
  "Only show text encoding when it's not UTF-8."
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

(display-time-mode 1)

(defconst batteryFull
  "Power AC, battery Full (100.0% load"
  "A string representing a full battery.")

;; display battery info if available and not full
(unless (or (equal "Battery status not available" (battery))
            (equal  batteryFull (substring (battery) 0 (length batteryFull))))
  (display-battery-mode 1))

(use-package! memento-mori
  :config
  (setq memento-mori-birth-date "1999-11-05")
  (memento-mori-mode))

(progn
  (line-number-mode -1)
  (column-number-mode -1)
  (size-indication-mode -1))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

(setq
 doom-modeline-project-detection `projectile
 doom-modeline-checker-simple-format t
 doom-modeline-workspace-name t
 doom-modeline-persp-name nil
 doom-modeline-persp-icon nil
 doom-modeline-mu4e t
 doom-modeline-github t
 display-time-load-average nil
 display-time-24hr-format t)

(provide 'config)
;;; Commentary:
;;; config.el ends here
