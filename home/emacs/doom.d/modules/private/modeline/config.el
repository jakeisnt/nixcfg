;;; package --- Summary
;;; Code:
(defun doom-modeline-conditional-buffer-encoding ()
  "Only show text encoding when it's not UTF-8."
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

(display-time-mode 1)

(defconst batteryFull
  "Power AC, battery Full (100.0% load, remaining time 0:00)"
  "A string representing a full battery.")

;; display battery info if available and not full
(unless (or (equal "Battery status not available" (battery))
            (equal  batteryFull (battery)))
  (display-battery-mode 1))

(use-package! memento-mori
  :config
  (setq memento-mori-birth-date "1999-11-05")
  (memento-mori-mode))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

(provide 'config)

;;; Commentary:
;;; config.el ends here
