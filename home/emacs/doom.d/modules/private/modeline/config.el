;;; package --- Summary
;;; Code:
(defun doom-modeline-conditional-buffer-encoding ()
  "Only show text encoding when it's not UTF-8."
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

(display-time-mode 1)

;; display battery info if available
(unless (equal "Battery status not avalible"
               (battery))
  (display-battery-mode 1))
;; Iterate through CamelCase words
(global-subword-mode 1)

(use-package! memento-mori
  :config
  (setq memento-mori-birth-date "1999-11-05")
  (memento-mori-mode))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

(provide 'config)

;;; Commentary:
;;; config.el ends here
