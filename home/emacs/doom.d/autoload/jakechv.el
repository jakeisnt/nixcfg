;;; YY --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) YY YY
;;
;; Author: YY <http://github/YY>
;; Maintainer: YY <YY>
;; Created: YY
;; Modified: YY
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/YY/YY
;; Package-Requires: ((emacs YY) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  
;;
;;; Code:

Y

(defun gotop ()
  "Run the GOTOP process monitor."
  (interactive)
  (if (get-buffer "*gotop*")
      (switch-to-buffer "*gotop*")
    (ansi-term "/usr/bin/env bash" "gotop")
    (comint-send-string "*gotop*" "gotop\n")))

(defun htop ()
  "Run the HTOP process monitor."
  (interactive)
  (if (get-buffer "*htop*")
      (switch-to-buffer "*htop*")
    (ansi-term "/usr/bin/env bash" "htop")
    (comint-send-string "*htop*" "htop\n")))

(defun connect-vultr ()
  "Connect to my Vultr VPS."
  (interactive)
  (dired "/ssh:jake@107.191.42.68:"))

;; https://stackoverflow.com/questions/32977707/sending-bash-commands-to-an-open-terminal-buffer-in-emacs
(defun visit-project-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer (persp-ansi-buffer-name)))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term (getenv "SHELL"))
        (rename-buffer (persp-ansi-buffer-name))
        (end-of-buffer)
        (insert (format "cd %s" (projectile-project-root)))
        (term-send-input))
    (switch-to-buffer-other-window (persp-ansi-buffer-name))))

(provide 'jakechv)
;;; jakechv.el ends here
