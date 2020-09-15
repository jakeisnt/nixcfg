;;; YY --- description -*- lexical-binding: t; -*-
;;;
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

(require 'evil)

;;; Code:
(setq user-full-name "Jacob Chvatal"
      user-mail-address "jakechvatal@gmail.com")

(setq-default delete-by-moving-to-trash t
              tab-width 4
              uniquify-buffer-name-style 'forward
              window-combination-resize t
              x-stretch-cursor t
              history-length 1000
              prescient-history-length 100)

(setq undo-limit 80000000
      evil-want-fine-undo t
      auth-source-cache-expiry nil
      inhibit-compacting-font-caches t
      display-line-numbers-type 'relative
      select-enable-clipboard t
      interprogram-paste-function 'x-cut-buffer-or-selection-value)

(setq projectile-globally-ignored-directories '("node_modules" ".happypack" "flow-typed" "build" "lib")
      grep-find-ignored-directories '("node_modules" ".happypack")

(setq doom-theme 'doom-nord
      doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 13))

(delete-selection-mode 1)

;; always split window to bottom right
(setq evil-vsplit-window-right t
      evil-split-window-below t)

(map!
 :leader
 :prefix "w"
 "v" (lambda () (interactive) (evil-window-vsplit) (+ivy/switch-buffer))
 "s" (lambda () (interactive) (evil-window-split) (+ivy/switch-buffer))
 "x" (lambda () (interactive) (evil-window-vsplit) (+ivy/projectile-find-file))
 "z" (lambda () (interactive) (evil-window-vsplit) (+ivy/project-search)))

(setq +ivy-buffer-preview t) ;; buffer previews

;; TODO: figure out the buffer names I want!
(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string ".*/[0-9]*-?" "🢔 " buffer-file-name)
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

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


(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

(use-package! abbrev
  :init
  (setq-default abbrev-mode t)
  ;; a hook function that sets the abbrev-table to org-mode-abbrev-table
  ;; whenever the major mode is a text mode
  (defun tec/set-text-mode-abbrev-table ()
    (if (derived-mode-p 'text-mode)
        (setq local-abbrev-table org-mode-abbrev-table)))
  :commands abbrev-mode
  :hook
  (abbrev-mode . tec/set-text-mode-abbrev-table)
  :config
  (setq abbrev-file-name (expand-file-name "abbrev.el" doom-private-dir))
  (setq save-abbrevs 'silently))


(use-package! company
  :config
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2
        company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort))

(map! :leader
      "h r n" (lambda () (interactive)
                (async-shell-command
                 (concat "echo " (shell-quote-argument (read-passwd "Rebuilding NixOS. Password: "))
                         " | sudo -S nixos-rebuild switch"))))

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

(use-package! latex-preview-pane
  :config
  (setq pdf-latex-command "xelatex")
  (add-hook 'latex-mode-hook #'latex-preview-pane-mode))

(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

;; to use pdfview with auctex
;; (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
;;    TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
;;    TeX-source-correlate-start-server t) ;; not sure if last line is neccessary
;; to have the buffer refresh after compilation
(provide 'config)
;;; config.el ends here
