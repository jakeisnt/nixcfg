;;; YY --- description -*- lexical-binding: t; -*-
;;;
;;
;; Copyright (C) YY YY
;;
;; Author: YY <http://github/YY> ;; Maintainer: YY <YY> ;; Created: YY
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
      interprogram-paste-function 'x-cut-buffer-or-selection-value
      gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024)
      lsp-completion-provider :capf
      lsp-idle-delay 0.500
      )

(setq projectile-globally-ignored-directories '("node_modules" ".happypack" "flow-typed" "build" "lib")
      grep-find-ignored-directories '("node_modules" ".happypack"))

(setq doom-theme 'doom-nord)

;; always split window to bottom right
(setq evil-vsplit-window-right t
      evil-split-window-below t)

(delete-selection-mode 1)


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
  (abbrev-mode tec/set-text-mode-abbrev-table)
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

;; Visit a commonly used URL.
;; TODO: Add hook to browse-url to show a list of
;; frequently used websites to visit in a minibuffer;
;; determine these based on previous uses of browse-url
;; or from browser history somehow.
(use-package! browse-url
  :init
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "firefox"))

(use-package! link-hint
  :ensure t
  :defer t)

(define-key evil-normal-state-map (kbd "SPC a") 'link-hint-open-link)

(defun skira-setup ()
  "Open everything I need to be productive at Skira."
  (interactive)
  (browse-url "https://app.slack.com/client/T0R0C5VFV")
  (browse-url "https://mail.google.com/mail/u/2/#inbox")
  (browse-url "https://calendar.google.com/calendar/b/2/r?tab=mc")
  (browse-url "https://github.com/plantaseed")
  (browse-url "https://app.asana.com/0/inbox/1189245019163511"))

  ;; Find a URL
  (map! :leader
        "\"" (lambda ()
               (interactive)
               (browse-url (read-string "URL:"))))
  (map! :leader
        "'" (lambda () (interactive) (counsel-search)))
  (map!
   :leader
   :prefix "v"
   :desc "Visit Calendar" "c" (lambda () (interactive) (browse-url "https://calendar.google.com"))
   :desc "Visit Discord" "d" (lambda () (interactive) (browse-url "https://discord.gg"))
   :desc "Visit Spotify" "s" (lambda () (interactive) (browse-url "https://open.spotify.com"))
   :desc "Visit Skira" "S" #'skira-setup
   :desc "Visit Gmail" "m" (lambda () (interactive) (browse-url "https://gmail.com"))
   :desc "Visit GitHub" "g" (lambda () (interactive) (browse-url "https://github.com/jakechv")))


;;; automatic #bang
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(defun hlu-make-script-executable ()
  "If file starts with a shebang, make `buffer-file-name' executable

       Since it doesn't rely on ##chmod##, it also works for remote
       files, i.e. those accessed by TrampMode.

       taken from:
       http://www.emacswiki.org/emacs-en/MakingScriptsExecutableOnSave"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (and (looking-at "^#!")
            	 (not (file-executable-p buffer-file-name)))
        (set-file-modes buffer-file-name
            		(logior (file-modes buffer-file-name) #o100))
        (message (concat "Made " buffer-file-name " executable"))))))

(provide 'config)
;;; config.el ends here
