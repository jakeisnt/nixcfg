;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Jacob Chvatal"
      user-mail-address "jakechvatal@gmail.com")

(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 tab-width 4                                      ; Set width for tabs
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to lose work
      inhibit-compacting-font-caches t            ; When there are lots of glyphs, keep them in memory
      truncate-string-ellipsis "…")               ; Unicode ellispis are nicer than "...", and also save /precious/ space

(delete-selection-mode 1)                         ; Replace selection when inserting text
(display-time-mode 1)                             ; Enable time in the mode-line
(unless (equal "Battery status not avalible"
               (battery))
  (display-battery-mode 1))                       ; On laptops it's nice to know how much power you have
(global-subword-mode 1)                           ; Iterate through CamelCase words

;; always split window to bottom right
(setq evil-vsplit-window-right t
      evil-split-window-below t)


;; open ivy to look for buffer after splitting window
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))

(defun doom-modeline-conditional-buffer-encoding ()
  "Only show text encoding when it's not UTF-8"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))


(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)


;; -------------------------------------------------------------------------- RSS
(use-package! elfeed)
(use-package! elfeed-protocol
  :config
  (elfeed-protocol-enable)
  (setq elfeed-protocol-ttrss-maxsize 200
        elfeed-set-timeout 36000
        elfeed-show-entry-switch 'display-buffer
        elfeed-search-remain-on-entry t
        elfeed-feeds
        '(("ttrss+https://jake@rss.chvatal.com"
           :password (read-passwd "Provide your tt-rss password:")))))

(after! elfeed
  :init
  ;; (map! :map elfeed-search-mode-map
	;; 	 "n" (lambda () (interactive) (next-line) (call-interactively 'elfeed-search-show-entry))
	;; 	 "p" (lambda () (interactive) (previous-line) (call-interactively 'elfeed-search-show-entry))
	;; 	 "m" (lambda () (interactive) (apply 'elfeed-search-toggle-all '(star))))
  (map!
   :leader
   :prefix "o"
   :desc "elfeed" "e" #'elfeed)
  (map! :leader
        :prefix "e"
        :desc "elfeed" "e" #'elfeed
        :desc "elfeed-protocol-ttrss-update" "u" #'elfeed-protocol-ttrss-update
        :desc "elfeed-protocol-ttrss-update-star" "s" #'elfeed-protocol-ttrss-update-star))


;; ------------------------------------------------------------------------ Org

(require 'find-lisp)
(setq org-directory "~/org/"
      j/org-agenda-directory "~/org/agenda/"
      org-agenda-files (find-lisp-find-files j/org-agenda-directory "\.org$")
      org-default-notes-file "~/org/refile.org"
      org-attach-id-dir "~/org/.attach/")


(setq j/org-calendar-dir "~/org/calendar/")

(setq org-log-done 'time
      org-log-into-drawer t
      org-log-state-notes-insert-after-drawers nil)

(use-package! org-chef
  :ensure t)


(setq org-capture-templates
      `(("i" "inbox" entry (file ,(concat j/org-agenda-directory "inbox.org"))
         "* TODO %?")
        ("m" "media" entry (file+headline ,(concat j/org-agenda-directory "media.org") "Media")
         "* TODO [#A] Reply: %a :@home:@school:" :immediate-finish t)
        ("l" "link" entry (file ,(concat j/org-agenda-directory "inbox.org"))
         "* TODO %(org-cliplink-capture)" :immediate-finish t)
        ("c" "org-protocol-capture" entry (file ,(concat j/org-agenda-directory "inbox.org"))
         "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)
        ("r" "recipe" entry (file "~/org/recipes.org")
         "%(org-chef-get-recipe-from-url)"
         :empty-lines 1)
        ;; ("m" "manual recipe" entry (file "~/org/recipes.org")
        ;;  "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")
        ))


(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-theme 'doom-one)

(setq display-line-numbers-type 'relative) ;; relative line numbers

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;

;; K :: open documentation for a symbol
;; gd :: go to definition

(map! :leader
      "h r n" (lambda () (interactive) (shell-command "sudo -S nixos-rebuild switch")))


(defun gotop ()
  "Run the GOTOP process monitor."
  (interactive)
  (if (get-buffer "*gotop*")
      (switch-to-buffer "*gotop*")
    (ansi-term "/bin/bash" "gotop")
    (comint-send-string "*gotop*" "gotop\n")))


(defun htop ()
  "Run the HTOP process monitor."
  (interactive)
  (if (get-buffer "*htop*")
      (switch-to-buffer "*htop*")
    (ansi-term "/bin/bash" "htop")
    (comint-send-string "*htop*" "htop\n")))

(defun connect-vultr () ;; connect to vps
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
