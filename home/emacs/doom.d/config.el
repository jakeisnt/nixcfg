;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;;; Code:
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
      ;; auto-save-default t                         ; Nobody likes to lose work
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
;; (use-package! elfeed)
;; (use-package! elfeed-protocol
;;   :config
;;   (elfeed-protocol-enable)
;;   (setq elfeed-protocol-ttrss-maxsize 200
;;         elfeed-set-timeout 36000
;;         elfeed-show-entry-switch 'display-buffer
;;         elfeed-search-remain-on-entry t
;;         elfeed-feeds
;;         '(("ttrss+https://jake@rss.chvatal.com"
;;            :password (read-passwd "Provide your tt-rss password:")))))

;; (after! elfeed
;;   :init
;;   ;; (map! :map elfeed-search-mode-map
;; 	;; 	 "n" (lambda () (interactive) (next-line) (call-interactively 'elfeed-search-show-entry))
;; 	;; 	 "p" (lambda () (interactive) (previous-line) (call-interactively 'elfeed-search-show-entry))
;; 	;; 	 "m" (lambda () (interactive) (apply 'elfeed-search-toggle-all '(star))))
;;   (map!
;;    :leader
;;    :prefix "o"
;;    :desc "elfeed" "e" #'elfeed)
;;   (map! :leader
;;         :prefix "e"
;;         :desc "elfeed" "e" #'elfeed
;;         :desc "elfeed-protocol-ttrss-update" "u" #'elfeed-protocol-ttrss-update
;;         :desc "elfeed-protocol-ttrss-update-star" "s" #'elfeed-protocol-ttrss-update-star))

;; ------------------------------------------------------------------------ Org

(require 'find-lisp)
(setq org-directory "~/org/"
      j/org-agenda-directory "~/org/agenda/"
      org-agenda-files (find-lisp-find-files j/org-agenda-directory "\.org$")
      org-default-notes-file "~/org/refile.org"
      org-attach-id-dir "~/org/.attach/"
      org-roam-directory "~/org/wiki/org/")


(setq j/org-calendar-dir "~/org/calendar/")

(use-package! org-projectile
  :init
  (map! :leader
        :prefix "p"
        :desc "Add a TODO to a project" "n" #'org-projectile-project-todo-completing-read)
  :config
  (progn
    (org-projectile-per-project)
    (setq org-projectile-projects-file
          "TODO.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates))
  :ensure t)
(setq org-log-done 'time
      org-log-into-drawer t
      org-log-state-notes-insert-after-drawers nil)

(use-package! deft ;; use deft to index org wiki files
      :after org
      :bind
      ("C-c n d" . deft)
      :custom
      (deft-recursive t)
      (deft-ignore-file-regexp "hugo_setup")
      (deft-use-filename-as-title t)
      (deft-use-filter-string-for-filename t)
      (deft-default-extension "org")
      (deft-directory "~/org/wiki/org/"))

(use-package! org
  :mode ("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode)
  :init
  (map! :map org-mode-map
        "M-n" #'outline-next-visible-heading
        "M-p" #'outline-previous-visible-heading)
  (setq org-src-window-setup 'current-window
        org-return-follows-link t
        org-babel-load-languages '((emacs-lisp . t)
                                   (python . t)
                                   (dot . t))
        org-confirm-babel-evaluate nil
        org-use-speed-commands t
        org-catch-invisible-edits 'show org-preview-latex-image-directory "/tmp/ltximg/"
        org-structure-template-alist '(("a" . "export ascii")
                                       ("c" . "center")
                                       ("C" . "comment")
                                       ("e" . "example")
                                       ("E" . "export")
                                       ("h" . "export html")
                                       ("l" . "export latex")
                                       ("q" . "quote")
                                       ("s" . "src")
                                       ("v" . "verse")
                                       ("el" . "src emacs-lisp")
                                       ("d" . "definition")
                                       ("t" . "theorem")))
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'proselint 'org-mode)))

;; add org-protocol as an org-mode module
(setq org-modules '(org-protocol))


(setq org-capture-templates `(
        ("i" "inbox" entry (file ,(concat j/org-agenda-directory "inbox.org"))
         "* TODO %?")
        ("m" "media" entry (file+headline ,(concat j/org-agenda-directory "media.org") "Media")
         "* TODO [#A] Reply: %a :@home:@school:" :immediate-finish t)
        ("p" "Protocol" entry (file+headline ,(concat org-directory "inbox.org") "Inbox")
            "* TODO %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
        ("L" "Protocol Link" entry (file+headline ,(concat org-directory "inbox.org") "Inbox")
            "* TODO %? [[%:link][%:description]] \nCaptured On: %U")
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

(after! (org org-roam)
  (use-package! org-roam-protocol)
  (use-package! org-roam-server)
  (setq org-roam-ref-capture-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
               "%?"
               :file-name "websites/${slug}"
               :head "#+TITLE: ${title}
    #+ROAM_KEY: ${ref}
    - source :: ${ref}"
               :unnarrowed t)))
  (defun j/org-roam-export-all ()
    "Re-exports all Org-roam files to Hugo markdown."
    (interactive)
    (dolist (f (org-roam--list-all-files))
      (with-current-buffer (find-file f)
        (when (s-contains? "setupfile" (buffer-string))
          (org-hugo-export-wim-to-md)))))
  (defun j/org-roam--backlinks-list (file)
    (when (org-roam--org-roam-file-p file)
      (mapcar #'car (org-roam-db-query [:select :distinct [from]
                                                :from links
                                                :where (= to $s1)
                                                :and from :not :like $s2] file "%private%"))))
  (defun j/org-export-preprocessor (_backend)
    (when-let ((links (j/org-roam--backlinks-list (buffer-file-name))))
      (insert "\n** Backlinks\n")
      (dolist (link links)
        (insert (format "- [[file:%s][%s]]\n"
                        (file-relative-name link org-roam-directory)
                        (org-roam--get-title-or-slug link))))))

  (defun j/org-roam-export-updated ()
    "Re-export files that are linked to the current file."
    (let ((files (org-roam-db-query [:select [to] :from links :where (= from $s1)] buffer-file-name)))
      (interactive)
      (dolist (f files)
        (with-current-buffer (find-file-noselect (car f))
          (when (s-contains? "setupfile" (buffer-string))
            (org-hugo-export-wim-to-md))))))
  (add-hook 'org-export-before-processing-hook #'j/org-export-preprocessor))

(after! (org ox-hugo)
  (defun j/conditional-hugo-enable ()
    (save-excursion
      (if (cdr (assoc "SETUPFILE" (org-roam--extract-global-props '("SETUPFILE"))))
          (org-hugo-auto-export-mode +1)
        (org-hugo-auto-export-mode -1))))
  (add-hook 'org-mode-hook #'j/conditional-hugo-enable))



(use-package! company-org-roam
  :when (featurep! :completion company)
  :after org-roam
  :config
  (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))


(use-package! org-journal ;; org-journal configuration
  :config
  (setq org-journal-date-prefix "#+TITLE: "
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-dir "~/org/journal/"
        org-journal-carryover-items nil)
  (defun org-journal-today ()
    (interactive)
    (org-journal-new-entry t)))

(use-package! bibtex-completion ;; autocompletion for notes templates
  :config
  (setq bibtex-completion-notes-path "~/org/wiki/org/"
        bibtex-completion-bibliography "~/org/wiki/org/biblio.bib"
        bibtex-completion-pdf-field "file"
        bibtex-completion-notes-template-multiple-files
        (concat
         "#+title: ${title}\n"
         "#+roam_key: cite:${=key=}\n"
         "* TODO Notes\n"
         ":PROPERTIES:\n"
         ":Custom_ID: ${=key=}\n"
         ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
         ":AUTHOR: ${author-abbrev}\n"
         ":JOURNAL: ${journaltitle}\n"
         ":DATE: ${date}\n"
         ":YEAR: ${year}\n"
         ":DOI: ${doi}\n"
         ":URL: ${url}\n"
         ":END:\n\n")))

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

(setq doom-theme 'doom-nord)

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
;;
(map! :leader
      "/" (lambda () (interactive) (counsel-search)))

;; K :: open documentation for a symbol
;; gd :: go to definition

(map! :leader
      "h r n" (lambda () (interactive)
    (let ((default-directory "/sudo::"))
     (shell-command "nixos-rebuild switch"))))


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


(sp-local-pair ;; pair << and >>
     '(org-mode)
     "<<" ">>"
     :actions '(insert))

;; default browser is firefox
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

;; shared-clipboard
(setq select-enable-clipboard t
      interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Let's have pretty source code blocks
(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil)
