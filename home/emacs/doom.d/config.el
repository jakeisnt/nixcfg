;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; https://github.com/tecosaur/emacs-config/blob/master/config.org

;; -------------------------------------------------------------- Global Configuration
(setq user-full-name "Jacob Chvatal"
      user-mail-address "jakechvatal@gmail.com"
      company-idle-delay nil
      lsp-ui-sideline-enable nil
      lsp-enable-symbol-highlighting nil)

(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 tab-width 4                                      ; Set width for tabs
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      inhibit-compacting-font-caches t            ; When there are lots of glyphs, keep them in memory
      truncate-string-ellipsis "…")               ; Unicode ellispis are nicer than "...", and also save /precious/ space

(delete-selection-mode 1)                         ; Replace selection when inserting text
(display-time-mode 1)                             ; Enable time in the mode-line
(unless (equal "Battery status not avalible"
               (battery))
  (display-battery-mode 1))                       ; On laptops it's nice to know how much power you have
(global-subword-mode 1)                           ; Iterate through CamelCase words

(if (eq initial-window-system 'x)                 ; if started by emacs command or desktop file
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))

;; determine where to split the window
(setq evil-vsplit-window-right t
      evil-split-window-below t)

;; open ivy to look for buffer after splitting window
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))

(setq +ivy-buffer-preview t)

(setq-default major-mode 'org-mode)

(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

(setq doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)


;; some other stuff
(use-package! deadgrep
  :if (executable-find "rg")
  :init
  (map! :leader
        :desc "ripgrep" "r" #'deadgrep))

(use-package! fira-code-mode
  :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x"))
  :hook prog-mode) ;; Enables fira-code-mode automatically for programming major modes

(use-package! mastodon
  :ensure t
  :init
  (setq mastodon-instance-url "https://merveilles.town")
  :config
  (mastodon-discover)
  (map!
   :leader
   :prefix "t"
   :desc "toot" "t" #'mastodon-toot))

;; ------------------------------------------------------------------------- Mail
(use-package! notmuch :commands (notmuch)
  :init
  (map! :desc "notmuch" "<f2>" #'notmuch)
  (map! :map notmuch-search-mode-map
        :desc "toggle read" "t" #'+notmuch/toggle-read
        :desc "Reply to thread" "r" #'notmuch-search-reply-to-thread
        :desc "Reply to thread sender" "R" #'notmuch-search-reply-to-thread-sender)
  (map! :map notmuch-show-mode-map
        :desc "Next link" "<tab>" #'org-next-link
        :desc "Previous link" "<backtab>" #'org-previous-link
        :desc "URL at point" "C-<return>" #'browse-url-at-point)
  (defun +notmuch/toggle-read ()
    "toggle read status of message"
    (interactive)
    (if (member "unread" (notmuch-search-get-tags))
        (notmuch-search-tag (list "-unread"))
      (notmuch-search-tag (list "+unread"))))
  :config
  (setq message-auto-save-directory "~/.mail/drafts/"
        message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program (executable-find "msmtp")
        message-sendmail-envelope-from 'header
        mail-envelope-from 'header
        mail-specify-envelope-from t
        message-sendmail-f-is-evil nil
        message-kill-buffer-on-exit t
        notmuch-always-prompt-for-sender t
        notmuch-archive-tags '("-inbox" "-unread")
        notmuch-crypto-process-mime t
        notmuch-hello-sections '(notmuch-hello-insert-saved-searches)
        notmuch-labeler-hide-known-labels t
        notmuch-search-oldest-first nil
        notmuch-archive-tags '("-inbox" "-unread")
        notmuch-message-headers '("To" "Cc" "Subject" "Bcc")
        notmuch-saved-searches '((:name "inbox" :query "tag:inbox")
                                 (:name "unread" :query "tag:inbox and tag:unread")
                                 (:name "drafts" :query "tag:draft"))))


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

;; -------------------------------------------------------------------------- Org
;; org-directories

(require 'find-lisp)

(setq org-directory "~/org/"
      j/org-agenda-directory "~/org/agenda/"
      org-agenda-files (find-lisp-find-files j/org-agenda-directory "\.org$")
      org-default-notes-file "~/org/refile.org"
      org-attach-id-dir "~/org/.attach/")

(setq j/org-calendar-dir "~/org/calendar/")

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

;; ;; Org-GCAL
;; (use-package! org-gcal
;;   :config (setq org-gcal-client-id
;;                 "619470530759-7g9daplb32uk39fmg2tsqqt15p9vvn5v.apps.googleusercontent.com"
;;                 org-gcal-client-secret
;;                 "lcOVB80NCf2_3Ei8WV0_4JKP"
;;                 org-gcal-file-alist
;;                 '(("jakechvatal@gmail.com" . "~/org/calendar/schedule.org"))))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  :hook (after-init . org-roam-mode)
  :custom-face (org-roam-link ((t (:inherit org-link :foreground "#005200"))))
  :init
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-show-graph" "g" #'org-roam-graph
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-capture" "c" #'org-roam-capture)
  (setq org-roam-directory "~/org/wiki/org/"
        org-roam-db-location "~/org/wiki/org/org-roam.db"
        org-roam-graph-exclude-matcher "private"
        org-roam-tag-sources '(prop last-directory)
        org-id-link-to-org-use-id t)
  :config
  (setq org-roam-capture-templates
        '(("l" "lit" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "lit/${slug}"
           :head "#+setupfile:./hugo_setup.org
#+hugo_slug: ${slug}
#+title: ${title}\n"
           :unnarrowed t)
          ("c" "concept" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "concepts/${slug}"
           :head "#+setupfile:./hugo_setup.org
#+hugo_slug: ${slug}
#+title: ${title}\n"
           :unnarrowed t)
          ("p" "people" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "people/${slug}"
           :head "#+setupfile:./hugo_setup.org
#+hugo_slug: ${slug}
#+title: ${title}\n"
           :unnarrowed t)
          ("t" "tools" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "tools/${slug}"
           :head "#+setupfile:./hugo_setup.org
#+hugo_slug: ${slug}
#+title: ${title}\n"
           :unnarrowed t)
          ("p" "private" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "private/${slug}"
           :head "#+title: ${title}\n"
           :unnarrowed t)))
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "lit/${slug}"
           :head "#+setupfile:./hugo_setup.org
#+roam_key: ${ref}
#+hugo_slug: ${slug}
#+roam_tags: website
#+title: ${title}
- source :: ${ref}"
           :unnarrowed t))))

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

(use-package! deft
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


(use-package! org-journal
  :config
  (setq org-journal-date-prefix "#+TITLE: "
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-dir "~/org/journal/"
        org-journal-carryover-items nil)
  (defun org-journal-today ()
    (interactive)
    (org-journal-new-entry t)))


;; TODO figure out what this is for
(use-package! bibtex-completion
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


;; (use-package! org-noter
;;   :after org
;;   :ensure t
;;   :config (setq org-noter-default-notes-file-names '("notes.org")
;;                 org-noter-notes-search-path '("~/org/notes/")
;;                 org-noter-separate-notes-from-heading t))

;; (use-package! citeproc-org
;;   :after org
;;   :config
;;   (citeproc-org-setup))

(require 'company-org-roam)
(use-package company-org-roam
  :when (featurep! :completion company)
  :after org-roam
  :config
  (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))

;; add auto spacing at 80 lines to org mode
(add-hook 'org-mode-hook '(lambda () (setq fill-column 80))
          'org-mode-hook 'turn-on-auto-fill)
;; ...and hopefully other things as well.
(add-hook 'org-journal-mode-hook '(lambda () (setq fill-column 80))
          'org-journal-mode-hook 'turn-on-auto-fill)
(add-hook 'org-capture-mode-hook '(lambda () (setq fill-column 80))
          'org-capture-mode-hook 'turn-on-auto-fill)
(add-hook 'org-roam-mode-hook '(lambda () (setq fill-column 80))
          'org-roam-mode-hook 'turn-on-auto-fill)

;; ----------------------------------------------------------------------------- ETC
;; clipboard between systems
(setq select-enable-clipboard t
      interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; line number configuration
(setq display-line-numbers-type 'relative)

;; ### mode fixes and configuration ###
(add-hook 'julia-mode 'julia-repl-mode)
(add-hook 'darkroom-mode 'visual-line-mode) (add-hook 'writeroom-mode 'visual-line-mode)

;; org alert default style
;;(setq alert-default-style 'libnotify)

;; ### PATH DEBUG FIX ###
;;(setq exec-path-from-shell-arguments '("-i"))

;; ### BROWSER ###
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

(use-package! smerge-mode
  :bind (("C-c h s" . j/hydra-smerge/body))
  :init
  (defun j/enable-smerge-maybe ()
    "Auto-enable `smerge-mode' when merge conflict is detected."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil :noerror)
        (smerge-mode 1))))
  (add-hook 'find-file-hook #'j/enable-smerge-maybe :append)
  :config
  (defhydra j/hydra-smerge (:color pink
                            :hint nil
                            :pre (smerge-mode 1)
                            ;; Disable `smerge-mode' when quitting hydra if
                            ;; no merge conflicts remain.
                            :post (smerge-auto-leave))
    "
   ^Move^       ^Keep^               ^Diff^                 ^Other^
   ^^-----------^^-------------------^^---------------------^^-------
   _n_ext       _b_ase               _<_: upper/base        _C_ombine
   _p_rev       _u_pper           g   _=_: upper/lower       _r_esolve
   ^^           _l_ower              _>_: base/lower        _k_ill current
   ^^           _a_ll                _R_efine
   ^^           _RET_: current       _E_diff
   "
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("q" nil "cancel" :color blue)))


(use-package! spray
  :config
  (setq spray-wpm 500
        spray-height 700))

(sp-local-pair
     '(org-mode)
     "<<" ">>"
     :actions '(insert))

;; (after! tramp
;;   (setenv "SHELL" "/bin/bash")
;;   (setq tramp-shell-prompt-pattern "\\(?:^\\|
;; \\)[^]#$%>\n]*#?[]#$%>] *\\(\\[[0-9;]*[a-zA-Z] *\\)*")) ;; default + 

;; (after! text-mode
;;   (add-hook! 'text-mode-hook
;;     ;; Apply ANSI color codes
;;     (with-silent-modifications
;;       (ansi-color-apply-on-region (point-min) (point-max)))))

(after! flyspell
  (require 'flyspell-lazy)
  (flyspell-lazy-mode 1))

(use-package! tramp
  :config
  (setq tramp-default-method "ssh"))

(defun connect-vultr () ;; connect to vps
  (interactive)
  (dired "/ssh:jake@107.191.42.68:"))

;; supercollider
(use-package! sclang)

;; set XeTeX mode in TeX/LaTeX
;; (add-hook 'LaTeX-mode-hook
;;           (lambda()
;;              (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
;;              (setq TeX-command-default "XeLaTeX")
;;              (setq TeX-save-query nil)
;;              (setq TeX-show-compilation t)))

 ;; to use pdfview with auctex
 ;; (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
 ;;    TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
 ;;    TeX-source-correlate-start-server t) ;; not sure if last line is neccessary
;; to have the buffer refresh after compilation
 (add-hook 'TeX-after-compilation-finished-functions
        #'TeX-revert-document-buffer)

(use-package! latex-preview-pane
  :config
  (setq pdf-latex-command "xelatex"))

(add-hook 'latex-mode-hook #'latex-preview-pane-mode)

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

;; (use-package wttrin
;;   :init
;;   (setq wttrin-default-cities '("Boston" "Portland" "New York")
;;         wttin-default-accept-language '("Accept-Language" . "en-US"))
;;   (defun wttrin-fetch-raw-string (query) TODO: modify pacakge to do this
;;     ;; "Get the weather information based on your query."
;;     (let ((url-user-agent "curl")))))


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

;; ----------------------------------------------------------------------------- OFF-LIMITS
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-interactive-popup-errors nil)
 '(latex-preview-pane-multifile-mode (quote auctex))
 '(package-selected-packages
   (quote
    (el-get exwm-firefox-evil auctex markdown-mode+ markdown-mode company-irony-c-headers flyspell-lazy org-chef ox-hugo mastodon fira-code-mode deft org-projectile-helm proof-general org-roam-server org-roam-bibtex org-projectile org-noter exwm-x elfeed-protocol company-org-roam)))
 '(safe-local-variable-values
   (quote
    ((format-all-mode)
     (electric-indent-mode)
     (eval
      (quote
       ((electric-indent-mode 0)
        (format-all-mode 0))))))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-roam-link ((t (:inherit org-link :foreground "#005200")))))
(put 'customize-group 'disabled nil)
