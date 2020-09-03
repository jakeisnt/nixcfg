;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Code:
(setq user-full-name "Jacob Chvatal"
      user-mail-address "jakechvatal@gmail.com")

;; don't expire cached auth information
(setq auth-source-cache-expiry nil)

(setq-default delete-by-moving-to-trash t                      ; Delete files to trash
              tab-width 4                                      ; Set width for tabs
              uniquify-buffer-name-style 'forward              ; Uniquify buffer names
              window-combination-resize t                      ; take new window space from all other windows (not just current)
              x-stretch-cursor t                               ; Stretch cursor to the glyph width
              history-length 1000                              ; improve history tracking for better recommendations
              prescient-history-length 100)

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      inhibit-compacting-font-caches t)           ; When there are lots of glyphs, keep them in memory

(delete-selection-mode 1)                         ; Replace selection when inserting text
(display-time-mode 1)                             ; Enable time in the mode-line

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

(defun doom-modeline-conditional-buffer-encoding ()
  "Only show text encoding when it's not UTF-8"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

(setq-default major-mode 'org-mode)
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
      j/org-calendar-dir "~/org/calendar/"
      org-agenda-files (find-lisp-find-files j/org-agenda-directory "\.org$")
      org-default-notes-file "~/org/refile.org"
      org-attach-id-dir "~/org/.attach/"
      org-roam-directory "~/org/wiki/org/"
      org-use-property-inheritance t ;; convenient
      org-log-done 'time ;; log the time you finish something
      org-list-allow-alphabetical t ;; allow alpha bullets
      org-export-in-background t) ;; run export async
;; org-catch-invisible-edits 'smart ;; dont do weird invisible stuff
;; org-re-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")

(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))

(use-package! org-ref
  :after org
  :config
  (setq org-ref-completion-library 'org-ref-ivy-cite))

(use-package! org-projectile
  :init
  (map! :leader
        :prefix "p"
        :desc "Add a TODO to a project" "n" #'org-projectile-project-todo-completing-read
        :desc "Find a line of code with a search term in a project." "l" #'+ivy/project-search
        :desc "Add a TODO to the current project." "N" #'org-projectile-capture-for-current-project)
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


(setq calc-angle-mode 'rad  ;; radians are rad
      calc-algebraic-mode t ;; allows '2*x instead of 'x<RET>2*
      calc-symbolic-mode t) ;; keeps stuff like √2 irrational for as long as possible

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
  (setq org-roam-graph-node-extra-config '(("shape"      . "underline")
                                           ("style"      . "rounded,filled")
                                           ("fillcolor"  . "#EEEEEE")
                                           ("color"      . "#C9C9C9")
                                           ("fontcolor"  . "#111111")
                                           ("fontname"   . "Overpass")))
  (use-package! org-roam-protocol)
  (use-package! org-roam-server
    :after org-roam
    :config
    (setq org-roam-server-host "127.0.0.1"
          org-roam-server-port 8078
          org-roam-server-export-inline-images t
          org-roam-server-authenticate nil
          org-roam-server-label-truncate t
          org-roam-server-label-truncate-length 60
          org-roam-server-label-wrap-length 20))

  (defun org-roam-server-open ()
    "Ensure the server is active, then open the roam graph."
    (interactive)
    (org-roam-server-mode 1)
    (browse-url-xdg-open (format "http://localhost:%d" org-roam-server-port)))


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

;; Search for something on the internet
(map! :leader
      "'" (lambda () (interactive) (counsel-search)))
;; Find a URL
(map! :leader
      "\"" (lambda ()
             (interactive)
             (browse-url (read-string "URL:"))))

;; K :: open documentation for a symbol
;; gd :: go to definition
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

(use-package! link-hint
  :ensure t
  :defer t)

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

(defun skira-setup ()
  "Open everything I need to be productive at Skira."
  (interactive)
  (browse-url-firefox "https://app.slack.com/client/T0R0C5VFV")
  (browse-url-firefox "https://mail.google.com/mail/u/2/#inbox")
  (browse-url-firefox "https://calendar.google.com/calendar/b/2/r?tab=mc")
  (browse-url-firefox "https://github.com/plantaseed")
  (browse-url-firefox "https://app.asana.com/0/inbox/1189245019163511"))

(map!
 :leader
 :prefix "o"
 :desc "Open everything I need to be productive at Skira." "S" #'skira-setup)

(define-key evil-normal-state-map (kbd "SPC a") 'link-hint-open-link)

(use-package! keycast ;; from tecosaur
  :commands keycast-mode
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (progn
          (add-hook 'pre-command-hook 'keycast-mode-line-update t)
          (add-to-list 'global-mode-string '("" mode-line-keycast " ")))
      (remove-hook 'pre-command-hook 'keycast-mode-line-update)
      (setq global-mode-string (remove '("" mode-line-keycast " ") global-mode-string))))
  (custom-set-faces!
    '(keycast-command :inherit doom-modeline-debug
                      :height 0.9)
    '(keycast-key :inherit custom-modified
                  :height 1.1
                  :weight bold)))


(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

(use-package abbrev
  :init
  (setq-default abbrev-mode t)
  ;; a hook funtion that sets the abbrev-table to org-mode-abbrev-table
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


(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2
        company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.

(after! flyspell (require 'flyspell-lazy) (flyspell-lazy-mode 1))

(use-package! org-super-agenda
  :commands (org-super-agenda-mode))
(after! org-agenda
  (org-super-agenda-mode))
(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator nil
      org-agenda-tags-column 100 ;; from testing this seems to be a good value
      org-agenda-compact-blocks t)

;; customize this in the future!
(setq org-agenda-custom-commands
      '(("o" "Overview"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                          :time-grid t
                          :date today
                          :todo "TODAY"
                          :scheduled today
                          :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:name "Next to do"
                           :todo "NEXT"
                           :order 1)
                          (:name "Important"
                           :tag "Important"
                           :priority "A"
                           :order 6)
                          (:name "Due Today"
                           :deadline today
                           :order 2)
                          (:name "Due Soon"
                           :deadline future
                           :order 8)
                          (:name "Overdue"
                           :deadline past
                           :face error
                           :order 7)
                          (:name "Assignments"
                           :tag "Assignment"
                           :order 10)
                          (:name "Issues"
                           :tag "Issue"
                           :order 12)
                          (:name "Emacs"
                           :tag "Emacs"
                           :order 13)
                          (:name "ProjectsG"
                           :tag "Project"
                           :order 14)
                          (:name "Research"
                           :tag "Research"
                           :order 15)
                          (:name "To read"
                           :tag "Read"
                           :order 30)
                          (:name "Waiting"
                           :todo "WAITING"
                           :order 20)
                          (:name "University"
                           :tag "uni"
                           :order 32)
                          (:name "Trivial"
                           :priority<= "E"
                           :tag ("Trivial" "Unimportant")
                           :todo ("SOMEDAY" )
                           :order 90)
                          (:discard (:tag ("Chore" "Routine" "Daily")))))))))))


(setf (alist-get 'height +org-capture-frame-parameters) 15)

(setq +org-capture-fn
      (lambda ()
        (interactive)
        (set-window-parameter nil 'mode-line-format 'none)
        (org-capture)))

(after! org-superstar
  (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        org-superstar-prettify-item-bullets t ))

(after! org
  (setq org-ellipsis " ▾ "
        org-priority-highest ?A
        org-priority-lowest ?E
        org-priority-faces
        '((?A . 'all-the-icons-red)
          (?B . 'all-the-icons-orange)
          (?C . 'all-the-icons-yellow)
          (?D . 'all-the-icons-green)
          (?E . 'all-the-icons-blue))))

;; Visit a commonly used URL.
;; TODO: Add hook to browse-url to show a list of
;; frequently used websites to visit in a minibuffer;
;; determine these based on previous uses of browse-url
;; or from browser history somehow.
(map!
 :leader
 :prefix "v"
 "s" (lambda () (interactive) (browse-url "https://spotify.com"))
 "d" (lambda () (interactive) (browse-url "https://discord.gg"))
 "m" (lambda () (interactive) (browse-url "https://gmail.com"))
 "g" (lambda () (interactive) (browse-url "https://github.com")))

(use-package! web-mode
  :init
  ;; adjust indents for web-mode to 2 spaces
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  :config
  ;; for better jsx syntax-highlighting in web-mode
  ;; - courtesy of Patrick @halbtuerke
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it)))

(use-package! add-node-modules-path
  :after js2-mode
  :init
  (add-hook! (js2-mode json-mode web-mode) #'add-node-modules-path))

(use-package! eslintd-fix
  :after js2-mode
  :init
  (setq flycheck-javascript-eslint-executable "eslint_d"
        eslintd-fix-executable "eslint_d")
  (add-hook! (js2-mode json-mode web-mode) 'eslintd-fix-mode))

;; (use-package! tide
;;   :after js2-mode
;;   :init
;;   (setq tide-completion-detailed t
;;         tide-always-show-documentation t)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   (company-mode +1)
;;   (add-hook! (js2-mode) #'tide-setup))

(setq projectile-globally-ignored-directories '("node_modules" ".happypack" "flow-typed" "build" "lib"))
(setq grep-find-ignored-directories '("node_modules" ".happypack"))


(define-format-all-formatter prettier
  (:executable "prettier")
  (:install "echo 'not installing prettier'")
  (:languages))

(use-package! emacs-format-all-the-code
  :config
  (define-format-all-formatter eslint_d
    (:executable "eslint_d")
    (install)
    (:languages
    "CSS" "GraphQL" "JavaScript" "JSON" "JSX" "Less" "Markdown" "PHP"
    "SCSS" "Solidity" "TOML" "TSX" "TypeScript" "Vue" "YAML"
    "_Angular" "_Flow")
    (:format
    (format-all--buffer-easy
      executable
      "--cache --parser" (let ((pair (assoc language
                                    '(("_Angular"   . "angular")
                                      ("_Flow"      . "flow")
                                      ("JavaScript" . "babel")
                                      ("JSX"        . "babel")
                                      ("Solidity"   . "solidity-parse")
                                      ("TSX"        . "typescript")))))
                  (if pair (cdr pair) (downcase language)))
      (when (buffer-file-name) (list "--stdin-filename" (buffer-file-name)))))))
