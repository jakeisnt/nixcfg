
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

;; Let's have pretty source code blocks
(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil)

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

(sp-local-pair ;; pair << and >>
 '(org-mode)
 "<<" ">>"
 :actions '(insert))

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

(setq-default major-mode 'org-mode)

(provide 'config)
;;; config.el ends here
