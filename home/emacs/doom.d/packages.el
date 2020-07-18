;; -*- no-byte-compile: t; -*-
;; .doom.d/packages.el

(package! linum-relative)
(package! org-alert)
(package! undo-tree)
(package! evil-snipe)
(package! darkroom) ;; better writeroom, for now
(package! focus) ;; focuses on current line

(package! deadgrep)
(package! quelpa)
(package! quelpa-use-package)
(package! anki-editor)

(package! org-tanglesync)
(package! org-chef)
(package! spray)
(package! tramp)

;; (package! emacs-anywhere)
(package! flyspell-lazy)
(package! wttrin)
(package! org-msg)
(package! mu4e-alert)

(package! evil-tex :recipe (:host github :repo "itai33/evil-tex")
                            :pin "4826bffa28...")
(package! org-super-agenda :pin "dd0d104c26...")

(package! org-pretty-table-mode
  :recipe (:host github :repo "Fuco1/org-pretty-table") :pin "88380f865a...")
(package! ox-gfm :pin "99f93011b0...")
(package! org-ref :pin "b05d6b4434...")
(package! org-graph-view :recipe (:host github :repo "alphapapa/org-graph-view") :pin "13314338d7...")
(package! org-roam-server :pin "7617ac01a1...")
(package! systemd :pin "51c148e09a...")

;; async package installation
(package! el-get)
(package! use-package-el-get)

;; plotting or something
(package! gnuplot)
(package! gnuplot-mode)
(package! org-plot :recipe (:local-repo "lisp" :no-byte-compile t))
(package! doct) ;; TODO: declarative org-capture templates
