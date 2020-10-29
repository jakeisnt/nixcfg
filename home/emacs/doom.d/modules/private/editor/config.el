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

;; Iterate through CamelCase words
(global-subword-mode 1)

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

(use-package! eslintd-fix
  :after js2-mode
  :init
  (setq flycheck-javascript-eslint-executable "eslint_d"
        eslintd-fix-executable "eslint_d")
  (add-hook! (js2-mode json-mode web-mode) 'eslintd-fix-mode))

(use-package! add-node-modules-path
  :after js2-mode
  :init
  (add-hook! (js2-mode json-mode web-mode) 'add-node-modules-path))

(setq lsp-clients-typescript-javascript-server-args "--jsx")

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

(use-package! editorconfig
  :config (editorconfig-mode 1))

(provide 'config)
;;; config.el ends here
