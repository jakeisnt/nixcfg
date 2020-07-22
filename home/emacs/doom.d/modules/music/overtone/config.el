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

(require 'cider)
(setq nrepl-hide-special-buffers t
      cider-repl-pop-to-buffer-on-connect nil
      cider-popup-stacktraces nil
      cider-repl-popup-stacktraces t)
(provide 'config)
;;; config.el ends here

