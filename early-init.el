;; early-init --- Stuff to eval before init.el -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(setq
 ;; do not popup warning buffer
 native-comp-async-report-warnings-errors 'silent
 ;; prefer .el files over .elc
 load-prefer-newer t)

;; disable some visual stuff
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;;; early-init.el ends here
