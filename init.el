;;; init.el --- My emacs configuration              -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Florian Guthmann

;; Author: Florian Guthmann <florian.guthmann@fau.de>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Da steh ich nun, ich armer Tor!
;; Und bin so klug als wie zuvor

;;; Code:

(eval-when-compile
  (require 'package)
  (package-initialize))

(defmacro set! (&rest args)
  "Set variables using customize with usage of ARGS like `setq'."
  (declare (debug setq))
  `(progn
     ,@(cl-loop for (key value) on args by #'cddr
                collecting `(customize-set-variable ',key ,value))))

(let* ((site-lisp-dir (file-name-as-directory (locate-user-emacs-file "site-lisp")))
       (dirs (directory-files site-lisp-dir t "^[^.]")))
  (dolist (d dirs)
    (when (file-directory-p d)
      (let ((autoload-file (expand-file-name ".auto-site.el" d)))
        (add-to-list 'load-path d)
        (loaddefs-generate d autoload-file nil nil nil t)
        (byte-recompile-directory d)
        (load autoload-file)))))

(load (set! custom-file (locate-user-emacs-file "custom.el")) t)

;;;; Sane defaults
(set! inhibit-startup-message t
      window-resize-pixelwise t
      frame-resize-pixelwise t
      use-dialog-box nil
      use-short-answers t
      visible-bell nil)

(set! backup-directory-alist '((".*" . "~/.local/share/backup"))
      backup-by-copying t
      delete-old-versions t
      create-lockfiles nil
      auto-save-default nil)

(set! scroll-margin 1
      scroll-step 1
      scroll-conservatively 101
      scroll-preserve-screen-position t
      fast-but-imprecise-scrolling t)

(set! help-window-select t)

(set! calendar-week-start-day 1
      calendar-date-style 'iso)

(set! use-package-always-defer t)

;;;; ui
(require-theme 'modus-themes)
(set! modus-themes-italic-constructs t
      modus-themes-bold-constructs t)
(load-theme 'modus-operandi)
(global-set-key (kbd "<f5>") #'modus-themes-toggle)

(set! column-number-mode t
      mode-line-compact t
      mode-line-percent-position nil)

;;;; basic editing
(set! tab-width 4
      indent-tabs-mode nil)

(set! save-interprogram-paste-before-kill t
      kill-do-not-save-duplicates)

(use-package avy
  :ensure t
  :bind ("C-z" . avy-goto-word-1))

(use-package elec-pair
  :hook (after-init . electric-pair-mode))

(use-package paren
  :hook (after-init . show-paren-mode)
  :init (set! show-paren-delay 0
              show-paren-context-when-offscreen 'overlay))

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :bind (:map vertico-map
              ("C-j" . #'vertico-exit-input))
  :init (set! vertico-cycle t
              vertico-resize nil))

(use-package corfu
  :ensure t
  :hook (prog-mode . corfu-mode)
  :init (set! corfu-preview-current nil
              corfu-cycle t
              corfu-echo-documentation 0.25
              tab-always-indent 'complete))

(use-package consult
  :ensure t
  :bind (("C-c s" . #'consult-line)
         ("C-c S" . #'consult-ripgrep)
         ("C-c y" . #'consult-yank-from-kill-ring)))

(use-package orderless
  :ensure t
  :init (set! completion-styles '(orderless basic)))

(use-package savehist
  :hook (after-init . savehist-mode)
  :init (set! history-delete-duplicates t
              history-length 1000
              savehist-save-minibuffer-history t))

(use-package saveplace
  :hook (after-init . save-place-mode))

(use-package recentf
  :bind ("C-x C-r" . recentf)
  :init (set! recentf-mode t
              recentf-max-saved-items 128))

;;;; applications
(use-package dired
  :init (set! dired-dwim-target t
              dired-listing-switches "-NAhl --group-directories-first"))

(use-package gnus
  :hook ((gnus-mode . gnus-topic-mode)
         (gnus-mode . hl-line-mode))
  :bind ("C-c m" . gnus)
  :init
  (set! gnus-select-method '(nnnil)
        gnus-parameters
        '(("^nnimap"
           (gcc-self . t)
           (gnus-use-scoring . nil)
           (display . nil)
           (agent-predicate . always)))
        gnus-summary-line-format "%U%R%z%I%(%[%-23,23f%]%) %s\n"
        mm-discouraged-alternatives '("text/html" "text/richtext")
        gnus-secondary-select-methods
        '((nntp "gmane" (nntp-address "news.gmane.io"))
          (nnimap "uni"
                  (gnus-search-engine gnus-search-imap)
                  (nnimap-user "florian.guthmann@fau.de")
                  (nnimap-address "faumail.fau.de")
                  (nnimap-stream starttls))
          (nnimap "personal"
                  (gnus-search-engine gnus-search-imap)
                  (nnimap-user "flogth@mailbox.org")
                  (nnimap-address "imap.mailbox.org")
                  (nnimap-stream starttls)))
        gnus-posting-styles
        '(("" (gcc "nnimap+personal:Sent"))
          ("uni"
           (address "florian.guthmann@fau.de")
           (gcc "nnimap+uni:Sent"))
          ((header "to" "fau.de")
           (gcc "nnimap+uni:Sent"))
          ((header "from" "fau.de")
           (gcc "nnimap+uni:Sent")))
        mail-user-agent 'gnus-user-agent
        user-mail-address "flogth@mailbox.org"
        user-full-name "Florian Guthmann"
        message-server-alist
        '(("florian.guthmann@fau.de" . "smtp faumail.fau.de 587"))
        send-mail-function #'smtpmail-send-it
        smtpmail-smtp-server "smtp.mailbox.org"
        smtpmail-stream-type 'starttls
        smtpmail-smtp-service 587))

;;;; version control
(set! vc-follow-symlinks t)

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status)
  :init (set! magit-define-global-key-bindings nil))

(use-package diff-hl
  :ensure t
  :hook (prog-mode . diff-hl-mode))

;;;; development
(dolist (m '(display-line-numbers-mode
             flymake-mode
             hs-minor-mode))
  (add-hook 'prog-mode-hook m))

(use-package compile
  :bind ("C-c k" . compile)
  :init (set! compilation-scroll-output t
        compilation-ask-about-save nil))

(use-package ansi-color
  :hook (compilation-filter-hook . ansi-color-compilation-filter)
  :init (set! ansi-color-for-compilation-mode t))

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c a" . #'eglot-code-actions)
              ("C-c r" . #'eglot-rename)
              ("C-c f" . #'eglot-format))
  :init (set! eglot-autoshutdown t
              eglot-confirm-server-initiated-edits 'diff))

(use-package eldoc
  :init (set! eldoc-echo-area-use-multiline-p nil
              eldoc-idle-delay 0.2))


(use-package xref
  :init (set! xref-search-program 'ripgrep))

;;;; markup languages
(use-package auctex
  :ensure t
  :init (set! TeX-master 'dwim
              TeX-auto-save t
              TeX-parse-self t
              preview-auto-cache-preamble t
              TeX-electric-math '("$" . "$")
              TeX-electric-sub-and-superscript t
              LaTeX-electric-left-right-brace t
              reftex-enable-partial-scans t
              reftex-plug-into-AUCTeX t)
  (dolist (m '(visual-line-mode
               TeX-fold-mode
               LaTeX-math-mode
               reftex-mode
               flymake-mode))
    (add-hook 'LaTeX-mode-hook m)))

(use-package org
  :hook ((org-mode . org-indent-mode)
         (org-mode . visual-line-mode))
  :init (set! org-pretty-entities nil
              org-html-doctype "xhtml5"
              org-html-html5-fancy t
              org-html-htmlize-output-type 'css))

;;;; programming languages

(when (executable-find "agda-mode")
  (load-file
   (let ((coding-system-for-read 'utf-8))
     (shell-command-to-string "agda-mode locate"))))

(use-package proof-general
  :ensure t
  :init (set! proof-splash-enable nil
              proof-three-window-enable nil
              proof-script-fly-past-comments t))

(use-package gnu-apl-mode
  :init (set! gnu-apl-show-tips-on-start nil))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package sly
  :ensure t
  :hook (lisp-mode . sly)
  :init (set! inferiour-lisp-program "sbcl"))

(use-package haskell-mode
  :ensure t
  :hook (haskell-mode . interactive-haskell-mode)
  :init (set! haskell-completing-read-function #'completing-read))

(use-package sweeprolog
  :ensure t
  :mode "\\.pl\\'"
  :hook (sweeprolog-mode . sweeprolog-electric-layout-mode))

;;;; keybindings
(set! repeat-mode t)

(global-set-key (kbd "M-[") #'insert-pair)
(global-set-key (kbd "M-)") #'delete-pair)

;;;; miscellaneous
(set-register ?d '(file . "~/.dotfiles"))
(set-register ?U '(file . "/ssh:uni:~"))
(set-register ?u '(file . "~/data/uni"))

(provide 'init)
;;; init.el ends here
