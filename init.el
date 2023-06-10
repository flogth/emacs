;; init --- My personal config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(unless (package-installed-p 'setup)
  (package-install 'setup))

(eval-when-compile
  (package-initialize)
  (require 'setup))

(defmacro set! (&rest args)
  "Macro for setting user options with `setq'-like ARGS."
  (declare (debug setq))
  `(setup (:option ,@args)))

(defun local/load-libraries ()
  "Load local libraries in `site-lisp' directory."
  (interactive)
  (dolist (d (directory-files (locate-user-emacs-file "site-lisp") t "^[^.]"))
    (when (file-directory-p d)
      (add-to-list 'load-path d))))
(local/load-libraries)

;;; Miscellaneous settings
(set! inhibit-startup-screen t
      visible-bell nil
      window-resize-pixelwise t
      frame-resize-pixelwise t
      pixel-scroll-mode t
      pixel-scroll-precision-mode t
      use-short-answers t)

(set! custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)

;;; Appearance and UI
(set! blink-cursor-mode nil
      column-number-mode t
      winner-mode t)

(setup modus-themes
  (set! modus-themes-variable-pitch-ui nil
        modus-themes-bold-constructs t
        modus-themes-italic-constructs t)
  (load-theme 'modus-operandi t)
  (:global "C-c t" #'modus-themes-toggle))

;;; Basic Editing
(set! tab-width 4
      indent-tabs-mode nil

      save-interprogram-paste-before-kill t
      kill-do-not-save-duplicates t

      savehist-mode t
      history-delete-duplicates t
      savehist-save-minibuffer-history t
      recentf-mode t
      save-place-mode t)

(setup elec-pair
  (set! electric-pair-mode t
        delete-pair-blink-delay 0))

(setup paren
  (set! show-paren-mode t
        show-paren-delay 0
        show-paren-context-when-offscreen t))

;;; Completion
(setup (:package vertico)
  (set! vertico-mode t
        vertico-cycle t
        vertico-resize nil))

(setup (:package corfu)
  (set! corfu-mode t
        corfu-auto t
        corfu-cycle t))

(setup (:package consult)
  (:global "C-c s" #'consult-line
           "C-c y" #'consult-yank-from-kill-ring))

(setup (:package orderless)
  (set! completion-styles '(orderless basic)))

(setup (:package marginalia)
  (set! marginalia-mode t))

(setup (:package avy)
  (:global "C-z" #'avy-goto-word-1))

;;; Help
(set! help-window-select t
      help-window-keep-selected t)

;;; Applications
(set! calendar-date-style 'iso
      calendar-week-start-day 1)

(setup dired
  (set! dired-dwim-target t))

(setup gnus
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
           mail-user-agent 'gnus-user-agent
           user-mail-address "flogth@mailbox.org"
           user-full-name "Florian Guthmann"
           message-server-alist
           '(("florian.guthmann@fau.de" . "smtp faumail.fau.de 587"))
           send-mail-function #'smtpmail-send-it
           smtpmail-smtp-server "smtp.mailbox.org"
           smtpmail-stream-type 'ssl
           smtpmail-smtp-service 465)
  (:hook #'gnus-topic-mode
         #'hl-line-mode)
  (:global "C-c m" #'gnus))

;;; Programming
(setup prog-mode
  (:hook #'display-line-numbers-mode))

;; version control
(setup (:package magit)
  (:global "C-c g" #'magit-status))

(setup (:package diff-hl)
  (:hook-into prog-mode))

;; compilation
(setup compilation
  (set! compilation-scroll-output 'first-error
        compilation-ask-about-save nil)
  (:global "C-c b" #'compile))

(setup ansi-color
  (set! ansi-color-for-compilation-mode t))

;; ide
(setup eglot
  (set! eglot-autoshutdown t
        eldoc-idle-delay 0.2)
  (:bind "C-c a" #'eglot-code-actions
         "C-c r" #'eglot-rename
         "C-c f" #'eglot-format))

(setup flymake
  (:hook-into prog-mode LaTeX-mode))

(set! xref-search-program 'ripgrep)

;;; Markup languages
(setup (:package web-mode)
  (:file-match "\\.html?\\'"))

(setup (:package auctex)
  (set! TeX-master 'dwim
           TeX-auto-save t
           TeX-parse-self t
           preview-auto-cache-preamble t
           TeX-electric-math '("$" . "$")
           TeX-electric-sub-and-superscript t
           LaTeX-electric-left-right-brace t
           reftex-enable-partial-scans t
           reftex-plug-into-AUCTeX t)

  (:hook #'visual-line-mode
         #'TeX-fold-mode
         #'LaTeX-math-mode
         #'reftex-mode))

(setup (:package org )
  (set! org-ellipsis " ↴"
        org-highlight-latex-and-related '(latex script entities)
        org-pretty-entities t
        org-hide-emphasis-markers nil
        org-preview-latex-image-directory
        (expand-file-name
         "ltxpng"
         (temporary-file-directory))
        org-src-window-setup 'current-window

        org-return-follows-link t
        org-mouse-1-follows-link t
        org-link-descriptive t
        org-html-doctype "xhtml5"
        org-html-html5-fancy t
        org-html-htmlize-output-type 'css)
    (:hook #'org-indent-mode
         #'visual-line-mode))

;;; Programming languages
(setup (:if-package agda2-mode)
    (when (executable-find "agda-mode")
      (load-file (let ((coding-system-for-read 'utf-8))
                   (shell-command-to-string "agda-mode locate"))))
    (set! agda2-highlight-face-groups 'default-faces))

(setup (:if-package gnu-apl-mode)
  (set! gnu-apl-show-tips-on-start nil))

(setup (:if-package proof-general)
  (set! proof-splash-enable nil
        proof-three-window-enable t
        proof-three-window-mode-policy 'smart))

(setup (:if-package sly)
  (:hook-into lisp-mode))

(setup (:if-package haskell-mode)
  (set! haskell-completing-read-function #'completing-read)
  (:hook #'interactive-haskell-mode))

(setup (:if-package antlr-mode)
  (:file-match "\\.g4\\'"))

(setup (:if-package nix-mode)
  (:file-match "\\.nix\\'"))

(setup prolog
  (:file-match "\\.pl\\'"))

;;; keybindings
(set-register ?d '(file . "~/.dotfiles"))
(set-register ?U '(file . "/ssh:uni:.www/"))
(set-register ?u '(file . "~/data/uni/lv"))
(set-register ?b '(file . "/ssh:blog:~"))

(global-set-key (kbd "C-c r") #'consult-recent-file)
(global-set-key (kbd "C-c C-f") #'consult-flymake)

  
(provide 'init)
;;; init.el ends here
