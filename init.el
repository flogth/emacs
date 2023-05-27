;; init --- My personal config -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
;;; functions for configuration ============================
(unless (package-installed-p 'setup)
  (package-install 'setup))

(require 'setup)
(setup-define :load-after
  (lambda (&rest features)
    (let ((body `(require ',(setup-get 'feature))))
      (dolist (feature (nreverse features))
        (setq body `(with-eval-after-load ',feature ,body)))
      body))
  :documentation "Load the current feature after FEATURES.")

(defmacro set! (&rest args)
  "Macro for setting user options.  `setq'-like ARGS."
  (declare (debug setq))
  `(setup (:option ,@args)))

(defun local/load-libraries ()
  "Load local libraries in `site-lisp' directory."
  (interactive)
  (dolist (d (directory-files (locate-user-emacs-file "site-lisp") t "^[^.]"))
    (when (file-directory-p d)
      (add-to-list 'load-path d))))
(local/load-libraries)

;;; basic settings =========================================
(set! inhibit-startup-message t
      vc-follow-symlinks t       ; do not warn when following symlinks
      visible-bell nil           ; do not flash a visual bell
      window-resize-pixelwise t  ; more flexible resizing
      frame-resize-pixelwise t
      use-dialog-box nil)

;; only in emacs >= 28
(when (boundp 'use-short-answers)
  (set! use-short-answers t))

;; customization
(set! custom-file
      (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; backups
(set! backup-directory-alist
      `(("." . ,(expand-file-name ".backups" user-emacs-directory)))
      auto-save-default nil
      backup-by-copying t
      delete-old-versions t
      create-lockfiles nil)

;;; ui =====================================================

(set-fringe-mode '(10 . 0))  ; add padding to frame
(set! blink-cursor-mode nil) ; do not blink cursor

(custom-set-faces
 '(line-number ((t (:height 0.8)))))

;; some visual niceties
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?…))
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code ?↩))

(set! window-divider-default-right-width 3
      window-divider-default-places 'right-only)
(window-divider-mode)

;; theme
(setup modus-themes
  (:option
   modus-themes-variable-pitch-ui nil
   modus-operandi-palette-overrides '((comment green-cooler)
                                      (fg-prompt cyan-intense)
                                      (proof-locked-face bg-green-subtle))
   modus-themes-bold-constructs t
   modus-themes-italic-constructs t
   modus-themes-prompts '(bold))
  (load-theme 'modus-vivendi t)
  (custom-set-faces
   '(default ((t (:weight regular :height 140 :family "JuliaMono"))))
   '(fixed-pitch ((t (:family (face-attribute 'default :family)))))))

;; modeline
(defun local/mode-line-modified ()
  "Mode line segment for buffer state."
  (or (if (buffer-file-name (buffer-base-buffer))
          (cond
           ((buffer-modified-p)
            (propertize "● " 'face 'custom-set))
           (buffer-read-only
            (propertize "■ " 'face 'mode-line))))
      "  "))

(defconst mode-line-modal-alist
  '((normal .  ("[N]" . font-lock-variable-name-face))
    (insert .  ("[I]" . font-lock-string-face))
    (visual .  ("[V]" . font-lock-keyword-face))
    (replace . ("[R]" . font-lock-type-face))
    (motion .  ("[M]" . font-lock-constant-face))
    (beacon .  ("[B]" . font-lock-builtin-face))))

(defun local/mode-line-modal ()
  "Mode line segment for meow mode."
  (if (boundp 'meow--current-state)
    (let ((m (alist-get meow--current-state
                        mode-line-modal-alist)))
      (propertize (car m) 'face (cdr m)))
    ""))

(defun local/mode-line-format (left right)
  (concat left
          " "
          (propertize
           " "
           'display `((space :align-to
                             (- right
                                (- 0 right-margin)
                                ,(length right)))))
          right))

(setq-default mode-line-format
      '((:eval
         (local/mode-line-format
          (format-mode-line
           '((:eval (format-mode-line (local/mode-line-modified) ))
             mode-line-buffer-identification
             (:eval (format-mode-line (local/mode-line-modal)))
             " (%l,%c)"))
          (format-mode-line
           '((:eval current-input-method-title)
             " "
             (:eval mode-name)
             (:eval mode-line-end-spaces)))))))

(column-number-mode t)

(winner-mode t)

;;; editor =================================================
(set! tab-width 4
      indent-tabs-mode nil)

;; kill ring
(set! save-interprogram-paste-before-kill t
      kill-do-not-save-duplicates t)

;; insert brackets,parens,... as pairs
(setup elec-pair
  (:option electric-pair-mode t
           delete-pair-blink-delay 0))

;; show matching parentheses
(setup paren
  (:option show-paren-mode t
           show-paren-delay 0
           show-paren-context-when-offscreen t))

;; editorconfig
(setup (:package editorconfig)
  (:option editorconfig-mode t))

;; buffer-env
(setup (:if-package buffer-env)
  (:option buffer-env-script-name "flake.nix"))

;; scrolling
(set! scroll-margin 1
      scroll-step   1
      scroll-conservatively 101
      scroll-preserve-screen-position t
      fast-but-imprecise-scrolling t)

;;; completion =============================================
(setup (:package vertico)
  (require 'vertico)
  (:option vertico-cycle t
           vertico-resize nil)
  (vertico-mode))

(setup (:package corfu)
  (require 'corfu)
  (:option corfu-auto t
           corfu-auto-delay 0
           corfu-preview-current nil
           corfu-cycle t
           corfu-echo-documentation 0.25
           tab-always-indent 'complete)
  (global-corfu-mode))

(setup (:package consult)
  (require 'consult)
  (:global "C-c s" #'consult-line
           "C-c S" #'consult-ripgrep
           "C-c y" #'consult-yank-from-kill-ring))

(setup (:package orderless)
  (:option completion-styles '(orderless basic)))

(setup (:package marginalia)
  (:option marginalia-mode t))

(setup
  ;; history
  (:option savehist-mode t
           history-delete-duplicates t
           history-length 1000
           savehist-save-minibuffer-history t)
  ;; recent files
  (:option recentf-mode t)
  ;; position in files
  (:option save-place-mode t))

;; help
(set! help-window-select t)

;;; applications ===========================================
;; calendar
(set! calendar-week-start-day 1
      calendar-date-style 'iso)

;; dired
(setup dired
  (:option dired-dwim-target t
           dired-listing-switches "-NAhl --group-directories-first"))

;; eshell
(setup eshell
  (:option eshell-banner-message ""
           eshell-scroll-to-bottom-on-input  t
           eshell-scroll-to-bottom-on-output t
           eshell-kill-processes-on-exit t
           eshell-hist-ignoredups t
           eshell-error-if-no-glob t)
  (:global "C-c t" #'local/eshell-new))

;; mail
(setup gnus
  (:option gnus-select-method '(nnnil)
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

;;; development ============================================
(setup prog-mode
  (:hook #'display-line-numbers-mode
         #'hs-minor-mode))

;; git
(setup (:package magit diff-hl)
  (:load-after meow flymake)
  ;; magit
  (:option magit-define-global-key-bindings nil)
  (:bind-into magit-mode-map
    "x" #'magit-discard
    "J" #'meow-next-expand
    "K" #'meow-prev-expand
    "L" #'magit-log)
  ;; diff in margin
  (require 'diff-hl)
  (:with-mode diff-hl-mode
    (:hook-into prog-mode)
    (add-hook 'magit-pre-refresh-hook  #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
  (:global "C-c g" #'magit-status))

;; compilation
(set! compilation-scroll-output 'first-error
      compilation-ask-about-save nil)

(setup ansi-color
  (:option ansi-color-for-compilation-mode t)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

;; ide
(setup eglot
  (:option eglot-autoshutdown t
           eldoc-echo-area-use-multiline-p nil
           eldoc-idle-delay 0.2
           eglot-confirm-server-initiated-edits nil)
  (:bind "C-c a" #'eglot-code-actions
         "C-c r" #'eglot-rename
         "C-c f" #'eglot-format))

(setup flymake
  (:hook-into prog-mode))

(setup xref
  ;; xref
  (:option xref-search-program 'ripgrep))

(setup outline-minor-mode
  (:option outline-minor-mode-cycle t)
  (:hook-into text-mode lisp-data-mode))

;;; prose languages ========================================
;; HTML
(setup (:package web-mode)
  (:file-match "\\.html?\\'"))

(setup (:package rainbow-mode)
  (:hook-into prog-mode))

;; LaTeX
(setup (:package auctex)
  (:option TeX-master 'dwim
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
         #'reftex-mode
         #'flymake-mode))

;; org
(setup (:package org )
  (:hook #'org-indent-mode
         #'visual-line-mode)
  (:option org-ellipsis " ↴"
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
           org-link-descriptive t)
  (:option org-html-doctype "xhtml5"
           org-html-html5-fancy t
           org-html-htmlize-output-type 'css)
  (:option org-superstar-special-todo-items t
           org-superstar-leading-bullet ?\s)
  (defconst org-electric-pairs
    '((?$ . ?$)))
  (add-hook 'org-mode-hook
            (defun org-add-electric-pairs ()
              (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs)
                          electric-pair-text-pairs electric-pair-pairs)))
  (defun local/disable<>pairing ()
    (setq-local electric-pair-inhibit-predicate
                (lambda (c)
                  (if (char-equal c ?<) t (electric-pair-inhibit-predicate c)))))
  (:hook local/disable<>pairing))

(setup (:package org-superstar)
  (:hook-into org-mode))

;;; programming languages ==================================
;; agda
(setup agda2-mode
  (when (executable-find "agda-mode")
    (load-file (let ((coding-system-for-read 'utf-8))
                 (shell-command-to-string "agda-mode locate"))))
  (:hook #'local/disable-aggressive-indent)
  (:option agda2-highlight-face-groups 'default-faces))

;; apl
(setup (:package gnu-apl-mode)
  (:option gnu-apl-show-tips-on-start nil))

;; c/c++
(setup cc-mode
  (:load-after eglot)
  (:hook #'eglot-ensure))

;; coq
(setup (:package proof-general)
  (defun local/setup-pg-faces ()
    "Setup faces for Proof General."
    ;; thanks david
    (set-face-background 'proof-locked-face "#90ee90"))

  (defun local/coq-init ()
    "Some initializations for `coq-mode'."
    (setq-local tab-width proof-indent))
  (:option proof-splash-enable nil
           proof-three-window-enable t
           proof-three-window-mode-policy 'smart)
  (:with-mode coq-mode
    (:hook #'local/setup-pg-faces
           #'local/coq-init)))

;; lisp
(setup (:package sly)
  (:with-mode lisp-mode
    (:hook #'sly-editing-mode)))

(setup (:package racket-mode geiser-racket))

(setup (:package geiser geiser-guile)
  (:hook-into scheme-mode
              racket-mode))

;; haskell
(setup (:package haskell-mode)
  (:load-after eglot)
  (:hook #'eglot-ensure
         #'interactive-haskell-mode)
  (:option haskell-completing-read-function #'completing-read))

;; java
(setup antlr-mode
  (:file-match "\\.g4\\'"))

;; nix
(setup (:package nix-mode)
  (defconst nix-electric-pairs
    '(("let" . " in")
      (?= . ";")))
  (defun nix-add-electric-pairs ()
    (setq-local electric-pair-pairs (append
                                     electric-pair-pairs nix-electric-pairs)
                electric-pair-text-pairs electric-pair-pairs))
  (:file-match "\\.nix\\'")
  (:hook #'nix-add-electric-pairs
         #'eglot-ensure))

(add-hook 'nix-mode-hook
          (defun nix-add-electric-pairs ()
            (setq-local electric-pair-pairs (append electric-pair-pairs nix-electric-pairs)
                        electric-pair-text-pairs electric-pair-pairs)))

;; prolog
(setup prolog
  (:file-match "\\.pl\\'"))

;; rust
(setup (:package rust-mode)
  (:load-after eglot)
  (:with-mode rust-mode
    (:hook #'eglot-ensure)))

;; ocaml
(setup (:package tuareg)
  (:hook #'eglot-ensure))

;;; config langs ===========================================
(setup (:package apache-mode))

;;; utilities ==============================================

(defun local/split-window-right ()
  "Like 'split-window-right' , but ask for buffer."
  (interactive)
  (split-window-right)
  (consult-buffer-other-window))

(defun local/split-window-below ()
  "Like 'split-window-below' , but ask for buffer."
  (interactive)
  (split-window-below)
  (consult-buffer-other-window))

(defun local/fill-line (&optional max-column char)
  "Fill rest of current line with CHAR upto column MAX-COLUMN."
  (interactive)
  (or max-column (setq max-column 60))
  (or char (setq char ?=))
  (save-excursion
    (end-of-line)
    (let* ((col (current-column))
           (n (- max-column col)))
      (if (> n 0)
          (insert (make-string n char))))))

(defun local/indent-buffer ()
  "Reformat the whole buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun local/mutate-int-at-point (f)
  "Replace integer at point with the result of calling F on it."
  (interactive "aEnter a function:")
  (let ((n (thing-at-point 'number)))
    (if (numberp n)
        (save-excursion
          (message (symbol-name (type-of f)))
          (skip-chars-backward "-0123456789")
          (if (looking-at "-?[0-9]+")
              (let* ((start (point))
                     (end (match-end 0)))
                (delete-region start end)
                (insert (number-to-string (funcall f n))))))
      (message "No number at point"))))

(defun local/inc-at-point ()
  "Increment integer at point."
  (interactive)
  (local/mutate-int-at-point #'1+))

(defun local/dec-at-point ()
  "Decrement integer at point."
  (interactive)
  (local/mutate-int-at-point #'1-))

(defun local/eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell t))

(defun local/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer nil))

(defun local/wrap-region (start end c replace)
  "Wrap region from START to END in (car C) and (cdr c).
If REPLACE is non-nil, replace the first and last characters in
region."
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (when replace
      (delete-char 1))
    (insert-char (car c))
    (goto-char (point-max))
    (when replace
      (delete-char -1))
    (insert-char (cdr c))))

(defun local/replace-round (start end arg)
  "Wrap region from START to END in parentheses.
Replaces the first and last characters.  With prefix argument
ARG, simply wrap the region."
  (interactive "*r\nP")
  (local/wrap-region start end '(?\( . ?\) ) (not arg)))

(defun local/replace-curly (start end arg)
  "Wrap region from START to END in curly braces.
Replaces the first and last characters.  With prefix argument
ARG, simply wrap the region."
  (interactive "*r\nP")
  (local/wrap-region start end '(?\{ . ?\} ) (not arg)))

(defun local/replace-square (start end arg)
  "Wrap region from START to END in square brackets.
Replaces the first and last characters.  With prefix argument
ARG, simply wrap the region."
  (interactive "*r\nP")
  (local/wrap-region start end '(?\[ . ?\] ) (not arg)))

;;; registers ==============================================

(set-register ?d '(file . "~/.dotfiles"))
(set-register ?U '(file . "/ssh:uni:.www/"))
(set-register ?u '(file . "~/data/uni/lv"))
(set-register ?b '(file . "/ssh:blog:~"))

;;; keybindings ============================================

(global-set-key (kbd "M-(") #'local/replace-round)
(global-set-key (kbd "M-{") #'local/replace-curly)
(global-set-key (kbd "M-[") #'local/replace-square)
(global-set-key (kbd "M-)") #'delete-pair)

(setup (:package meow)
  (require 'meow)
  (:option meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
           meow-use-clipboard t
           meow-expand-hint-remove-delay 0)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("h" . meow-left)
   '("l" . meow-right)
   '("<escape>" . ignore))
  (meow-leader-define-key
   '("j"   . "H-j")
   '("k"   . "H-k")
   '("h"   . "H-h")
   '("l"   . "H-l")
   '("u"   . undo-redo)
   '(":"   . execute-extended-command)
   '(";"   . pp-eval-expression)
   '("."   . find-file)
   '(","   . switch-to-buffer)
   '("TAB" . hs-toggle-hiding)
   '("k" . local/kill-this-buffer)
   '("r" . jump-to-register)
   '("b" . compile)
   '("B" . recompile)
   '("wh" . windmove-left)
   '("wj" . windmove-down)
   '("wk" . windmove-up)
   '("wl" . windmove-right)

   '("1"   . meow-digit-argument)
   '("2"   . meow-digit-argument)
   '("3"   . meow-digit-argument)
   '("4"   . meow-digit-argument)
   '("5"   . meow-digit-argument)
   '("6"   . meow-digit-argument)
   '("7"   . meow-digit-argument)
   '("8"   . meow-digit-argument)
   '("9"   . meow-digit-argument)
   '("0"   . meow-digit-argument)
   '("/"   . meow-keypad-describe-key)
   '("?"   . meow-cheatsheet))

  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))
  (meow-global-mode t))

;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)

;;; init.el ends here
