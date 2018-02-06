;;;;--------------------
;;;; Initialize

;; Server (daemon mode)
(require 'server)
(unless (server-running-p)
  (server-start))

;; Load loop macro
(require 'cl)


;;;;--------------------
;;;; Packages

(require 'package)
(setq package-archives
      '(
        ;("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA" . "https://melpa.org/packages/")
        ))

(package-initialize)

(defvar required-packages '(
                            flyspell
                            ;; auto-complete
                            company
                            fuzzy
                            gscholar-bibtex
                            google-this
                            cuda-mode
                            edit-server
                            ;; irony
                            ;; company-irony
                            quelpa
                            highlight-parentheses
                            shrink-whitespace
                            undo-tree
                            browse-kill-ring
                            define-word
                            )
  "A list of packages to ensure are installed at launch.")

(setq use-package-verbose t)  ;; Show package load times.

(defun required-packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (required-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Required is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; Quelpa packages
(quelpa '(emacs-pager :repo "mbriggs/emacs-pager" :fetcher github))


;;;;--------------------
;;;; Package Settings

;; gscholar-bibtex
(setq gscholar-bibtex-default-source "Google Scholar")
(setq gscholar-bibtex-database-file "~/projects/research/bib.bib")

;; shrink-whitespace
(global-set-key (kbd "M-\\") 'shrink-whitespace)

;; flyspell
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

                                        ; keybinding
(global-set-key (kbd "<f8>") 'ispell-word)
;;(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
(global-set-key (kbd "M-<f8>") 'flyspell-buffer)
;; (global-set-key (kbd "<f7>") 'flyspell-check-previous-highlighted-word)
;; (defun flyspell-check-next-highlighted-word ()
;;   "Custom function to spell check next highlighted word"
;;   (interactive)
;;   (flyspell-goto-next-error)
;;   (ispell-word)
;;   )
;; (global-set-key (kbd "<f9>") 'flyspell-check-next-highlighted-word)

;; highlight-parentheses
(add-hook 'prog-mode-hook 'highlight-parentheses-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)

;; COMPlete ANYthing
(add-hook 'after-init-hook 'global-company-mode)
                                        ; disable in shell mode
(add-hook 'shell-mode-hook (lambda () (company-mode -1)) 'append)
                                        ; show time
(setq company-idle-delay 0.2)
(setq company-tooltip-idle-delay 0.2)
(setq company-echo-delay 0)
                                        ; color
(custom-set-faces
 '(company-preview
   ((t (:foreground "#606060" :underline t))))
 '(company-preview-common
   ((t (:inherit company-preview))))
 '(company-tooltip
   ((t (:background "#e0e0e0" :foreground "#606060"))))
 '(company-tooltip-selection
   ((t (:background "#d0d0ff" :foreground "#000000"))))
 '(company-scrollbar-bg
   ((t (:background "#e0e0e0"))))
 '(company-scrollbar-fg
   ((t (:background "#606060"))))
 '(company-tooltip-search
   ((t (:background "#ffffd1"))))
 '(company-tooltip-search-selection
   ((t (:background "#ffffd1"))))
 '(company-tooltip-common
   ((((type x)) (:inherit company-tooltip :weight bold))
    (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection
   ((((type x)) (:inherit company-tooltip-selection :weight bold))
    (t (:inherit company-tooltip-selection)))))
                                        ; case sensitive
(setq company-dabbrev-downcase nil)
                                        ; show numbers
(setq company-show-numbers t)

;; edit-server
(require 'edit-server)
(edit-server-start)

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode t)

;; browse-kill-ring
(browse-kill-ring-default-keybindings)

;; define-word
(global-set-key (kbd "M-s M-d") 'define-word-at-point)
(global-set-key (kbd "M-s d") 'define-word)


;;;;--------------------
;;;; Customize

;; yes/no -> y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; forward/backward whitespace
(global-set-key (kbd "M-<right>") 'forward-whitespace)
(defun backward-whitespace ()
  "backward to last whitespace"
  (interactive)
  (forward-whitespace -1))
(global-set-key (kbd "M-<left>") 'backward-whitespace)

;; input method
(setq default-input-method 'TeX) ; todo consider math-symbol-lists package

;; eval-buffer
(global-set-key (kbd "M-<RET>") 'eval-buffer)

;; Reload .emacs.
(global-set-key (kbd "M-<f12>")
  '(lambda () (interactive) (load-file "~/.emacs")))

;; C-x k kill this buffer
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; NO menu bar
(menu-bar-mode -1)

;; mode bar
                                        ; space
(setq mode-line-front-space "☯ ")
(setq mode-line-end-spaces "☯")
;; (setq mode-line-end-spaces "X")
                                        ; line & column number
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)
                                        ; color
(set-face-foreground 'mode-line "#000000")
(set-face-background 'mode-line "#d0d0ff")
(set-face-foreground 'mode-line-inactive "#606060")
(set-face-background 'mode-line-inactive "#e0e0e0")

;; vertical-border
(set-face-foreground 'vertical-border "#e0e0e0")
(set-face-background 'vertical-border "#e0e0e0")

;; shell
(defun new-shell ()
  "creates a shell with given name"
  (interactive)
  (let ((shell-name (read-string "shell name: " nil)))
    (shell (concat "*" shell-name "*"))))
(global-set-key (kbd "M-s M-s") 'new-shell)

;; browse url
(global-set-key (kbd "M-s M-b") 'browse-url-at-point)
(global-set-key (kbd "M-s b") 'browse-url)


;;;;--------------------
;;;; Languages

;; No tabs, indentation 4
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq c-basic-offset 4)
(setq cperl-indent-level 4)
(add-hook 'json-mode-hook
          (lambda  ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 4)))

;; Bash

                                        ; Shell indentation 8
(setq sh-basic-offset 8
      sh-indentation 8
      ;; case
      sh-indent-for-case-label 0
      sh-indent-for-case-alt '+)

                                        ; Shell pager (fix less & more)
(add-to-list 'auto-mode-alist '("\\.emacs-pager$" . emacs-pager-mode))

;; C++

                                        ; irony
;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'objc-mode-hook 'irony-mode)

;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
