;;;; PACKAGES

(require 'package)

(add-to-list 'package-archives
             '("MELPA Stable" . "https://stable.melpa.org/packages/"))

(add-to-list 'package-archives          
             '("MELPA" . "https://melpa.org/packages/"))

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

(unless package-archive-contents
  (package-refresh-contents))

(ensure-package-installed
 'flyspell
 ;; 'auto-complete
 'company
 'fuzzy
 'gscholar-bibtex
 'google-this
 'cuda-mode
 )
    
(package-initialize)

;;;; COSTUMIZE

;; Server (daemon mode)
(require 'server)
(unless (server-running-p)
  (server-start))

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

;; eval-buffer
(global-set-key (kbd "M-<RET>") 'eval-buffer)

;; ;; auto-complete
;; (require 'auto-complete)
;; ;; (global-auto-complete-mode t)
;; ;; (defun auto-complete-mode-maybe ()
;; ;;   "No maybe for you. Only AC!"
;; ;;   (unless (minibufferp (current-buffer))
;; ;;     (auto-complete-mode 1)))
;; (add-hook 'text-mode-hook 'auto-complete-mode)
;; (add-hook 'prog-mode-hook 'auto-complete-mode)

;; ; workaround with flyspell
;; (ac-flyspell-workaround)

;; ; show time
;; (setq ac-auto-show-menu 0.3)

;; ; color
;; (set-face-foreground 'ac-completion-face "magenta")
;; (set-face-background 'ac-selection-face "magenta")

;; ; keybinding
;; (define-key ac-mode-map (kbd "C-TAB") 'auto-complete)

;; COMPlete ANYthing
(add-hook 'after-init-hook 'global-company-mode)

; show time
(setq company-idle-delay 0.3)
(setq company-echo-delay 0)

; color
(custom-set-faces
 '(company-preview
   ((t (:foreground "brightblack" :underline t))))
 '(company-preview-common
   ((t (:inherit company-preview))))
 '(company-tooltip
   ((t (:background "white" :foreground "black"))))
 '(company-tooltip-selection
   ((t (:background "brightmagenta" :foreground "black"))))
 '(company-tooltip-common
   ((((type x)) (:inherit company-tooltip :weight bold))
    (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection
   ((((type x)) (:inherit company-tooltip-selection :weight bold))
    (t (:inherit company-tooltip-selection)))))

;; gscholar-bibtex
(setq gscholar-bibtex-default-source "Google Scholar")
(setq gscholar-bibtex-database-file "~/projects/research/bib.bib")

;; NO menu bar
(menu-bar-mode -1)

;; mode bar

; end chars
;; (setq mode-line-end-spaces "☯")
(setq mode-line-end-spaces "X")

; Line & Column number
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

; color
(set-face-foreground 'mode-line "black")
(set-face-background 'mode-line "#afffff")
(set-face-foreground 'mode-line-inactive "black")
(set-face-background 'mode-line-inactive "white")

;; vertical-border
(set-face-foreground 'vertical-border "white")
(set-face-background 'vertical-border "white")

;; shell
(defun new-shell ()
  "creates a shell with given name"
  (interactive)
  (let ((shell-name (read-string "shell name: " nil)))
    (shell (concat "*" shell-name "*"))))
(global-set-key (kbd "M-s M-s") 'new-shell)

;;;; EDITOR

;; No tabs, indentation 4
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq c-basic-offset 4)
(setq cperl-indent-level 4)

;; SHELL

; Shell indentation 8
(setq sh-basic-offset 8
      sh-indentation 8
      ;; case
      sh-indent-for-case-label 0
      sh-indent-for-case-alt '+)

; Shell pager (fix less & more)
(setenv "PAGER" "emacs-pipe")
