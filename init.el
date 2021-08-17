;;; init.el -- Ben's init.el
;;
;; Free
;;
;;; Commentary:
;;
;; Nothing worth mentioning.

;;; Code:


;; Global settings
;; ---------------
(electric-pair-mode -1)             ; Automagically close parenthesis / brackets.
(delete-selection-mode -1)          ; Deletes content of marked-text when typing.
(show-paren-mode 1)                 ; Highlight matching parenthesis.
(column-number-mode 1)              ; Display column number
(desktop-save-mode -1)               ; Save / restore opened files.
(menu-bar-mode -1)                  ; No menu bar
(tool-bar-mode -1)                  ; No toolbar
(scroll-bar-mode -1)                ; No scrollbar
(setq x-select-enable-clipboard t)
(setq make-backup-files -1)         ; stop creating backup~ files
(setq auto-save-default -1)         ; stop creating #autosave# files
(setq require-final-newline nil)    ; Forceload- new line at EOF.
(defvar show-paren-style -1)        ; Highlight content of brackets.
(defvar whitespace-style (quote (face trailing empty tabs)))
(setq-default indent-tabs-mode nil)
(global-hl-line-mode 1)
(set-face-background hl-line-face "gray20")


;; Backup directory
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; "Custom" settings
;; -----------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)

;; Re-builder style
(require 're-builder)
(setq reb-re-syntax 'string)

;; Packaging
;; ---------

;; Set repositories
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)

;; Ensure use-package is installed:
(unless (require 'use-package nil 'noerror)
  (require 'package)
  (package-refresh-contents)
  (package-initialize)
  (package-install 'use-package)
  (require 'use-package))


;; Path
;; ----
(setenv "PATH" (concat ".:~/.emacs.d/bin" (getenv "PATH")))
(setq exec-path (append exec-path '(".:~/.emacs.d/bin")))


;; Third-party global customizations:
;; ----------------------------------
(if window-system
    (use-package "exec-path-from-shell"
      :ensure t
      :init
      (exec-path-from-shell-initialize)))

;; Until emacs 25.3: https://lists.gnu.org/archive/html/emacs-devel/2017-09/msg00211.html
(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))


(use-package fill-column-indicator
  :ensure t)

;; Theme:
;; ------
(use-package monokai-theme
  :ensure t
  :init
  (load-theme 'monokai t))

(use-package smart-cursor-color
  :ensure t
  :init
  (require 'smart-cursor-color)
  (smart-cursor-color-mode +1))

(set-face-attribute 'default nil :height 140 :family "Inconsolata")

;; Modes:
;; ------
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


(use-package yaml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package less-css-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist
               '("\\.less\\'" . less-css-mode)))

(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist
               '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist
               '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist
               '("\\.liquid\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (setq-default indent-tabs-mode nil))))

(use-package scss-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist
               '("\\.scss\\'" . less-css-mode))
  (defvar scss-compile-at-save nil))

(use-package slim-mode
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :init
  (add-hook 'css-mode-hook
            (lambda()
              (whitespace-mode)
              (rainbow-mode 1)
              (set-fill-column 120)
              (fci-mode t))))

(use-package go-mode
  :ensure t)

(use-package flymake
  :init
  (global-set-key (kbd "C-;") 'flymake-display-err-menu-for-current-line)
  (global-set-key (kbd "C-c n") 'flymake-goto-next-error)
  (global-set-key (kbd "C-c p") 'flymake-goto-prev-error))


(use-package osx-clipboard
  :ensure t
  :init
  (osx-clipboard-mode +1)
  )

(defvar magit-auto-revert-mode nil)
(defvar magit-last-seen-setup-instructions "1.4.0")
(use-package magit
  :ensure t
  :init
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
  )

(use-package projectile
  :ensure t
  :init
  (projectile-global-mode))

(use-package helm-projectile
  :ensure t
  :init
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

(use-package flycheck
  :ensure t)

;; Emacs lisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (flymake-mode)
            (flycheck-mode)
            (whitespace-mode)
            (set-fill-column 80)
            (fci-mode t)))

;; Python
(add-hook 'python-mode-hook
          (lambda ()
            (setq-default indent-tabs-mode nil)
            (setq tab-width 4)
            (flymake-mode)
            (setq indent-region-function nil)
            (fci-mode t)
            (flymake-pyflakes-init)
            (whitespace-mode)
            (setq-default fill-column 79)
            (auto-fill-mode 80)
            ))


;; SQL
(defalias 'mysql-mode
  (lambda()
    (interactive)
    (sql-mode)
    (sql-highlight-mysql-keywords)))


;; Key bindings.
;; -------------
(if window-system
    (global-set-key "\C-z" nil))
(global-set-key (kbd "C-c r")
                '(lambda () (interactive) (load-file "~/.emacs.d/init.el")))
(global-set-key [C-f12] (lambda() (interactive)(kill-other-buffers)))
(global-set-key (kbd "M-w")   'clipboard-kill-ring-save)
(global-set-key (kbd "C-y")   'clipboard-yank)
(global-set-key (kbd "C-c S") 'hs-show-all)
(global-set-key (kbd "C-c H") 'hs-hide-all)
(global-set-key (kbd "C-c s") 'hs-show-block)
(global-set-key (kbd "C-c h") 'hs-hide-block)

(global-set-key (kbd "C-M-h") 'windmove-left)
(global-set-key (kbd "C-M-l") 'windmove-right)
(global-set-key (kbd "C-M-k") 'windmove-up)
(global-set-key (kbd "C-M-j") 'windmove-down)

;; Override comint mode key binginds
(add-hook 'comint-mode-hook
          (function (lambda () 
                      (local-set-key (kbd "C-M-h") 'windmove-left)
                      (local-set-key (kbd "C-M-l") 'windmove-right)
                      (local-set-key (kbd "C-M-k") 'windmove-up)
                      (local-set-key (kbd "C-M-j") 'windmove-down))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))


(provide 'init)
;;; init.el ends here
