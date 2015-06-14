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
(electric-pair-mode 1)              ; Automagically close parenthesis / brackets.
(delete-selection-mode -1)          ; Deletes content of marked-text when typing.
(show-paren-mode 1)                 ; Highlight matching parenthesis.
(column-number-mode 1)              ; Display column number
(desktop-save-mode 1)               ; Save / restore opened files.
(menu-bar-mode -1)                  ; No menu bar
(tool-bar-mode -1)                  ; No toolbar
(scroll-bar-mode -1)                ; No scrollbar
(setq x-select-enable-clipboard t)
(setq make-backup-files -1)         ; stop creating backup~ files
(setq auto-save-default -1)         ; stop creating #autosave# files
(defvar show-paren-style -1)          ; Highlight content of brackets.
(defvar whitespace-style (quote (face trailing empty tabs)))


;; Load-path
;; ---------
(add-to-list 'load-path "~/.emacs.d/lib")
(add-to-list 'load-path "~/.emacs.d/secret")


;; "Custom" settings
;; -----------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


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
      (exec-path-from-shell-initialize)
      ))

(use-package powerline
  :ensure t
  :init
  (powerline-default-theme)           ; Pretty bar at the bottom.
  )

(use-package fill-column-indicator
  :ensure t
  )

;; Theme:
;; ------

(use-package monokai-theme
  :ensure t
  :init
  (load-theme 'monokai t)
  )

(set-face-attribute 'default nil :height 100 :family "Inconsolata")


;; Modes:
;; ------
(use-package less-css-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist
               '("\\.less\\'" . less-css-mode))
  )

(use-package scss-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist
               '("\\.scss\\'" . less-css-mode))
  (defvar scss-compile-at-save nil)
  )

(use-package slim-mode
  :ensure t
  )

(use-package rainbow-mode
  :ensure t
  :init
  (add-hook 'css-mode-hook
            (lambda()
              (whitespace-mode)
              (rainbow-mode 1)
              (set-fill-column 120)
              (fci-mode t)
            ))
  )


(use-package go-mode
  :ensure t
  :init
  (require 'go-mode-autoloads)
  )

(use-package flymake
  :init
  (global-set-key (kbd "C-;") 'flymake-display-err-menu-for-current-line)
  (global-set-key (kbd "C-c n") 'flymake-goto-next-error)
  (global-set-key (kbd "C-c p") 'flymake-goto-prev-error)
  )

;; Figure this shit out:

;; (use-package helm
;;   :ensure t
;;   :init
;;   (require 'helm-config)
;;   (helm-mode t)
;;   ;; (helm-autoresize-mode 1)
;;   ;; (setq helm-M-x-fuzzy-match t)
;;   )

;; (use-package ido
;;   :init
;;   (require 'ido)
;;   (ido-mode t)
;;   )

;; (use-package flx-ido
;;   :ensure t
;;   :init
;;     (require 'flx-ido)
;;     (ido-mode 1)
;;     (ido-everywhere 1)
;;     (flx-ido-mode 1)
;;     ;; disable ido faces to see flx highlights.
;;     (setq ido-enable-flex-matching t)
;;     (setq ido-use-faces nil)
;;   )

(defvar magit-auto-revert-mode nil)
(defvar magit-last-seen-setup-instructions "1.4.0")
(use-package magit
  :ensure t
  :init
  )

(use-package projectile
  :ensure t
  :init
  (projectile-global-mode)
  )

(use-package flycheck
  :ensure t
  )

;; Emacs lisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (flymake-mode)
            (flycheck-mode)
            (whitespace-mode)
            (set-fill-column 80)
            (fci-mode t)
            ))

;; Python
(use-package python-mode
  :init
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init))
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "~/.emacs.d/bin/pychecker.sh"  (list local-file))))

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
  )

;; Coffeescript
(use-package coffee-mode
  :ensure t
  :ensure flymake-coffee
  :init
  (add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
  (custom-set-variables '(coffee-tab-width 2))
  (add-hook 'coffee-mode-hook
            (lambda ()
              (whitespace-mode)
              (flymake-coffee-load)
              (set-fill-column 120)
              (fci-mode t)
              ))
  )

;; Ruby
(use-package ruby-mode
  :ensure flycheck
  :ensure flymake-ruby
  :init
  (setq
   ruby-deep-indent-paren nil
   ruby-indent-tabs-mode nil
   ruby-insert-encoding-magic-comment nil
   ruby-insert-encoding-magic-comment nil
   ruby-deep-indent-paren nil
   )
  (eval-after-load "hideshow"
    '(add-to-list 'hs-special-modes-alist
                  `(ruby-mode
                    ,(rx (or "do" "if" "def" "class" "module" "{" "[")) ; Block start
                    ,(rx (or "}" "]" "end"))                            ; Block end
                    ,(rx (or "#" "=begin"))                             ; Comment start
                    ruby-forward-sexp nil)))
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist
               '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))
  (add-hook 'ruby-mode-hook
            (lambda()
              (flycheck-mode)
              (flymake-ruby-load)
              (hs-minor-mode)
              (whitespace-mode)
              (set-fill-column 120)
              (fci-mode t)
              ))
  )

;; SQL
(defalias 'mysql-mode
  (lambda()
    (interactive)
    (sql-mode)
    (sql-highlight-mysql-keywords)
    ))


;; Utils
;; -----
(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer
          (delq (current-buffer)
                (remove-if-not 'buffer-file-name (buffer-list)))))

;; Hipchat
(use-package jabber
  :ensure t
  :init
  (require 'hipchat)
  )


;; Key bindings.
;; -------------
(if window-system
    (global-set-key "\C-z" nil))
(global-set-key (kbd "C-c r")
                '(lambda () (interactive) (load-file "~/.emacs.d/init.el")))
(global-set-key [C-f12] (lambda() (interactive)(kill-other-buffers)))
(global-set-key (kbd "M-w")   'clipboard-kill-ring-save)
(global-set-key (kbd "C-y")   'clipboard-yank)
(global-set-key (kbd "C-x +") 'text-scale-increase)
(global-set-key (kbd "C-x =") 'text-scale-increase)
(global-set-key (kbd "C-x -") 'text-scale-decrease)
(global-set-key (kbd "C-c S") 'hs-show-all)
(global-set-key (kbd "C-c H") 'hs-hide-all)
(global-set-key (kbd "C-c s") 'hs-show-block)
(global-set-key (kbd "C-c h") 'hs-hide-block)

(provide 'init)
;;; init.el ends here


;; Bug fix
;; -------
;; Stupid bug here: https://github.com/alpaker/Fill-Column-Indicator/issues/31
(make-variable-buffer-local 'line-move-visual)
(defadvice previous-line (around avoid-jumpy-fci activate)
  (if (and (symbol-value 'fci-mode) (> (count-lines 1 (point)) 0))
      (prog (fci-mode -1) ad-do-it (fci-mode 1))
    ad-do-it))
