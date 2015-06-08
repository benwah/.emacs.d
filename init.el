;; Global settings
;; ---------------
(electric-pair-mode 1)              ; Automagically close parenthesis / brackets.
(delete-selection-mode -1)          ; Deletes content of marked-text when typing.
(show-paren-mode 1)                 ; Highlight matching parenthesis.
(setq show-paren-style -1)          ; Highlight content of brackets.
(column-number-mode 1)              ; Display column number
(setq make-backup-files -1)         ; stop creating backup~ files
(setq auto-save-default -1)         ; stop creating #autosave# files
(desktop-save-mode 1)               ; Save / restore opened files.
(menu-bar-mode -1)                  ; No menu bar
(tool-bar-mode -1)                  ; No toolbar
(scroll-bar-mode -1)                ; No scrollbar
(global-set-key "\M-w" 'clipboard-kill-ring-save) ; Play well with linux clipboard
(global-set-key "\C-y" 'clipboard-yank)
(setq x-select-enable-clipboard t)
(if window-system
    (global-set-key "\C-z" nil))
(setq-default
 show-trailing-whitespace t
 )

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
    (use-package exec-path-from-shell
      :ensure t
      :init
      (exec-path-from-shell-initialize)
      ))

(use-package powerline
  :ensure t
  :init
  (powerline-default-theme)           ; Pretty bar at the bottom.
  )

;; (use-package tabbar
;;   :ensure t
;;   :init
;;   (tabbar-mode)
;;   )

(use-package fill-column-indicator
  :ensure t
  )


;; Theme:
;; ------
(use-package color-theme
  :ensure t
  :init
  (color-theme-initialize)
  (color-theme-billw)
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
  (setq scss-compile-at-save nil)
  )

(use-package slim-mode
  :ensure t
  )

(use-package rainbow-mode
  :ensure t
  :init
  (add-hook 'css-mode-hook 'custom-css-mode-hook)
  (defun custom-css-mode-hook ()
    (rainbow-mode 1))
  )

(use-package flymake
  :init
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init))
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

(use-package projectile
  :ensure t
  :init
  (projectile-global-mode)
  )

;; Python
(use-package python-mode
  :init
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "~/.emacs.d/pychecker.sh"  (list local-file))))

  (add-hook 'python-mode-hook
            (lambda ()
              (setq-default indent-tabs-mode nil)
              (setq tab-width 4)
              (setq-default fill-column 79)
              (flymake-mode)
              (setq indent-region-function nil)
              (fci-mode t)
              (flymake-pyflakes-init)
              ))
  (add-hook 'python-mode-hook 'auto-fill-mode 80)
  )

;; Coffeescript
(use-package coffee-mode
  :ensure t
  :ensure flymake-coffee
  :init
  (add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
  (custom-set-variables '(coffee-tab-width 2))
  (add-hook 'coffee-mode-hook
            'flymake-coffee-load
            )
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
	     (hs-minor-mode)
	     (set-fill-column 120)
             ))
  (add-hook 'ruby-mode-hook 'flymake-ruby-load)
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

;; Key bindings.
;; -------------
(global-set-key (kbd "C-c r")
                '(lambda () (interactive) (load-file "~/.emacs.d/init.el")))
(global-set-key [C-f12] (lambda() (interactive)(kill-other-buffers)))
(global-set-key (kbd "C-x +") 'text-scale-increase)
(global-set-key (kbd "C-x =") 'text-scale-increase)
(global-set-key (kbd "C-x -") 'text-scale-decrease)
(global-set-key (kbd "C-c S") 'hs-show-all)
(global-set-key (kbd "C-c H") 'hs-hide-all)
(global-set-key (kbd "C-c s") 'hs-show-block)
(global-set-key (kbd "C-c h") 'hs-hide-block)


(provide 'init)
