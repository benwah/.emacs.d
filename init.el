;; Global settings
;; ---------------
(electric-pair-mode 1)              ; Automagically close parenthesis / brackets.
(delete-selection-mode -1)          ; Deletes content of marked-text when typing.
(show-paren-mode 1)                 ; Highlight matching parenthesis.
(setq show-paren-style 'expression) ; Highlight content of brackets.
(column-number-mode 1)              ; Display column number
(setq make-backup-files -1)         ; stop creating backup~ files
(setq auto-save-default -1)         ; stop creating #autosave# files
(desktop-save-mode 1)               ; Save / restore opened files.
(menu-bar-mode -1)                  ; No menu bar
(tool-bar-mode -1)                  ; No toolbar
(scroll-bar-mode -1)                ; No scrollbar
(global-set-key "\M-w" 'clipboard-kill-ring-save) ; Play well with linux clipboard
(global-set-key "\C-y" 'clipboard-yank)
(if window-system
    (global-set-key "\C-z" nil))
(setq-default show-trailing-whitespace t)

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
      :init
      (exec-path-from-shell-initialize)
      ))

(use-package powerline
  :ensure t
  :init
  (powerline-default-theme)           ; Pretty bar at the bottom.
  )

(use-package tabbar
  :ensure t
  :ensure tabbar-ruler
  :init
  (tabbar-mode)
  (require 'cl)
  (setq tabbar-ruler-global-tabbar t) ; If you want tabbar
  (setq tabbar-ruler-global-ruler nil) ; if you want a global ruler
  (require 'tabbar-ruler)
  (tabbar-ruler-group-by-projectile-project)
  (global-set-key (kbd "C-c t") 'tabbar-ruler-move)
  )

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
  (setenv "PATH" (concat (getenv "PATH") ":~/.rbenv/shims"))
  (setq ruby-insert-encoding-magic-comment nil)
  (setq ruby-deep-indent-paren nil)
  ;; (require 'flymake-ruby)
  ;; (add-hook 'ruby-mode-hook 'rubocop-mode)
  (add-hook 'ruby-mode-hook
           (lambda()
             (flycheck-mode)
             ))
  (add-hook 'ruby-mode-hook 'flymake-ruby-load)
  )

;; Key bindings.
;; -------------
(global-set-key (kbd "C-c r")
                '(lambda () (interactive) (load-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-x +") 'text-scale-increase)
(global-set-key (kbd "C-x =") 'text-scale-increase)
(global-set-key (kbd "C-x -") 'text-scale-decrease)
