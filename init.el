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
(setenv "PATH" (shell-command-to-string "source ~/.bash_profile; echo -n $PATH"))
(electric-pair-mode -1)              ; Automagically close parenthesis / brackets.
(delete-selection-mode -1)          ; Deletes content of marked-text when typing.
(show-paren-mode 1)                 ; Highlight matching parenthesis.
(column-number-mode 1)              ; Display column number
(desktop-save-mode -1)               ; Save / restore opened files.

(if (display-graphic-p)
    (progn
      (menu-bar-mode -1)                  ; No menu bar
      (tool-bar-mode -1)                  ; No toolbar
      (scroll-bar-mode -1)                ; No scrollbar
      )
)

(setq x-select-enable-clipboard t)
(setq make-backup-files -1)         ; stop creating backup~ files
(setq auto-save-default -1)         ; stop creating #autosave# files
(setq require-final-newline nil)    ; Forceload- new line at EOF.
(defvar show-paren-style -1)        ; Highlight content of brackets.
(defvar whitespace-style (quote (face trailing empty tabs)))
(setq-default indent-tabs-mode nil)
(global-hl-line-mode 1)
(set-face-background hl-line-face "gray20")

;; Load-path
;; ---------
(add-to-list 'load-path "~/.emacs.d/lib")
(add-to-list 'load-path "~/.emacs.d/secret")


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
 '(coffee-tab-width 2)
 '(minitest-default-command (quote ("dev" "test")))
 '(minitest-use-bundler nil)
 '(package-selected-packages
   (quote
    (helm-ag ag typescript graphql-mode dismal cheatsheet exec-path-from-shell emacsql moe-theme python-mode nose jinja2-mode multiple-cursors csv-mode multi-term minitest robe helm-projectile osx-clipboard markdown-mode yaml-mode smart-cursor-color eruby-mode web-mode use-package slim-mode scss-mode rainbow-mode projectile magit less-css-mode go-mode flycheck fill-column-indicator coffee-mode)))
 '(sql-mysql-login-params
   (quote
    ((user :default "")
     (server :default "")
     (database :default "")))))

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


(use-package emacsql
  :ensure t)

(use-package fill-column-indicator
  :ensure t)

;; Theme:
;; ------

(use-package moe-theme
  :ensure t
  :init
  (require 'moe-theme)
  (load-theme 'moe-dark t)
)

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
  (projectile-global-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

(use-package helm-projectile
  :ensure helm-ag
  :init
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

(use-package flycheck
  :ensure t)

;; Emacs lisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (flycheck-mode)
            (whitespace-mode)
            (set-fill-column 80)
            (fci-mode t)))

;; Python
(add-hook 'python-mode-hook
          (lambda ()
            (setq python-shell-interpreter "pyenv python")
            (setq-default indent-tabs-mode nil)
            (setq tab-width 4)
            (setq indent-region-function nil)
            (fci-mode t)
            (whitespace-mode)
            (setq-default fill-column 79)
            (auto-fill-mode 80)
            ))

;; Coffeescript
(use-package coffee-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
  (custom-set-variables '(coffee-tab-width 2))
  (add-hook 'coffee-mode-hook
            (lambda ()
              (whitespace-mode)
              (set-fill-column 120)
              (fci-mode t)
              )))

;; Ruby
(defun inf-ruby-console-dev (dir)
  "Run dev console"
  (interactive "D")
  (let ((default-directory (file-name-as-directory dir)))
    (unless (file-exists-p "dev.yml")
      (error "The directory must contain a dev.yml"))
    (run-ruby "bash -c \"source ~/.bash_profile && dev console\"")))

(defvar inf-ruby-console-patterns-alist
  '(
    ("dev.yml" . dev)
    (inf-ruby-console-rails-p . rails)
    ("*.gemspec" . gem)
    (inf-ruby-console-racksh-p . racksh)
    ("Gemfile" . default)))

(use-package ruby-mode
  :ensure flycheck
  :ensure robe
  :init
  (setq
   ruby-indent-tabs-mode nil
   ruby-insert-encoding-magic-comment nil
   ruby-insert-encoding-magic-comment nil
   ruby-deep-indent-paren nil)
  (eval-after-load "hideshow"
    '(add-to-list 'hs-special-modes-alist
                  `(ruby-mode
                    ,(rx (or "do" "if" "def" "class" "module" "{" "[")) ; Block start
                    ,(rx (or "}" "]" "end"))                            ; Block end
                    ,(rx (or "#" "=begin"))                             ; Comment start
                    ruby-forward-sexp nil)))
  (setq flycheck-checker-error-threshold 400)
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist
               '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))
  (add-hook 'ruby-mode-hook
            (lambda()
              (setq-default indent-tabs-mode nil)
              (flycheck-mode)
              (hs-minor-mode)
              (whitespace-mode)
              (set-fill-column 120)
              (fci-mode t)
              (robe-mode))))

(use-package minitest
  :ensure t
  :init
  (require 'minitest))



(define-derived-mode minitest-compilation-mode comint-mode ""
  "Override."
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(defun colorize-compilation-buffer ()
  "Override."
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun minitest--extract-str ()
  (save-excursion
    (save-restriction
      (widen)
      (end-of-line)
      (or (re-search-backward "\\(test[_A-Za-z0-9]*\\) ['\"]\\(.+\\)['\"] do" nil t)
          (re-search-backward "def \\(test\\)_\\([_A-Za-z0-9]+\\)" nil t)
          (re-search-backward "\\(it\\) '\\([^\"]+?\\)'" nil t)
          (re-search-backward "\\(it\\) \"\\([^\"]+?\\)\"" nil t)))))

(defun minitest--test-name-flag (test-name)
  (let ((flag (format "-n%s" test-name)))
    (cond (minitest-use-spring (concat "TESTOPTS=" flag))
          (t flag))))

(defun minitest-verify-single ()
  "Run on current file."
  (interactive)
  (if (minitest--extract-str)
      (let* ((cmd (match-string 1))
             (str (match-string 2))
             (post_command
              (cond
               ((equal "test" cmd) (format "test_%s" (replace-regexp-in-string " " "_" str)))
               ((equal "test_each_cart_type" cmd) (format "test_%s" (replace-regexp-in-string " " "_" str)))
               ((equal "it" cmd) str))))
        (minitest--file-command (minitest--test-name-flag post_command)))
        (error "No test found. Make sure you are on a file that has `def test_foo` or `test \"foo\"`")))

(defun minitest-test-command ()
  "Override."
  minitest-default-command)

(defun minitest-project-root ()
  "Override, use projectile to get project root."
  (or (projectile-project-root) (error "You're not into a project")))

(defun minitest--run-command (command &optional file-name)
  "Override."
  (let (
        (compilation-scroll-output t)
        (actual-command (concat
                         (format "source ~/.bash_profile && cd %s && " (minitest-project-root))
                         (or minitest-default-env "")
                         " "
                         command))
        )
    (setq minitest--last-command (list command file-name))
    (compilation-start
     actual-command
     'minitest-compilation-mode
     (lambda (arg) (minitest-buffer-name (or file-name ""))))))

(defun minitest--file-command (&optional post-command)
  "Run command on currently visited file.  POST-COMMAND are optional arguments."
  (let (
        (file-name
         (file-relative-name (file-truename (buffer-file-name)) (minitest-project-root)))
        )
    (if file-name
        (minitest-run-file file-name post-command)
      (error "Buffer is not visiting a file"))))

;; Multi-cursor
;; TODO: Make C-S work and test this shit.
(use-package multiple-cursors
  :ensure t
  :init
  (require 'multiple-cursors)
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  )

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

;; Bug fix
;; -------
;; Stupid bug here: https://github.com/alpaker/Fill-Column-Indicator/issues/31
;; (make-variable-buffer-local 'line-move-visual)
;; (defadvice previous-line (around avoid-jumpy-fci activate)
;;   (if (and (symbol-value 'fci-mode) (> (count-lines 1 (point)) 0))
;;       (prog (fci-mode -1) ad-do-it (fci-mode 1))
;;     ad-do-it))


;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(provide 'init)
;;; init.el ends here

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


(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq reb-re-syntax 'string)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'eshell-preoutput-filter-functions
          'ansi-color-filter-apply)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(defun ansi-color-show (&optional beg end)
  "Interpret ANSI color esacape sequence by colorifying cotent.
Operate on selected region on whole buffer."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (ansi-color-apply-on-region beg end))
