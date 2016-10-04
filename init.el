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
 '(coffee-tab-width 2)
 '(package-selected-packages
   (quote
    (web-mode use-package slim-mode scss-mode rainbow-mode projectile monokai-theme magit less-css-mode go-mode flymake-ruby flymake-coffee flycheck fill-column-indicator coffee-mode))))


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
  :ensure t
  :init
  (require 'go-mode-autoloads))

(use-package flymake
  :init
  (global-set-key (kbd "C-;") 'flymake-display-err-menu-for-current-line)
  (global-set-key (kbd "C-c n") 'flymake-goto-next-error)
  (global-set-key (kbd "C-c p") 'flymake-goto-prev-error))

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
              )))

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
              )))

;; Multi-cursor
;; TODO: Make C-S work and test this shit.
;; (use-package multiple-cursors
;;   :ensure t
;;   :init
;;   (require 'multiple-cursors)
;;   (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;;   (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;;   (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;;   (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;;   )

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
  :ensure flymake-ruby
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
  (setq flycheck-checker-error-threshold 800)
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist
               '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))
  (add-hook 'ruby-mode-hook
            (lambda()
              (setq-default indent-tabs-mode nil)
              (flycheck-mode)
              (flymake-ruby-load)
              (hs-minor-mode)
              (whitespace-mode)
              (set-fill-column 120)
              (fci-mode t)
              (robe-mode))))

(use-package minitest
  :ensure t
  :init
  (require 'minitest))

(custom-set-variables
 '(minitest-default-command '("dev" "test"))
 '(minitest-use-bundler nil))

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
  (let ((compilation-scroll-output t)
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
  (let ((file-name (file-relative-name (buffer-file-name) (minitest-project-root))))
    (if file-name
        (minitest-run-file file-name post-command)
      (error "Buffer is not visiting a file"))))

;; Terminal
(use-package multi-term
  :ensure t
  :init
  (require 'multi-term)
  (setq multi-term-program "/bin/bash"))

(global-set-key (kbd "C-c t") 'shell)

(add-to-list 'term-bind-key-alist
             '("M-<backspace>" . term-send-backward-kill-word))

;; SQL
(defalias 'mysql-mode
  (lambda()
    (interactive)
    (sql-mode)
    (sql-highlight-mysql-keywords)))

;; Utils
;; -----
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))

;; Custom functions
(defun gh-projects-init ()
  "Initialize gh-projects."

  ;; This provides shortcuts to launch browser-windows in order to quickly
  ;; access your projects' issues, branches and pull requests.
  ;; For this to work properly, you need to add a configuration file called
  ;; github-projects.el in your load-path somewhere.
  ;;
  ;; Sample "github-projects.el" in your load-path:

  ;; ;;; github-project.el --- Config file
  ;; ;;; Commentary:
  ;; ;;; Code:
  ;;
  ;; (defvar gh-project-list
  ;;   '(
  ;;     (
  ;;      "MyOrganization"
  ;;      "MyGithubUsername"
  ;;      (
  ;;       ("my-project1" "o")
  ;;       ("my-project2" "t")))))
  ;;
  ;; (provide 'github-projects)
  ;; ;;; github-projects.el ends here

  ;; This would define 8 shortcuts:
  ;; C-c g p o: Open my pull requests for my-project1
  ;; C-c g b o: Open my branches for my-project1
  ;; C-c g i o: Open my issues for my-project1
  ;; C-c g m o: Open master branch for my-project1
  ;; C-c g p t: Open my pull requests for my-project2
  ;; C-c g b t: Open my branches for my-project2
  ;; C-c g m t: Open master branch for my-project2

  (defun gh-open-path (organization project path)
    (browse-url
     (format "https://github.com/%s/%s/%s" organization project path)))

  (defun gh-open-master (gh-organization gh-project)
    (gh-open-path gh-organization gh-project ""))

  (defun gh-open-branches (gh-organization gh-project)
    (gh-open-path gh-organization gh-project "branches/yours"))

  (defun gh-open-prs (gh-organization gh-project gh-user)
    (gh-open-path gh-organization gh-project (format "pulls/%s" gh-user)))

  (defun gh-open-assigned-issues (gh-organization gh-project gh-user)
    (gh-open-path gh-organization gh-project (format "issues/assigned/%s" gh-user)))

  (defvar gh-project-list)
  (require 'github-projects)
  (if (boundp 'gh-project-list)
      (let (row organization username projects)
        (while gh-project-list
          (setq row (pop gh-project-list))
          (setq organization (pop row))
          (setq username (pop row))
          (setq projects (pop row))
          (let (project-row project shortcut)
            (while projects
              (setq project-row (pop projects))
              (setq project (pop project-row))
              (setq shortcut (pop project-row))

              (global-set-key (kbd (format "C-c g p %s" shortcut)) `(lambda () (interactive) (gh-open-prs ',organization ',project ',username)))
              (global-set-key (kbd (format "C-c g b %s" shortcut)) `(lambda () (interactive) (gh-open-branches ',organization ',project)))
              (global-set-key (kbd (format "C-c g i %s" shortcut)) `(lambda () (interactive) (gh-open-assigned-issues ',organization ',project ',username)))
              (global-set-key (kbd (format "C-c g m %s" shortcut)) `(lambda () (interactive) (gh-open-master ',organization ',project)))))))))
(gh-projects-init)

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
