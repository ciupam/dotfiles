;;; init.el --- Basic emacs setup
;;; Commentary:
;;; Code:
(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(menu-bar-mode -1)

(set-face-attribute 'default nil :font "Fira Code Retina" :height 130)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Default load path
(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'package)
(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			             ("melpa-stable" . "https://stable.melpa.org/packages/")
			             ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Some config stuff
(column-number-mode)
(global-display-line-numbers-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; First try to indent the current line, and if the line
;; was already indented, then try `completion-at-point'
(setq tab-always-indent 'complete)

;; Ignore directories using projectile search grep

(eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "node_modules")
     (add-to-list 'grep-find-ignored-directories ".mypy_cache")))

;; Backup and autosave files

(setq create-lockfiles nil)
(setq user-emacs-directory (expand-file-name "~/.cache/emacs"))

(setq backup-directory-alist
      `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix
      (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

;; Keep customization settings in a temporary file

(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; Disable line numbers for some modes

(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; FIXME: mby exwm will solve the problem
;; path currently is exported in .bashrc so its kinda hacky

(use-package exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Autopair

(require 'autopair (expand-file-name "~/.emacs.d/lisp/autopair/autopair.el"))
(autopair-global-mode)

;; Ivy packages

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ;; Don't need listing buffers
         ("C-x C-b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package swiper)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :init (load-theme 'doom-old-hope t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Managing projects the easy way
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/bin")
    (setq projectile-project-search-path '("~/bin")))
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-known-projects-file
        (expand-file-name "tmp/projectile-bookmarks.eld" user-emacs-directory)))

;; Magit
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Python
;; (use-package python-mode
;;   :ensure nil
;;   :custom
;;   (python-shell-interpreter "python3"))

;; Eglot
(use-package eglot)
(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)

;; cpp stuff
(defun my-c++-mode-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(use-package clang-format
  :init
  (fset 'c-indent-line-or-region 'clang-format-region))
;;clang-format-region))
;;(setq clang-format-style "file")

;; (use-package flycheck
;;   :init (global-flycheck-mode)
;;   (setq flycheck-flake8rc ".flake8"))
;; Configure Flymake for Python
;; (add-hook 'after-init-hook #'global-flycheck-mode)

(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

;; Disable legacy flymake
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
;; Set as a minor mode for Python
(add-hook 'python-mode-hook '(lambda () (flymake-mode)))

;; Hydra
(use-package hydra)

(defhydra hydra-zoom ()
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(global-set-key (kbd "C-c z") 'hydra-zoom/body)

(winner-mode 1)
(defhydra hydra-window (global-map
                        "C-u"
                        :timeout 0)
  ("h" windmove-left)
  ("j" windmove-up)
  ("k" windmove-down)
  ("l" windmove-right)
  ("u" winner-undo)
  ("U" winner-redo))

;; (global-set-key (kbd "C-c w") 'hydra-window/body)

(defhydra hydra-region (global-map
                        "C-c r"
                        :timeout 0)
  ("c" comment-region)
  ("u" uncomment-region))

(defhydra hydra-magit (global-map
                       "C-c g"
                       :timeout 0)
  ("g" magit-status)
  ("b" magit-blame))

(provide 'init)
;;; init.el ends here
