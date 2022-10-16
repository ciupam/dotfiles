;;; init.el --- Basic emacs setup
;;; Commentary:
;;; Code:


(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(setq tab-always-indent 'complete)
(setq create-lockfiles nil)
(setq user-emacs-directory (expand-file-name "~/.cache/emacs"))
(setq backup-directory-alist
      `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
(setq auto-save-list-file-prefix
      (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))
(setq use-dialog-box nil)
(setq global-auto-revert-non-file-buffers t)

(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (set-fringe-mode 10)
      (set-face-attribute 'default nil :font "JetBrains Mono" :height 145))
  (xterm-mouse-mode 1)
  (global-set-key [mouse-4] 'scroll-down-line)
  (global-set-key [mouse-5] 'scroll-up-line))

(tooltip-mode -1)
(menu-bar-mode -1)
(column-number-mode)
(global-display-line-numbers-mode 1)
(global-auto-revert-mode 1)
(dolist (mode '(org-mode-hook
                term-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(add-to-list 'load-path "~/.emacs.d/lisp")
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)


(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file 'noerror 'nomessage)


(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)


;; Save what you enter into minibuffer prompts
(setq history-length 25)
(savehist-mode 1)


(save-place-mode 1)


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


(require 'autopair (expand-file-name "~/.emacs.d/lisp/autopair/autopair.el"))
(autopair-global-mode)


(use-package which-key
    :config
    (which-key-mode))


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


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (
         (js-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (rustic-mode . lsp-deferred)
         (c++-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config (progn
            (setq lsp-pylsp-plugins-pylint-enabled t
                  lsp-pylsp-plugins-pycodestyle-enabled t
                  lsp-pylsp-plugins-yapf-enabled t
                  lsp-prefer-flymake t
                  lsp-headerline-breadcrumb-enable nil)))
(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)


(use-package dap-mode
  :config
  (setq dap-auto-configure-features '(sessions locals controls tooltip)))


(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy))

(require 'dap-python)


;;;
(provide 'init)
;;; init.el ends here
