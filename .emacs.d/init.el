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
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(column-number-mode)
(winner-mode 1)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                term-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "Fira Code Retina" :height 130)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(add-to-list 'load-path "~/.emacs.d/lisp")
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(load custom-file t)


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
         (python-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (yaml-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (c++-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config (progn
            (setq lsp-pylsp-plugins-pylint-enabled t
                  lsp-prefer-flymake t)))
(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package yaml-mode)


(use-package dap-mode)

;; CPP
;; (defun my-c++-mode-hook ()
;;   (setq c-basic-offset 4)
;;   (c-set-offset 'substatement-open 0))
;; (add-hook 'c++-mode-hook 'my-c++-mode-hook)
;; (use-package clang-format
;;   :init
;;   (fset 'c-indent-line-or-region 'clang-format-region))
;; clang-format-region))
;; (setq clang-format-style "file")

;; FLYCHECK
;; (use-package flycheck
;;   :init (global-flycheck-mode)
;;   (setq flycheck-flake8rc ".flake8"))
;; Configure Flymake for Python
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; FLYMAKE
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
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
(add-hook 'python-mode-hook '(lambda () (flymake-mode)))


(use-package hydra)
(defhydra hydra-window
  (global-map "C-u" :timeout 0)
  ("u" winner-undo)
  ("U" winner-redo))

;;;
(provide 'init)
;;; init.el ends here
