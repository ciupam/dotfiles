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

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-buffer-or-recentf)
         ;; Don't need listing buffers
         ("C-x C-b" . counsel-buffer-or-recentf)
         ("C-x C-f" . counsel-find-file)
         ("C-x p g" . counsel-git-grep)
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

(use-package which-key
  :config
  (which-key-mode))

;; (use-package lsp-mode
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;          ;; (python-mode . lsp-enable-which-key-integration)
;;          (js-mode . lsp-enable-which-key-integration)
;;          (typescript-mode . lsp-enable-which-key-integration)
;;          (c-mode . lsp-enable-which-key-integration)
;;          (c++-mode . lsp-enable-which-key-integration)
;;          ;; if you want which-key integration
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)

;; ;; optionally
;; (use-package lsp-ui :commands lsp-ui-mode)
;; ;; if you are helm user
;; ;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; ;; if you are ivy user
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; ;; optionally if you want to use debugger
;; (use-package dap-mode)
;; ;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-pyright)
;;                           (lsp-enable-which-key-integration))))  ; or lsp-deferred

(use-package org)

(use-package direnv
  :config
  (direnv-mode))

(use-package pipenv
  :hook (python-mode . pipenv-mode))
