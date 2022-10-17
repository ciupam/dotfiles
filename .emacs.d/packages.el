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
                  lsp-prefer-flymake t)))
(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :config
  (setq dap-auto-configure-features '(sessions locals controls tooltip)))

(use-package rustic)

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy))
(require 'dap-python)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  ;; Dashboard does not pops when starting client
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

;; (use-package evil
;;   :config
;;   (setq evil-insert-state-cursor 'bar)
;;   (evil-mode 1))
