(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(menu-bar-mode -1)

(set-face-attribute 'default nil :font "Fira Code Retina" :height 130)

;; Make ESC quit prompts

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

;; Files created by packages
;; Could be prob fixed by pck "no-littering"

(setq projectile-known-projects-file
      (expand-file-name "tmp/projectile-bookmarks.eld" user-emacs-directory))


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

(add-to-list 'load-path "~/.emacs.d/lisp/autopair") ;; comment if autopair.el is in standard load path
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

;; Move lines vscode style

(use-package move-text)
(move-text-default-bindings)

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
  :init (load-theme 'doom-badger t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-modenit
  :config
  (setq which-key-idle-delay 0.3))

;; More precise help text

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-descrive-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

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
  (setq projectile-switch-project-action #'projectile-dired))

;; Magit

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

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

(use-package flycheck
  :init (global-flycheck-mode)
  (setq flycheck-flake8rc ".flake8"))

(add-hook 'after-init-hook #'global-flycheck-mode)

;; (defclass eglot-deno (eglot-lsp-server) ()
;;   :documentation "A custom class for deno lsp.")

;; (cl-defmethod eglot-initialization-options ((server eglot-deno))
;;   "Passes through required deno initialization options"
;;   (list :enable t
;;         :lint t))

;; (add-to-list 'eglot-server-programs '((js-mode typescript-mode) . (eglot-deno "deno" "lsp")))


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


(defhydra hydra-buffer-menu (:color pink
                             :hint nil)
  "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
  ("m" Buffer-menu-mark)
  ("u" Buffer-menu-unmark)
  ("U" Buffer-menu-backup-unmark)
  ("d" Buffer-menu-delete)
  ("D" Buffer-menu-delete-backwards)
  ("s" Buffer-menu-save)
  ("~" Buffer-menu-not-modified)
  ("x" Buffer-menu-execute)
  ("b" Buffer-menu-bury)
  ("g" revert-buffer)
  ("T" Buffer-menu-toggle-files-only)
  ("O" Buffer-menu-multi-occur :color blue)
  ("I" Buffer-menu-isearch-buffers :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))

(define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)

;;; using ido find file in tag files
;; (defun tags-extra-get-all-tags-files ()
;;   "Return all, fully qualified, file names."
;;   (save-excursion
;;     (let ((first-time t)
;;           (res nil))
;;       (while (visit-tags-table-buffer (not first-time))
;;         (setq first-time nil)
;;         (setq res
;;               (append res (mapcar 'expand-file-name (tags-table-files)))))
;;       res)))

;; (defun ido-find-file-in-tag-files ()
;;   (interactive)
;;   (find-file
;;    (expand-file-name
;;     (ido-completing-read
;;      "Files: " (tags-extra-get-all-tags-files) nil t))))
