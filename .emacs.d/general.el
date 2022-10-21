;; Default minor modes
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode 1)
(global-display-line-numbers-mode 1)
(global-auto-revert-mode 1)
(column-number-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(set-fringe-mode 10)

;; Buffer-Global variables
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Buffer-Local Variables
(setq use-dialog-box nil)
(setq create-lockfiles nil)
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(setq tab-always-indent 'complete)
(setq global-auto-revert-non-file-buffers t)
(setq display-line-numbers-type 'relative)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(load-theme 'wombat)

;; (setq recentf-max-menu-items 25)
;; (setq recentf-max-saved-items 25)
;; (global-set-key "\C-x\ \C-r" 'recentf-open-files)
;; (recentf-mode 1)
;; (setq history-length 25)

;; Prevents loading custom file into init.el
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file 'noerror 'nomessage)

;; Cache, autosaves and backups
(setq user-emacs-directory (expand-file-name "~/.cache/emacs"))
(setq backup-directory-alist
      `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
(setq auto-save-list-file-prefix
      (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

;; Load font faces
(defun efs/set-font-faces ()
  (set 'efs/default-font-size 140)
  (set-face-attribute 'default nil :font "JetBrains Mono" :height efs/default-font-size)
  (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height efs/default-font-size)
  (set-face-attribute 'variable-pitch nil :font "JetBrains Mono" :height efs/default-font-size :weight 'regular))
(if (daemonp)
    (progn
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame (efs/set-font-faces))))
      (exec-path-from-shell-initialize))
  (efs/set-font-faces))
