;;; init.el --- Basic emacs setup
;;; Commentary:
;;; Code:

(load "~/.emacs.d/general.el")
(load "~/.emacs.d/packages.el")
(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'autopair (expand-file-name "~/.emacs.d/lisp/autopair/autopair.el"))
(autopair-global-mode)

;;;
(provide 'init)
;;; init.el ends here
