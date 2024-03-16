;;; init.el --- Basic emacs setup
;;; Commentary:
;;; Code:

(load "~/.emacs.d/general.el")
(load "~/.emacs.d/packages.el")
(add-to-list 'load-path "~/.emacs.d/lisp")

(set-variable 'c-file-style "gnu")

(add-to-list 'load-path "~/.emacs.d/lisp/autopair")
(require 'autopair)
(autopair-global-mode)

(add-to-list 'load-path "~/.emacs.d/lisp/pcap-mode")
(require 'pcap-mode)

;;;
(provide 'init)
;;; init.el ends here
