;; AucTeX
(server-start)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(put 'upcase-region 'disabled nil)
(add-hook 'emacs-lisp-mode-hook 'turn-on-font-lock)
