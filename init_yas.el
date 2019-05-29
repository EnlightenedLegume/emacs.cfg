(use-package yasnippet
  :config
  (add-hook 'yas-global-mode-hook
	    (lambda ()
	      (setq yas-snippet-dirs
		    '("c:/Users/bdhua/HOME/.emacs.d/snippets")))))
(yas-global-mode 1)


