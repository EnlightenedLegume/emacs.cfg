;; org-mode
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
; With indent/visual line mode, we don't want column enforce mode
(add-hook 'org-mode-hook (lambda () (column-enforce-mode -1)))

;; date exporting
(require 'ox)
(add-to-list 'org-export-filter-timestamp-functions 'legume/rm-braces)
(defun legume/rm-braces (trans back _comm)
  "Remove <> around time-stamps."
  (pcase back
    ((or `jekyll `html)
     (replace-regexp-in-string "&[lg]t;" "" trans))
    (`latex
     (replace-regexp-in-string "[<>]" "" trans))))
;; custom date format
(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats
      '("<%B %d>" . "<%m/%d/%y %a [%H:%M]>"))

;; load up other packages
(require 'org-alert)
(setq alert-default-style 'libnotify)
(require 'org-ref)

;; Org-mode comments
(use-package poporg
  :bind (("C-c /" . poporg-dwim)))
(defun legume/set-input-method-tex ()
    (set-input-method "TeX"))
(add-hook 'poporg-mode-hook 'legume/set-input-method-tex)

;; Source code editing
(defun legume/org-insert-src (lang)
  (interactive "MLanguage: ")
  (let ((src (concat "#+BEGIN_SRC " lang "\n\n\t#+END_SRC")))
    (insert src)))
