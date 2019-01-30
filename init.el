;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;EMACS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;MELPA;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; use-package package
(eval-when-compile
  (require 'use-package))

;;AESTHETICS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq my-def-theme 'base16-monokai)
(load-theme my-def-theme t)
(set-frame-font "Iosevka Type" nil t)
(when window-system (set-frame-size (selected-frame) 120 60))
;; Disable tool bar
(tool-bar-mode -1)
;; Change cursor in non-active window
(defun non-active-cursor ()
  (setq cursor-in-non-selected-windows '(hbar . 5)))
(add-hook 'window-configuration-change-hook 'non-active-cursor)

;; Load remote/tramp connection settings/functions
(load "~/.emacs.d/init_remote.el")

;; Load PDF printing function
(load "~/.emacs.d/init_print.el")

;; Load hydras
(load "~/.emacs.d/init_hydra.el")

;; Load my git functions
(load "~/.emacs.d/init_git.el")

;; Load org configs
(load "~/.emacs.d/init_org.el")

;; Load pretty symbols config
(load "~/.emacs.d/init_pretty.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MODES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; google-this
(google-this-mode 1)

;; fsharp-mode
(unless (package-installed-p 'fsharp-mode)
  (package-install 'fsharp-mode))
(require 'fsharp-mode)
(setq inferior-fsharp-program
      (concat (concat "\"C:/Program Files (x86)/Microsoft Visual Studio/2017/"
		      "BuildTools/Common7/IDE/CommonExtensions/Microsoft/")
	      "FSharp/fsi.exe\""))
(setq fsharp-compiler
      (concat (concat "\"C:/Program Files (x86)/Microsoft Visual Studio/2017/"
		      "BuildTools/Common7/IDE/CommonExtensions/Microsoft/")
	      "FSharp/fsc.exe\""))

;; merlin
(use-package merlin
  :config 
  (setq merlin-command "~/.emacs.d/merlin-start.bat"))

;; cider
(use-package cider
  :config
  (setq cider-default-repl-command "lein"))

;; undo tree
(require 'undo-tree)
(defun undo-tree-visualizer-config ()
  "Rebinds keys for more natural use of tree visualizer"
  (local-set-key (kbd "<return>") 'undo-tree-visualizer-quit))
(add-hook 'undo-tree-visualizer-mode-hook 'undo-tree-visualizer-config)
(global-undo-tree-mode)

;; ispell
(setq ispell-program-name (executable-find "hunspell"))
(setq ispell-dictionary "american")
(setq ispell-local-dictionary "american")
(setq ispell-dictionary-alist '(
       ("american"
           "[[:alpha:]]"
           "[^[:alpha:]]"
           "[']"
           t
           ("-d" "en_US")
           nil
           iso-8859-1)))
(require 'ispell)

;; Column enforcing
(require 'column-enforce-mode)
(setq column-enforce-column 80)
(setq default-fill-column 80)
(add-hook 'prog-mode-hook 'column-enforce-mode)
(add-hook 'text-mode-hook 'column-enforce-mode)
(setq column-number-mode t)

;;GOD MODE
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-mode-all)
;; Function to change the cursor look on entry and exit to god-mode-all
(defun god-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'hollow
                      'box))
  (set-cursor-color (if (or god-local-mode buffer-read-only)
			"chartreuse"
		      "#f92672")))
(defun god-enabled-cursor ()
  (set-cursor-color "chartreuse")
  (setq cursor-type 'hollow))  
(defun god-disabled-cursor ()
  (interactive)
  (set-cursor-color "#f92672")
  (setq cursor-type 'box))
(add-hook 'god-mode-enabled-hook 'god-enabled-cursor)
(add-hook 'god-mode-disabled-hook 'god-disabled-cursor)

;; AucTeX
(server-start)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(put 'upcase-region 'disabled nil)
(add-hook 'emacs-lisp-mode-hook 'turn-on-font-lock)

;; Company mode 
(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (global-set-key (kbd "M-s M-s")  'company-complete))

;; MATLAB-mode
(add-to-list 'load-path "~/.emacs.d/matlab")
(load-library "matlab-load")

;; Enable CEDET feature support for MATLAB code. (Optional)
;; (matlab-cedet-setup)

;; Verilog
(autoload 'verilog-mode "verilog-mode" "Verilog mode" t )
(add-to-list 'auto-mode-alist '("\\.[ds]?vh?\\'" . verilog-mode))


;; DO NOT TOUCH: CUSTOM
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-electric-sub-and-superscript t)
 '(custom-safe-themes
   (quote
    ("e297f54d0dc0575a9271bb0b64dad2c05cff50b510a518f5144925f627bb5832" "5a39d2a29906ab273f7900a2ae843e9aa29ed5d205873e1199af4c9ec921aaab" "e1498b2416922aa561076edc5c9b0ad7b34d8ff849f335c13364c8f4276904f0" "39fe48be738ea23b0295cdf17c99054bb439a7d830248d7e6493c2110bfed6f8" "c968804189e0fc963c641f5c9ad64bca431d41af2fb7e1d01a2a6666376f819c" "6de7c03d614033c0403657409313d5f01202361e35490a3404e33e46663c2596" "ed317c0a3387be628a48c4bbdb316b4fa645a414838149069210b66dd521733f" "c158c2a9f1c5fcf27598d313eec9f9dceadf131ccd10abc6448004b14984767c" default)))
 '(desktop-save-mode t)
 '(initial-buffer-choice "C:/Users/bdhua/Documents/todo/ongoing.org")
 '(org-agenda-files
   (quote
    ("c:/Users/bdhua/Documents/research/liberty/notes.org" "c:/Users/bdhua/Documents/research/liberty/reports/06-11-2018.org" "c:/Users/bdhua/Documents/todo/ongoing.org" "c:/Users/bdhua/Documents/todo/events.org" "c:/Users/bdhua/Documents/todo/daily.org")))
 '(org-icalendar-use-deadline (quote (event-if-not-todo event-if-todo todo-due)))
 '(org-icalendar-use-scheduled (quote (event-if-not-todo event-if-todo todo-start)))
 '(package-selected-packages
   (quote
    (cubicaltt google-maps google-translate google-this auto-complete merlin tuareg poporg ggtags cider undo-tree spinner slime-company simple-httpd queue purty-mode purple-haze-theme punpun-theme origami orgtbl-join orgtbl-ascii-plot org-ref org-plus-contrib org-beautify-theme org-alert org memoize js2-mode green-phosphor-theme god-mode flyspell-correct-popup flycheck django-snippets django-mode django-manage column-enforce-mode clojure-mode base16-theme avy auctex ample-theme 0blayout)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-code ((t (:foreground "#f4bf75" :family "Overpass Mono"))))
 '(org-date ((t (:foreground "gold" :underline nil))))
 '(org-document-title ((t (:inherit org-level-1 :foreground "aquamarine1" :underline nil :weight bold :height 1.75 :foundry "slab" :family "Iosevka"))))
 '(org-done ((t (:foreground "#a6e22e" :strike-through t :weight extra-light :foundry "Slab" :family "Iosevka"))))
 '(org-level-1 ((t (:inherit default :distant-foreground "SkyBlue1" :foreground "indian red" :slant normal :weight semi-bold :height 1.25 :width normal :foundry "slab" :family "Iosevka"))))
 '(org-level-2 ((t (:inherit outline-2 :foreground "SteelBlue3" :box (:line-width 5 :color "#272822") :slant normal :weight semi-bold :height 1 :width normal :foundry "slab" :family "Iosevka"))))
 '(org-level-3 ((t (:inherit default :foreground "peru"))))
 '(org-special-keyword ((t (:foreground "dim gray"))))
 '(org-tag ((t (:inherit org-todo :foreground "plum" :weight light :height 0.75))))
 '(org-todo ((t (:foreground "#f92672" :weight extra-light :foundry "slab" :family "Iosevka")))))
(put 'set-goal-column 'disabled nil)
