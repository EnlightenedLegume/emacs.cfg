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

;; TURN TABS OFF
(setq-default indent-tabs-mode nil)

;;AESTHETICS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq my-def-theme 'base16-monokai)
(load-theme my-def-theme t)
(set-frame-font "Iosevka Type" nil t)
;; For things that aren't safe on text terminals
;; (when window-system
;;   (set-frame-size (selected-frame) 120 60)
;;   (define-key input-decode-map "\C-i" [C-i]))
;; Disable tool bar
(tool-bar-mode -1)
;; Change cursor in non-active window
(defun non-active-cursor ()
  (setq cursor-in-non-selected-windows '(hbar . 5)))
(add-hook 'window-configuration-change-hook 'non-active-cursor)

;; Scrolling
(require 'golden-ratio-scroll-screen)
(global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
(global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up)
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)
(pixel-scroll-mode 1)

;; Sublimity map
(require 'sublimity)
(require 'sublimity-map)

;;LOAD OTHER FILES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load remote/tramp connection settings/functions
(load "~/.emacs.d/init_remote.el")

;; Load PDF printing function
(load "~/.emacs.d/init_print.el")

;; Load hydras
;; (load "~/.emacs.d/init_hydra.el")

;; Load my git functions
(load "~/.emacs.d/init_git.el")

;; Load org configs
(load "~/.emacs.d/init_org.el")

;; Load pretty symbols config
(load "~/.emacs.d/init_pretty.el")

;; Load emacs settings
(load "~/.emacs.d/init_latex.el")

;; Load yas
(load "~/.emacs.d/init_yas.el")

;; Load c indentation styles
(load "~/.emacs.d/init_indent.el")

;; Load better buffer switching
(load "~/.emacs.d/init_buffers.el")

;; Load lsp mode config
(load "~/.emacs.d/init_lsp.el")

;; Load modalka
(load "~/.emacs.d/init_modalka.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MODES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; poporg
(autoload 'poporg-dwim "poporg" nil t)
(global-set-key "\C-ceo" 'poporg-dwim)

;; Symbol overlay
(require 'symbol-overlay)
(global-set-key (kbd "M-i") 'symbol-overlay-put)
(global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
(global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
(global-set-key (kbd "C-*") 'symbol-overlay-mode)
(global-set-key (kbd "C-&") 'symbol-overlay-remove-all)

;; cc-mode
(setq c-default-style '((c++-mode . "barefoot")))

;; ace-jump
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;; enable a more powerful jump back function from ace jump mode
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode-pop-mark)

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

;; ;; cider
;; (use-package cider
;;   :config
;;   (setq cider-default-repl-command "lein"))

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
(setq column-enforce-column 100)
(setq-default fill-column 100)
(add-hook 'prog-mode-hook 'column-enforce-mode)
(add-hook 'text-mode-hook 'column-enforce-mode)
(setq column-number-mode t)

;; modalka
;; Function to change the cursor look on entry and exit to god-mode-all
;; (defun god-update-cursor ()
;;   (setq cursor-type (if (or god-local-mode buffer-read-only)
;;                         'hollow
;;                       'box))
;;   (set-cursor-color (if (or god-local-mode buffer-read-only)
;; 			"chartreuse"
;; 		      "#f92672")))
;; (defun god-enabled-cursor ()
;;   (set-cursor-color "chartreuse")
;;   (setq cursor-type 'hollow))  
;; (defun god-disabled-cursor ()
;;   (interactive)
;;   (set-cursor-color "#f92672")
;;   (setq cursor-type 'box))
;; (add-hook 'god-mode-enabled-hook 'god-enabled-cursor)
;; (add-hook 'god-mode-disabled-hook 'god-disabled-cursor)


;; Company mode 
(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (global-set-key (kbd "M-s M-s")  'company-complete))

;; ;; MATLAB-mode
;; (add-to-list 'load-path "~/.emacs.d/matlab")
;; (load-library "matlab-load")

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
 '(ansi-color-names-vector
   ["#000000" "#5f8787" "#ddeecc" "#99bbaa" "#888888" "#999999" "#888888" "#c1c1c1"])
 '(ansi-term-color-vector
   [unspecified "#000000" "#5f8787" "#ddeecc" "#99bbaa" "#888888" "#999999" "#888888" "#c1c1c1"] t)
 '(custom-enabled-themes (quote (base16-darktooth)))
 '(custom-safe-themes
   (quote
    ("31e9b1ab4e6ccb742b3b5395287760a0adbfc8a7b86c2eda4555c8080a9338d9" "c2efd2e2e96b052dd91940b100d86885337a37be1245167642451cf6da5b924a" "fc7fd2530b82a722ceb5b211f9e732d15ad41d5306c011253a0ba43aaf93dccc" "c614d2423075491e6b7f38a4b7ea1c68f31764b9b815e35c9741e9490119efc0" "bc4c89a7b91cfbd3e28b2a8e9e6750079a985237b960384f158515d32c7f0490" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "02940c38e51991e8ee8ac69537341149d56e9c88d57f2c357eeb1744daad1953" "aded4ec996e438a5e002439d58f09610b330bbc18f580c83ebaba026bbef6c82" "78c1c89192e172436dbf892bd90562bc89e2cc3811b5f9506226e735a953a9c6" "6271fc9740379f8e2722f1510d481c1df1fcc43e48fa6641a5c19e954c21cc8f" "7559ac0083d1f08a46f65920303f970898a3d80f05905d01e81d49bb4c7f9e39" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "ad16a1bf1fd86bfbedae4b32c269b19f8d20d416bd52a87cd50e355bf13c2f23" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "3be1f5387122b935a26e02795196bc90860c57a62940f768f138b02383d9a257" "808b47c5c5583b5e439d8532da736b5e6b0552f6e89f8dafaab5631aace601dd" "44961a9303c92926740fc4121829c32abca38ba3a91897a4eab2aa3b7634bed4" "8be07a2c1b3a7300860c7a65c0ad148be6d127671be04d3d2120f1ac541ac103" "87d46d0ad89557c616d04bef34afd191234992c4eb955ff3c60c6aa3afc2e5cc" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "dd4628d6c2d1f84ad7908c859797b24cc6239dfe7d71b3363ccdd2b88963f336" "1263771faf6967879c3ab8b577c6c31020222ac6d3bac31f331a74275385a452" "fb44ced1e15903449772b750c081e6b8f687732147aa43cfa2e7d9a38820744b" "65f35d1e0d0858947f854dc898bfd830e832189d5555e875705a939836b53054" "d9aa334b2011d57c8ce279e076d6884c951e82ebc347adbe8b7ac03c4b2f3d72" "e297f54d0dc0575a9271bb0b64dad2c05cff50b510a518f5144925f627bb5832" "5a39d2a29906ab273f7900a2ae843e9aa29ed5d205873e1199af4c9ec921aaab" "e1498b2416922aa561076edc5c9b0ad7b34d8ff849f335c13364c8f4276904f0" "39fe48be738ea23b0295cdf17c99054bb439a7d830248d7e6493c2110bfed6f8" "c968804189e0fc963c641f5c9ad64bca431d41af2fb7e1d01a2a6666376f819c" "6de7c03d614033c0403657409313d5f01202361e35490a3404e33e46663c2596" "ed317c0a3387be628a48c4bbdb316b4fa645a414838149069210b66dd521733f" "c158c2a9f1c5fcf27598d313eec9f9dceadf131ccd10abc6448004b14984767c" default)))
 '(org-agenda-files (quote ("~/notes/daily.org")))
 '(org-icalendar-use-deadline (quote (event-if-not-todo event-if-todo todo-due)))
 '(org-icalendar-use-scheduled (quote (event-if-not-todo event-if-todo todo-start)))
 '(package-selected-packages
   (quote
    (modalka banner-comment flycheck-rtags helm-rtags company-rtags ac-rtags rtags irony yascroll symbol-overlay switch-window swap-buffers sublimity system-specific-settings smooth-scroll smooth-scrolling lush-theme lsp-p4 dap-mode helm-lsp lsp-treemacs company-lsp lsp-ui lsp-mode zoom-window zoom magithub magit golden-ratio golden-ratio-scroll-screen buffer-expose buffer-flip achievements ace-jump-buffer ace-isearch ace-jump-mode boogie-friends fold-this org-edit-latex magic-latex-buffer latex-math-preview cdlatex ac-math iasm-mode company-coq proof-general cubicaltt google-maps google-translate google-this auto-complete merlin tuareg poporg ggtags cider undo-tree spinner slime-company simple-httpd queue purty-mode purple-haze-theme punpun-theme origami orgtbl-join orgtbl-ascii-plot org-ref org-plus-contrib org-beautify-theme org-alert memoize js2-mode green-phosphor-theme god-mode flyspell-correct-popup flycheck django-snippets django-mode django-manage column-enforce-mode clojure-mode base16-theme avy auctex ample-theme 0blayout)))
 '(pixel-scroll-mode t)
 '(show-paren-mode t)
 '(smooth-scrolling-mode t)
 '(sublimity-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Type" :foundry "nil" :slant normal :weight normal :height 120 :width normal))))
 '(org-code ((t (:foreground "#f4bf75" :family "Courier"))))
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
