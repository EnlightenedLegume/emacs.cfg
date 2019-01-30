;;REMOTE CONNECTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq tramp-default-method "plink")
(setq password-cache-expiry nil)

;; Sets shell to specified path
(defun set-shell (path)
  (interactive)
  (setq explicit-shell-file-name path))

;; Change the explicit shell file name
(defun set-shell-unix ()
  (interactive)
  (set-shell "/bin/bash"))

;; Change back to the local shell
(defun set-shell-local ()
  (interactive)
  (set-shell nil))

;; Set shell to win-bash (linux subsystem)
(defun set-shell-win-bash ()
  (interactive)
  (set-shell "C:/Windows/System32/bash.exe"))

;; Connect to courselab
(defun courselab-shell (num)
  (interactive "P")
  (let ((default-directory
	  "/pscp:bdhuang@courselab.cs.princeton.edu:/u/bdhuang/"))
    (call-interactively 'shell)))

;; Opens shell on peace
(defun peace-shell (num)
  (interactive "P")
  (let ((default-directory
	  "/pscp:bdhuang@peace.cs.princeton.edu#75:/u/bdhuang/"))
    (call-interactively 'shell)))

;; Opens shell on blue
;; For some reason, this tries to ssh directly to blue. To get it to
;; work, I need to kill the ssh process that it starts on windows
(defun blue-shell (num)
  (interactive "P")
  (add-to-list 'tramp-default-proxies-alist
	       '("blue\\.cs\\.princeton\\.edu" nil
		 "/plink:bdhuang@peace.cs.princeton.edu#75:"))
  (let ((default-directory
	  (concat "/plink:bdhuang@peace.cs.princeton.edu#75"
		  "|ssh:blue.cs.princeton.edu:/u/bdhuang/")))
    (call-interactively 'shell)))

;; Loads TAGS file for llvm-liberty
(defun load-tags-llvm-liberty ()
  (interactive)
  (visit-tags-table "/pscp:bdhuang@peace.cs.princeton.edu#75:/u/bdhuang/llvm-liberty/TAGS"))

;; Loads TAGS file for llvm
(defun load-tags-llvm ()
  (interactive)
  (visit-tags-table "/pscp:bdhuang@peace.cs.princeton.edu#75:/u/bdhuang/llvm-workspace/TAGS"))

;; Disable company mode on remote buffers, because synchronous blocks
;; across slow connections are unbearable
(defun disable-company-on-remote ()
  (when (file-remote-p default-directory) (company-mode -1)))
(add-hook 'after-change-major-mode-hook 'disable-company-on-remote)
