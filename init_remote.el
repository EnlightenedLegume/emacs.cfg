;;REMOTE CONNECTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; Connect to courselab
(defun vm1-shell (num)
  (interactive "P")
  (let ((default-directory
	  "/scp:bhuang-u1604.swlab.barefootnetworks.com:/home/bhuang/"))
    (set-shell-unix)
    (call-interactively 'shell)
    (setq tags-table-list
	  '("/scp:bhuang-u1604.swlab.barefootnetworks.com:/home/bhuang/bf-p4c-compilers/"))))

(defun cp-shell (num)
  (interactive "P")
  (let ((num (ido-completing-read "Machine number: "
                                 '("01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" ))))
    (let ((default-directory
            (concat "/scp:compiler-pool" num ".swlab.barefootnetworks.com:/home/bhuang/")))
      (set-shell-unix)
      ;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
      (call-interactively 'shell))))

;; Disable company mode on remote buffers, because synchronous blocks
;; across slow connections are unbearable
(defun disable-company-on-remote ()
  (when (file-remote-p default-directory) (company-mode -1)))
(add-hook 'after-change-major-mode-hook 'disable-company-on-remote)

;; Set my compilation command to what will normally work
(setq compile-command
      "sudo docker exec -w \"/mnt/build\" $(sudo docker container ps | awk  'BEGIN {FS = \" [ ]+\"} ; NR==2 { print $6 }') make -j4")
