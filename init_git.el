;; Git functions and key bindings
(defun git-commit-am ()
  "Performs a git commit -am in the current directory"
  (interactive)
  (let ((msg (read-string "Enter commit message: ")))
    (shell-command (concat "git commit -am " "\"" msg "\""))))
(global-set-key (kbd "M-g M-C") 'git-commit-am)

(defun git-push-origin-master ()
  "Performs a git push origin master in the current directory"
  (interactive)
  (shell-command "git push origin master"))
(global-set-key (kbd "M-g M-p") 'git-push-origin-master)

(defun git-update-modified ()
  "Performs a git commit -am and then pushes to origin master"
  (interactive)
  (call-interactively 'git-commit-am)
  (shell-command "git push origin master"))
(global-set-key (kbd "M-g M-u") 'git-update-modified)

(defun git-status ()
  "Performs a git commit -am and then pushes to origin master"
  (interactive)
  (shell-command "git status"))
(global-set-key (kbd "M-g M-s") 'git-status)

(defun git-add-all ()
  "Performs a git add all"
  (interactive)
  (shell-command "git add ."))
(global-set-key (kbd "M-g M-A") 'git-add-all)

(defun git-add ()
  "git add with user specified parameter"
  (interactive)
  (let ((msg (read-string "Enter files to be added: ")))
    (shell-command (concat "git add " msg))))
(global-set-key (kbd "M-g M-a") 'git-add)
