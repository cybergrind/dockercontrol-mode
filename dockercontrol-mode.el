(defvar tabulated-list-format)
(defvar tabulated-list-entries)
(defvar tabulated-list-sort-key)

;(declare-function tabulated-list-init-header  "tabulated-list" ())

(defun docker-get-map ()
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'docker-list)
    (define-key map (kbd "P") 'docker-pause)
    (define-key map (kbd "U") 'docker-unpause)
    (define-key map (kbd "s") 'docker-start)
    (define-key map (kbd "S") 'docker-stop)
    (define-key map (kbd "R") 'docker-remove)
    (define-key map (kbd "E") 'docker-exec)
    map))

(defvar dockercontrol-mode-map
  (docker-get-map)
  "Keymap for dockercontrol-mode")

(defun docker-update-map ()
  (interactive)
  (setq dockercontrol-mode-map (docker-get-map))
  (use-local-map dockercontrol-mode-map))

(defvar docker-control-positions
  (let ((ht (make-hash-table :test 'equal)))
    (puthash 'name 0 ht)
    (puthash 'status 1 ht)
    (puthash 'image 2 ht)
    ht))

(define-derived-mode dockercontrol-mode tabulated-list-mode "Docker"
  "Mode for dockercontrol"
  (use-local-map dockercontrol-mode-map)
  (setq tabulated-list-format
        [("Name" 30 t)
         ("Status" 15 t)
         ("Image" 15 t)])
  (setq tabulated-list-sort-key (cons "Status" nil))
  (add-hook 'tabulated-list-revert-hook 'list-processes--refresh nil t)
  (setq truncate-lines nil)
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

(defun docker (&optional buffer)
  "Run dockercontrol-mode"
  (interactive)
  (unless (bufferp buffer)
    (setq buffer (get-buffer-create "*Docker*")))
  (with-current-buffer buffer
    (dockercontrol-mode)
    (docker-list))
  (display-buffer buffer)
  nil)

(defun docker-pause ()
  "pause container"
  (interactive)
  (let* ((container (tabulated-list-get-id)))
    (start-process "docker-pause" nil "docker" "pause" container))
  (docker-list))

(defun docker-unpause ()
  "pause container"
  (interactive)
  (let* ((container (tabulated-list-get-id)))
    (start-process "docker-unpause" nil "docker" "unpause" container))
  (docker-list))

(defun docker-start ()
  (interactive)
  (let* ((container (tabulated-list-get-id)))
    (start-process "docker-start" "msgs" "docker" "start" container))
  (docker-list))

(defun docker-stop ()
  (interactive)
  (let* ((container (tabulated-list-get-id)))
    (start-process "docker-stop" nil "docker" "stop" container))
  (docker-list))

(defun docker-list ()
  "Get docker processes list"
  (interactive)
  (setq tabulated-list-entries
        (car (read-from-string (shell-command-to-string "python2 ~/.emacs.d/el-get/dockercontrol-mode/dockercontrol.py"))))
  (tabulated-list-print))

(defun docker-remove ()
  (interactive)
  (let ((container (tabulated-list-get-id)))
    (start-process "docker-rm" nil "docker" "rm" container))
  (tabulated-list-print))

(defun docker-exec ()
  (interactive)
  (let* ((container (tabulated-list-get-id))
         (entry (tabulated-list-get-entry))
         (name-pos (gethash 'name docker-control-positions))
         (name (elt entry name-pos)))
    (start-process "docker-exec" "*exec*"
                   "screen" "-t" name
                   "docker" "exec" "-it"
                   container "/bin/bash")))

(prin1-to-string '(list '("asdf" ["one" "two"])))

(defun dockercontrol-mode-hook ()
  "init"
  (message "dockercontrol-mode hook execute"))

(provide 'dockercontrol-mode)
