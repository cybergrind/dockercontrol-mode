
(defvar tabulated-list-format)
(defvar tabulated-list-entries)
(defvar tabulated-list-sort-key)

;(declare-function tabulated-list-init-header  "tabulated-list" ())

(define-derived-mode dockercontrol-mode tabulated-list-mode "Docker"
  "Mode for dockercontrol"
  (setq tabulated-list-format
        [("Name" 30 t)
         ("Status" 15 t)
         ("Image" 15 t)])
  (setq tabulated-list-sort-key (cons "Status" nil))
  (add-hook 'tabulated-list-revert-hook 'list-processes--refresh nil t)
  (setq truncate-lines nil)
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

(defvar dockercontrol-mode-map
  (let ((map make-keymap))
    (define-key map (kbd "r") 'docker-list)
    (define-key map (kbd "p") 'docker-pause)
    map))

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

(defun docker-pause (line)
  "pause container"
  (message "Got line %s" line)
  (eval (shell-command-to-string)))

(defun docker-list ()
  "Get docker processes list"
  (interactive)
  (setq tabulated-list-entries
        (car (read-from-string (shell-command-to-string "python2 ~/.emacs.d/dockercontrol-mode/dockercontrol.py"))))
  (tabulated-list-print))

(prin1-to-string '(list '("asdf" ["one" "two"])))

(defun dockercontrol-mode-hook ()
  "init"
  (message "dockercontrol-mode hook execute"))

(provide 'dockercontrol-mode)