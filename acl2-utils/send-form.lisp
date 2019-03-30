(defvar acl2-active-shell-buffer "*shell*"
  "*Used by send-acl2-form to send the current acl2 form to the active shell buffer")

(defun set-acl2-active-shell-buffer ()
  "Make the current buffer the active acl2 shell buffer"
  (interactive)
  (setq acl2-active-shell-buffer
	(buffer-name)))

(defun skip-white-space ()
  (while (looking-at "[ \t\n]")
    (forward-char 1)))

(defun skip-nested-comments (depth)
  (cond 
   ((< depth 1) t)
   (t (re-search-forward "|#\\|#|" (point-max) t 1)
      (forward-char -2)
      (cond ((looking-at "#|")
	     (forward-char 2)
	     (skip-nested-comments (1+ depth)))
	    (t (forward-char 2)
	       (skip-nested-comments (1- depth)))))))

(defun skip-comments ()
  (cond ((looking-at ";")
	 (beginning-of-line 2)
	 (skip-comments))
	((looking-at "#|")
	 (forward-char 2)
	 (skip-nested-comments 1)
	 (skip-comments))
	((looking-at "[ \t\n]")
	 (skip-white-space)
	 (skip-comments))
	(t t)))

(defun send-acl2-form ()
  "Send the sexpt starting from the point to acl2-active-shell-buffer"
  (interactive)
  (skip-comments)
  (let ((begin (point))
	(end))
    (forward-sexp)
    (setq end (point))
    (process-send-region acl2-active-shell-buffer begin end)
    (process-send-string acl2-active-shell-buffer "\C-j")
    (accept-process-output))
  (skip-comments))

(global-set-key "\M-n" 'send-acl2-form)
(global-set-key "\C-xas" 'set-acl2-active-shell-buffer)
