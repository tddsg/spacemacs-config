;; select the current line
(defun tddsg-select-current-line ()
  "Select current line"
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))


;; kill line backward
(defun tddsg-kill-line-backwards ()
  "Kill line backwards."
  (interactive)
  (kill-line 0))

;; mark ring setting
(defun tddsg-set-mark ()
  (interactive)
  (push-mark (point) t nil))

(defun tddsg-shell-other-window (&optional buffer)
  "Open a `shell' in a new window."
  (interactive)
  (let ((old-buf (current-buffer))
        (current-prefix-arg 4) ;; allow using C-u
        (shell-buf (call-interactively 'shell)))
    (switch-to-buffer-other-window shell-buf)
    (switch-to-buffer old-buf)
    (other-window 1)))

(defun tddsg-shell-current-window (&optional buffer)
  "Open a `shell' in the current window."
  (interactive)
  (let ((old-buf (if (= (count-windows) 1) (current-buffer)
                   (progn
                     (other-window 1)
                     (let ((buf (window-buffer))) (other-window -1) buf))))
        (old-window (frame-selected-window))
        (current-prefix-arg 4) ;; allow using C-u
        (shell-buf (call-interactively 'shell)))
    (switch-to-buffer old-buf)
    (select-window old-window)
    (switch-to-buffer shell-buf)))

;; unpop
(defun tddsg-unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
      (when mark-ring
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
        (when (null (mark t)) (ding))
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (marker-position (car (last mark-ring))))))

;; temporarily change the mode line for a few seconds
(defun tddsg-change-temporary-mode-line (text time)
  "Display TEXT in mode line for TIME seconds."
  (interactive)
  (let ((buf (current-buffer)))
    (setq mode-line-format text)
    (run-at-time time nil
                 (lambda (v b)
                   (with-current-buffer b
                     (setq mode-line-format v)
                     (force-mode-line-update)))
                 tddsg-default-mode-line buf)))

;; temporarily change the mode line for a few seconds
(defun tddsg-set-default-mode-line ()
  "Set mode-line to be the default one."
  (interactive)
  (setq mode-line-format tddsg-default-mode-line)
  (force-mode-line-update))

;; save as new file and open it without closing the old file
(defun tddsg-save-file-as-and-open-file (filename &optional confirm)
  "Save current buffer into file FILENAME and open it in a new buffer."
  (interactive
   (list (if buffer-file-name
	     (read-file-name "Save as and open file: "
			     nil nil nil nil)
	   (read-file-name "Save as and open file: " default-directory
			   (expand-file-name
			    (file-name-nondirectory (buffer-name))
			    default-directory)
			   nil nil))
	 (not current-prefix-arg)))
  (or (null filename) (string-equal filename "")
      (progn
	;; If arg is just a directory,
	;; use the default file name, but in that directory.
	(if (file-directory-p filename)
	    (setq filename (concat (file-name-as-directory filename)
				   (file-name-nondirectory
				    (or buffer-file-name (buffer-name))))))
	(and confirm
	     (file-exists-p filename)
	     ;; NS does its own confirm dialog.
	     (not (and (eq (framep-on-display) 'ns)
		       (listp last-nonmenu-event)
		       use-dialog-box))
	     (or (y-or-n-p (format "File `%s' exists; overwrite? " filename))
		 (error "Canceled")))
        (write-region (point-min) (point-max) filename )
        (find-file filename)))
  (vc-find-file-hook))

;;
(defun tddsg-yank-current-word-to-minibuffer ()
  "Get word at point in original buffer and insert it to minibuffer."
  (interactive)
  (let (word beg)
    (with-current-buffer (window-buffer (minibuffer-selected-window))
      (save-excursion
        (skip-syntax-backward "w_")
        (setq beg (point))
        (skip-syntax-forward "w_")
        (setq word (buffer-substring-no-properties beg (point)))))
    (when word
      (insert word))))

;;
(defun tddsg-yank-current-word-to-isearch-buffer ()
  "Pull current word from buffer into search string."
  (interactive)
  (save-excursion
    (skip-syntax-backward "w_")
    (isearch-yank-internal
     (lambda ()
       (skip-syntax-forward "w_")
       (point)))))

