;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Install some required packages

(require 'package)
(setq use-package-verbose t)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; add my own modules
(add-to-list 'load-path "~/.emacs.d/personal/modules")

;; activate all the packages (in particular autoloads)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Always load newest byte code
(setq load-prefer-newer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Install initial package first

(setq package-list '(use-package))

(unless package-archive-contents (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities functions for TDDSG mode

(defconst tddsg-savefile-dir (expand-file-name "savefile" user-emacs-directory))

(when (not (file-exists-p tddsg-personal-dir))
  (make-directory tddsg-personal-dir))

(defvar tddsg-default-mode-line mode-line-format)

;; select the current line
(defun tddsg-select-current-line ()
  "Select current line"
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))


;; get the closet parent folder containing a Makefile
(defun tddsg-get-closest-build-path (&optional (file "Makefile"))
  "Get path of the closest parent folder that contains a Makefile"
  (let ((root (expand-file-name "/"))) ; the win32 must reconsider it
    (loop
     for d = default-directory then (expand-file-name ".." d)
     if (file-exists-p (expand-file-name file d))
     return d
     if (equal d root)
     return nil)))

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




;; define key for other minor mode
(define-key isearch-mode-map (kbd "C-.") 'tddsg-yank-current-word-to-isearch-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Various configuration



