;;; package --- Summary

;;; Commentary:

;;; Code:


(require 'smartparens)
(require 'company)
(require 'powerline)
(require 'buffer-move)
(require 'god-mode)
(require 'god-mode-isearch)
(require 'pdf-sync)
(require 'spaceline-segments)
(require 'spaceline)
(require 'pdf-view)
(require 'pdf-tools)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTILITIES FUNCTIONS

(defun tddsg/shell-other-window (&optional buffer)
  "Open a `shell' in a new window."
  (interactive)
  (let ((old-buf (current-buffer))
        (current-prefix-arg 4) ;; allow using C-u
        (shell-buf (call-interactively 'shell)))
    (switch-to-buffer old-buf)
    (switch-to-buffer-other-window shell-buf)
    ;; (switch-to-buffer-other-window old-buf)
    ))

(defun tddsg/shell-current-window (&optional buffer)
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

(defun tddsg/save-file-as-and-open-file (filename &optional confirm)
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

(defun tddsg/dired-home ()
  (interactive)
  (dired "~/"))

(defun tddsg/mark-line ()
  "Select current line"
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(defun tddsg/mark-sexp ()
  "Mark sexp using the smartparens package"
  (interactive)
  (let ((current-char (char-after)))
    (if (= ?\) (char-syntax current-char))
        (progn
          (deactivate-mark)
          (forward-char)
          (backward-sexp)))
    (if (region-active-p) (sp-forward-sexp)
      (progn
        (set-mark-command nil)
        (sp-forward-sexp 1)))))

(defun tddsg/smart-mark-sexp ()
  "Expand region or mark sexp"
  (interactive)
  (let ((current-char (char-after)))
    (cond ((= ?w (char-syntax current-char))
           (call-interactively 'er/expand-region))
          ((= ?_ (char-syntax current-char))
           (call-interactively 'er/expand-region))
          (t (tddsg/mark-sexp)))))

(defun tddsg/mark-paragraph ()
  "Mark the paragraph"
  (interactive)
  (if (region-active-p) (forward-paragraph 1)
    (progn
      (backward-paragraph)
      (if (looking-at "[[:space:]]*$") (next-line 1))
      (set-mark-command nil)
      (forward-paragraph 1))))

(defun tddsg/helm-do-ag (arg)
  "Search by Helm-Ag in the current directory, \
or in a custom directory when prefix-argument is given (C-u)"
  (interactive "P")
  (if (null arg)
      (helm-do-ag (expand-file-name default-directory))
    (call-interactively 'helm-do-ag)))

(defun tddsg/join-with-beneath-line ()
  "Join the current line to the line beneath it."
  (interactive)
  (delete-indentation 1)
  (let ((current-char (char-after)))
    (if (or (= ?w (char-syntax current-char))
            (= ?_ (char-syntax current-char))
            (= ?\" (char-syntax current-char))
            (= ?\( (char-syntax current-char))
            (= ?< (char-syntax current-char))
            (= ?/ (char-syntax current-char)))
        (just-one-space))))

(defun tddsg/join-to-above-line ()
  "Join the current line to the line above it."
  (interactive)
  (delete-indentation)
  (delete-horizontal-space)
  (let ((current-char (char-after)))
    (if (or (= ?w (char-syntax current-char))
            (= ?_ (char-syntax current-char))
            (= ?\" (char-syntax current-char))
            (= ?\( (char-syntax current-char))
            (= ?< (char-syntax current-char))
            (= ?/ (char-syntax current-char)))
        (just-one-space))))

(defun tddsg/yank-current-word-to-minibuffer ()
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

(defun tddsg/yank-current-word-to-isearch-buffer ()
  "Pull current word from buffer into search string."
  (interactive)
  (save-excursion
    (skip-syntax-backward "w_")
    (isearch-yank-internal
     (lambda ()
       (skip-syntax-forward "w_")
       (point)))))


(defun tddsg-create-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.emacs.d/emacs-backup/")
         ;; remove Windows driver letter in path, ➢ for example: “C:”
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath ))
         (backupFilePath (replace-regexp-in-string
                          "//" "/" (concat backupRootDir filePath "~"))))
    (make-directory (file-name-directory backupFilePath)
                    (file-name-directory backupFilePath))
    backupFilePath))

;; call compile to the closest parent folder containing a Makefile
(defun tddsg/compile ()
  (interactive)
  (cl-labels
      ((find-make-file-dir
        (cur-dir root-dir make-file)
        (cond ((string= cur-dir root-dir) "")
              ((file-exists-p (expand-file-name make-file cur-dir)) cur-dir)
              (t (find-make-file-dir (expand-file-name ".." cur-dir)
                                     root-dir
                                     make-file)))))
    (let* ((cur-dir default-directory)
           (root-dir "/")
           (make-file "Makefile")
           (new-command
            (if (and (>  (length compile-command) 4)
                     (string= (substring compile-command 0 4) "make"))
                (format "make -k -C %s"
                        (find-make-file-dir cur-dir root-dir make-file))
              compile-command)))
      (setq compile-command new-command)
      (call-interactively 'compile))))

(defun tddsg/unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))

(defun tddsg-set-mark ()
  (interactive)
  (push-mark (point) t nil))

(defun tddsg/enable-company-auto-suggest ()
  (interactive)
  (setq company-idle-delay 0.5))

(defun tddsg/disable-company-auto-suggest ()
  (interactive)
  (setq company-idle-delay 300))

(defun tddsg-buffer-focus ()
  (if (derived-mode-p 'text-mode 'tuareg-mode)
      (tddsg/disable-company-auto-suggest)
    (tddsg/enable-company-auto-suggest)))

(defun tddsg-hook-prog-text-mode ()
  (linum-mode 1)
  (column-marker-3 80)
  (whitespace-mode 1))

(defun tddsg-hook-prog-mode ()
  (flycheck-mode 1))

(defun tddsg-hook-text-mode ()
  (flyspell-mode 1))

(defun tddsg-fix-comint-window-size ()
  "Change process window size."
  (when (derived-mode-p 'comint-mode)
    (let ((process (get-buffer-process (current-buffer))))
      (when process
        (set-process-window-size process (window-height) (window-width))))))

(defun tddsg-hook-shell-mode ()
  (add-hook 'window-configuration-change-hook
            'tddsg-fix-comint-window-size nil t)
  (rainbow-delimiters-mode-enable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INIT CONFIGS

(defun tddsg/init-configs ()
  ;; visual interface setting
  (global-hl-todo-mode 1)           ;; highlight current line
  (blink-cursor-mode 0)             ;; turn on blinking
  (setq blink-cursor-blinks 15)     ;; blink 15 times
  (setq-default fill-column 80)
  (setq text-scale-mode-step 1.1)   ;; scale changing font size
  (setq frame-title-format          ;; frame title
        '("" invocation-name " - "
          (:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name)) "%b"))))

  ;; scrolling
  (spacemacs/toggle-smooth-scrolling-off)  ;; disable smooth-scrolling
  (setq redisplay-dont-pause t
        scroll-conservatively 10000
        scroll-margin 5
        scroll-preserve-screen-position 't)

  ;; mode paragraph setting
  (setq paragraph-separate "[ \t\f]*$"
        paragraph-start "\f\\|[ \t]*$")

  ;; spell
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra")
        ispell-dictionary "english")

  ;; automatically setting mark for certain commands
  (setq global-mark-ring-max 1000
        mark-ring-max 200)
  (setq set-mark-command-repeat-pop t)
  (defadvice find-file (before set-mark activate) (tddsg-set-mark))
  (defadvice isearch-update (before set-mark activate) (tddsg-set-mark))
  (defadvice beginning-of-buffer (before set-mark activate) (tddsg-set-mark))
  (defadvice end-of-buffer (before set-mark activate) (tddsg-set-mark))
  (defadvice merlin-locate (before set-mark activate) (tddsg-set-mark))

  ;; advice on buffer focusing
  (defadvice other-window (after update activate) (tddsg-buffer-focus))
  (defadvice windmove-do-window-select (after update activate) (tddsg-buffer-focus))
  (defadvice split-window (after update activate) (tddsg-buffer-focus))
  (defadvice set-buffer (after update activate) (tddsg-buffer-focus))
  (defadvice switch-to-buffer (after update activate) (tddsg-buffer-focus))
  (defadvice save-buffer (after update activate) (tddsg-buffer-focus))
  (defadvice pop-to-buffer (after update activate) (tddsg-buffer-focus))
  (defadvice previous-buffer (after update activate) (tddsg-buffer-focus))
  (defadvice next-buffer (after update activate) (tddsg-buffer-focus))
  (defadvice keyboard-quit (after update activate) (tddsg-buffer-focus))

  ;; mode editing setting
  (electric-pair-mode t)
  (delete-selection-mode t)                            ;; delete selection by keypress
  (setq require-final-newline t)                       ;; newline at end of file
  (defadvice newline (after newline-after activate)    ;; indent after new line
    (indent-according-to-mode))
  ;; (setq company-idle-delay 200)         ;; set delay time by default
  (global-company-mode)

  ;; some Emacs threshold
  (setq max-lisp-eval-depth 50000)
  (setq max-specpdl-size 50000)

  ;; mode-line setting
  (setq powerline-default-separator 'wave)

  ;; compilation
  (setq compilation-ask-about-save nil
        compilation-window-height 15)

  ;; shell
  (setq comint-prompt-read-only nil)
  (defadvice shell (after linum activate) (linum-mode 1))
  (setq shell-default-shell 'ansi-term)
  (add-hook 'shell-mode-hook 'tddsg-hook-shell-mode)

  ;; smartparens
  (smartparens-global-mode)

  ;; backup
  (setq make-backup-files t)
  (setq make-backup-file-name-function 'tddsg-create-backup-file-name)

  ;; evil mode
  (setq-default evil-cross-lines t)

  ;; dired
  (add-to-list 'savehist-additional-variables 'helm-dired-history-variable)
  (setq dired-guess-shell-alist-user
        '(("\\.pdf\\'" "okular &")
          ("\\.txt\\'" "gedit")))

  ;; helm
  (setq helm-ag-insert-at-point 'symbol)

  ;; diminish
  (eval-after-load "abbrev" '(diminish 'abbrev-mode " ↹"))
  (eval-after-load "whitespace" '(diminish 'whitespace-mode " S"))
  (eval-after-load "whitespace-mode" '(diminish 'whitespace-mode " R"))
  (eval-after-load "smartparens" '(diminish 'smartparens-mode " ♓"))
  (eval-after-load "super-save" '(diminish 'super-save-mode " ⓢ"))
  (eval-after-load "god-mode" '(diminish 'god-local-mode " ☼"))
  (eval-after-load "which-key" '(diminish 'which-key-mode " ⌨"))
  (eval-after-load "rainbow-mode" '(diminish 'rainbow-mode " ☔"))
  (eval-after-load "autorevert" '(diminish 'auto-revert-mode " ↺"))
  (eval-after-load "visual-line" (diminish 'visual-line-mode " ⤾"))
  (eval-after-load "merlin" '(diminish 'merlin-mode " ☮"))
  (eval-after-load "flycheck" '(diminish 'flycheck-mode " ✔"))
  (eval-after-load "flyspell" '(diminish 'flyspell-mode " ✔"))
  (eval-after-load "projectile" '(diminish 'projectile-mode " π"))

  ;; hooks, finally hook
  (add-hook 'LaTeX-mode-hook 'tddsg-hook-prog-text-mode)
  (add-hook 'tex-mode-hook 'tddsg-hook-prog-text-mode)
  (add-hook 'prog-mode-hook 'tddsg-hook-prog-text-mode)
  (add-hook 'text-mode-hook 'tddsg-hook-prog-text-mode)
  (add-hook 'prog-mode-hook 'tddsg-hook-prog-mode)
  (add-hook 'text-mode-hook 'tddsg-hook-text-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INIT KEYS

(defun tddsg/init-keys ()
  (global-set-key (kbd "<home>") 'crux-move-beginning-of-line)
  (global-set-key (kbd "<escape>") 'god-mode-all)
  (global-set-key (kbd "C-S-<backspace>") 'kill-whole-line)
  (global-set-key (kbd "C-<backspace>") 'backward-kill-word)
  (global-set-key (kbd "C-<delete>") 'kill-word)
  (global-set-key (kbd "M-<backspace>") 'backward-kill-word)
  (global-set-key (kbd "M-<delete>") 'kill-word)
  (global-set-key (kbd "C-M-k") 'sp-kill-sexp)
  (global-set-key (kbd "C-M-SPC") 'tddsg/smart-mark-sexp)
  (global-set-key (kbd "C-<left>") 'left-word)
  (global-set-key (kbd "C-<right>") 'right-word)
  (global-set-key (kbd "C-+") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  (global-set-key (kbd "C-/") 'undo)
  (global-set-key (kbd "C-S-/") 'undo-tree-redo)
  (global-set-key (kbd "C-;") 'iedit-mode)
  (global-set-key (kbd "C-^") 'tddsg/join-with-beneath-line)
  (global-set-key (kbd "C-_") 'tddsg/join-to-above-line)
  (global-set-key (kbd "C-\\") 'goto-last-change)
  (global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
  (global-set-key (kbd "C-c d") 'crux-duplicate-current-line-or-region)

  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x _") 'shrink-window)
  (global-set-key (kbd "C-x m") 'monky-status)
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x G") 'magit-diff)
  (global-set-key (kbd "C-x w s") 'tddsg/save-file-as-and-open-file)

  (global-set-key (kbd "C-x C-d") 'helm-dired-history-view)
  (global-set-key (kbd "C-x C-b") 'switch-to-buffer)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)

  (global-set-key (kbd "C-x C-z") nil)
  (global-set-key (kbd "C-z") nil)

  (global-set-key (kbd "C-c f") 'projectile-find-file)
  (global-set-key (kbd "C-c o") 'helm-occur)
  (global-set-key (kbd "C-c r") 'projectile-replace)
  (global-set-key (kbd "C-c R") 'projectile-replace-regexp)
  (global-set-key (kbd "C-c i") 'helm-semantic-or-imenu)
  (global-set-key (kbd "C-c g") 'tddsg/helm-do-ag)
  (global-set-key (kbd "C-c m") 'tddsg/shell-other-window)
  (global-set-key (kbd "C-c M") 'shell)

  (global-set-key (kbd "C-c C-g") 'helm-projectile-grep)
  (global-set-key (kbd "C-c C-i") 'helm-imenu-anywhere)
  (global-set-key (kbd "C-c C-SPC") 'helm-all-mark-rings)
  (global-set-key (kbd "C-c C-c") 'tddsg/compile)

  (global-set-key (kbd "M-S-<up>") 'move-text-up)
  (global-set-key (kbd "M-S-<down>") 'move-text-down)
  (global-set-key (kbd "M-S-SPC") 'delete-blank-lines)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "M-s p") 'check-parens)
  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "M-;") 'comment-dwim-2)
  (global-set-key (kbd "M-?") 'company-complete)
  (global-set-key (kbd "M-H") 'tddsg/mark-line)
  (global-set-key (kbd "M-h") 'tddsg/mark-paragraph)
  (global-set-key (kbd "M-[") 'helm-company)
  (global-set-key (kbd "M-]") 'helm-dabbrev)

  (global-set-key (kbd "s-h") 'split-window-below)
  (global-set-key (kbd "s-v") 'split-window-right)
  (global-set-key (kbd "s-o") 'spacemacs/toggle-maximize-buffer)
  (global-set-key (kbd "s-w") 'delete-window)
  (global-set-key (kbd "s-x") 'helm-mini)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-\\") 'goto-last-change)
  (global-set-key (kbd "s-g") 'magit-status)
  (global-set-key (kbd "s-u") 'winner-undo)
  (global-set-key (kbd "s-r") 'winner-redo)
  (global-set-key (kbd "s-v") 'split-window-right)
  (global-set-key (kbd "s-v") 'split-window-right)
  (global-set-key (kbd "s-v") 'split-window-right)

  (global-set-key (kbd "M-m h g") 'helm-do-grep-ag)
  (global-set-key (kbd "M-m h o") 'helm-occur)
  (global-set-key (kbd "M-m h s") 'helm-semantic-or-imenu)
  (global-set-key (kbd "M-m s d") 'dictionary-search)
  (global-set-key (kbd "M-m S f m") 'flyspell-mode)
  (global-set-key (kbd "M-m S f b") 'flyspell-buffer)
  (global-set-key (kbd "M-m S i b") 'ispell-buffer)
  (global-set-key (kbd "M-m S i c") 'ispell-continue)
  (global-set-key (kbd "M-m S i k") 'ispell-kill-ispell)
  (global-set-key (kbd "M-m m t") 'ansi-term)
  (global-set-key (kbd "M-m m S") 'shell)
  (global-set-key (kbd "M-m m s") 'tddsg/shell-other-window)
  (global-set-key (kbd "M-m w t") 'transpose-frame)

  (global-set-key (kbd "M-m L c") 'langtool-check)
  (global-set-key (kbd "M-m L b") 'langtool-correct-buffer)
  (global-set-key (kbd "M-m L d") 'langtool-check-done)
  (global-set-key (kbd "M-m L n") 'langtool-goto-next-error)
  (global-set-key (kbd "M-m L p") 'langtool-goto-previous-error)

  ;; workspaces transient
  (global-set-key (kbd "M-m 1") 'eyebrowse-switch-to-window-config-1)
  (global-set-key (kbd "M-m 2") 'eyebrowse-switch-to-window-config-2)
  (global-set-key (kbd "M-m 3") 'eyebrowse-switch-to-window-config-3)
  (global-set-key (kbd "M-m 4") 'eyebrowse-switch-to-window-config-4)
  (global-set-key (kbd "M-m 5") 'eyebrowse-switch-to-window-config-5)
  (global-set-key (kbd "M-m 6") 'eyebrowse-switch-to-window-config-6)
  (global-set-key (kbd "M-m 7") 'eyebrowse-switch-to-window-config-7)
  (global-set-key (kbd "M-m 8") 'eyebrowse-switch-to-window-config-8)
  (global-set-key (kbd "M-m 9") 'eyebrowse-switch-to-window-config-9)
  (global-set-key (kbd "M-m 0") 'eyebrowse-switch-to-window-config-0)
  (global-set-key (kbd "M-m +") 'eyebrowse-next-window-config)
  (global-set-key (kbd "M-m -") 'eyebrowse-prev-window-config)
  (global-set-key (kbd "s-1") 'eyebrowse-switch-to-window-config-1)
  (global-set-key (kbd "s-2") 'eyebrowse-switch-to-window-config-2)
  (global-set-key (kbd "s-3") 'eyebrowse-switch-to-window-config-3)
  (global-set-key (kbd "s-4") 'eyebrowse-switch-to-window-config-4)
  (global-set-key (kbd "s-5") 'eyebrowse-switch-to-window-config-5)
  (global-set-key (kbd "s-6") 'eyebrowse-switch-to-window-config-6)
  (global-set-key (kbd "s-7") 'eyebrowse-switch-to-window-config-7)
  (global-set-key (kbd "s-8") 'eyebrowse-switch-to-window-config-8)
  (global-set-key (kbd "s-9") 'eyebrowse-switch-to-window-config-9)
  (global-set-key (kbd "s-0") 'eyebrowse-switch-to-window-config-0)
  (global-set-key (kbd "s-+") 'eyebrowse-next-window-config)
  (global-set-key (kbd "s--") 'eyebrowse-prev-window-config)
  (global-set-key (kbd "C-x M-<right>") 'eyebrowse-next-window-config)
  (global-set-key (kbd "C-x M-<left>") 'eyebrowse-prev-window-config)

  ;; layout
  (global-set-key (kbd "M-m l") nil)  ;; disable key "M-m l" first
  (global-set-key (kbd "M-m l m") 'spacemacs/layouts-transient-state/body)
  (global-set-key
   (kbd "M-m l s")
   'spacemacs/layouts-transient-state/persp-save-state-to-file-and-exit)
  (global-set-key
   (kbd "M-m l l")
   'spacemacs/layouts-transient-state/persp-load-state-from-file-and-exit)

  (define-key isearch-mode-map (kbd "C-.") 'tddsg/yank-current-word-to-isearch-buffer)
  (define-key minibuffer-local-map (kbd "C-.") 'tddsg/yank-current-word-to-minibuffer)
  (define-key shell-mode-map (kbd "C-j") 'newline)
  (define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
  (define-key undo-tree-map (kbd "C-_") nil)

  ;; god-mode
  (define-key isearch-mode-map (kbd "C-z") 'god-mode-isearch-activate)
  (define-key god-mode-isearch-map (kbd "C-z") 'god-mode-isearch-disable)
  (define-key god-local-mode-map (kbd "C-z") 'god-mode-all)
  (define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
  (define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)
  (define-key god-local-mode-map (kbd "<escape>") 'god-mode-all)
  (define-key god-local-mode-map (kbd "z") 'repeat)
  (define-key god-local-mode-map (kbd "i") 'god-local-mode)

  ;; windmove
  (global-set-key (kbd "S-<left>") 'windmove-left)
  (global-set-key (kbd "S-<right>") 'windmove-right)
  (global-set-key (kbd "S-<up>") 'windmove-up)
  (global-set-key (kbd "S-<down>") 'windmove-down)
  (global-set-key (kbd "s-b") 'windmove-left)
  (global-set-key (kbd "s-f") 'windmove-right)
  (global-set-key (kbd "s-p") 'windmove-up)
  (global-set-key (kbd "s-n") 'windmove-down)

  ;; buffer-clone
  (global-set-key (kbd "C-S-<left>") 'buf-clone-left)
  (global-set-key (kbd "C-S-<right>") 'buf-clone-right)
  (global-set-key (kbd "C-S-<up>") 'buf-clone-up)
  (global-set-key (kbd "C-S-<down>") 'buf-clone-down)
  (global-set-key (kbd "M-m b c h") 'buf-clone-left)
  (global-set-key (kbd "M-m b c l") 'buf-clone-right)
  (global-set-key (kbd "M-m b c k") 'buf-clone-up)
  (global-set-key (kbd "M-m b c j") 'buf-clone-down)

  ;; buffer-move
  (global-set-key (kbd "C-M-S-<left>") 'buf-move-left)
  (global-set-key (kbd "C-M-S-<right>") 'buf-move-right)
  (global-set-key (kbd "C-M-S-<up>") 'buf-move-up)
  (global-set-key (kbd "C-M-S-<down>") 'buf-move-down)

  ;; Latex-mode
  (define-key TeX-mode-map (kbd "<f5>") (kbd "C-c C-c C-j"))
  (define-key TeX-mode-map (kbd "<f6>") 'pdf-sync-forward-search)

  ;; pdf-tools
  (define-key pdf-view-mode-map (kbd "C-<home>") 'pdf-view-first-page)
  (define-key pdf-view-mode-map (kbd "C-<end>") 'pdf-view-last-page)

  ;; flyspell
  (define-key flyspell-mode-map (kbd "C-;") nil)

  ;; dired mode
  (define-key dired-mode-map (kbd "C-^") 'tddsg/dired-home)

  ;; evil mode
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>")
    'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>")
    'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>")
    'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>")
    'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward)
  (define-key evil-motion-state-map (kbd "C-^") nil)
  (define-key evil-motion-state-map (kbd "C-_") nil)
  (define-key evil-motion-state-map (kbd "C-z") nil)
  (define-key evil-insert-state-map (kbd "C-z") 'god-local-mode)

  ;; company mode
  (define-key company-active-map (kbd "M-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-.") 'company-show-location)

   ;; reassign key-chords
  (key-chord-define-global ",." 'helm-mini)
  (key-chord-define-global "zx" 'helm-mini)
  (key-chord-define-global "xs" 'save-buffer)
  (key-chord-define-global "JK" 'previous-buffer)
  (key-chord-define-global "KL" 'next-buffer)
  (key-chord-define-global "ji" 'indent-according-to-mode)
  (key-chord-define-global "jj" 'avy-goto-char-2)
  (key-chord-define-global "jk" 'avy-goto-word-1)
  (key-chord-define-global "jl" 'avy-goto-line)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INIT THEMES

(defcustom tddsg-themes nil
  "Association list of override faces to set for different custom themes.")

(defun tddsg-read-custom-themes (alist-symbol key value)
  "Set VALUE of a KEY in ALIST-SYMBOL."
  (set alist-symbol
        (cons (list key value) (assq-delete-all key (eval alist-symbol)))))

;; override some settings of the leuven theme
(defun tddsg-custom-theme-leuven ()
  (tddsg-read-custom-themes
   'tddsg-themes
   'leuven
   '(;; cursors & line
     (cursor ((t (:background "lime green"))))
     (hl-line ((t (:background "honeydew2"))))
     ;; latex font face
     (font-latex-bold-face ((t (:foreground "gray26" :weight bold))))
     (font-latex-math-face ((t (:foreground "DeepSkyBlue4"))))
     (font-latex-sedate-face ((t (:foreground "green4"))))
     (font-latex-subscript-face ((t (:height 0.96))))
     (font-latex-superscript-face ((t (:height 0.96))))
     (font-latex-verbatim-face ((t (:inherit nil :background "white" :foreground "light coral"))))
     (font-latex-sectioning-0-face ((t (:background "white smoke" :foreground "forest green" :overline t :weight bold :height 1.2))))
     (font-latex-sectioning-1-face ((t (:background "white smoke" :foreground "steel blue" :overline t :weight bold :height 1.2))))
     (font-latex-sectioning-2-face ((t (:background "#F0F0F0" :foreground "royal blue" :overline "#A7A7A7" :weight bold :height 1.1))))
     ;; dired mode
     (diredp-compressed-file-name ((t (:foreground "royal blue"))))
     (diredp-compressed-file-suffix ((t (:foreground "royal blue"))))
     (diredp-ignored-file-name ((t (:foreground "peru"))))
     ;; font lock face
     (font-lock-constant-face ((t (:foreground "dark goldenrod"))))
     (font-lock-doc-face ((t (:foreground "#8959a8"))))
     (font-lock-function-name-face ((t (:foreground "dark orchid" :weight normal))))
     (font-lock-keyword-face ((t (:foreground "blue" :weight normal))))
     (font-lock-string-face ((t (:foreground "#3e999f"))))
     (font-lock-type-face ((t (:foreground "MediumOrchid4" :weight normal))))
     (font-lock-variable-name-face ((t (:foreground "DodgerBlue3" :weight normal))))
     (company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
     (company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
     ;; others
     (diredp-file-suffix ((t (:foreground "sienna"))))
     (powerline-active1 ((t (:inherit mode-line :background "#163365")))))))

;; override some settings of the spacemacs-dark theme
(defun tddsg-custom-theme-spacemacs-dark ()
  (tddsg-read-custom-themes
   'tddsg-themes
   'spacemacs-dark
   '(;; cursors & line
     (cursor ((t (:background "lime green"))))
     ;; dired
     (diredp-compressed-file-name ((t (:foreground "burlywood"))))
     (diredp-compressed-file-suffix ((t (:foreground "yellow green"))))
     (diredp-dir-name ((t (:foreground "medium sea green" :weight bold))))
     (diredp-file-name ((t (:foreground "burlywood"))))
     (diredp-file-suffix ((t (:foreground "powder blue"))))
     (diredp-ignored-file-name ((t nil)))
     (diredp-link-priv ((t (:foreground "dodger blue"))))
     (diredp-symlink ((t (:foreground "dodger blue"))))
     ;; hilock
     '(hi-blue ((t (:background "medium blue" :foreground "white smoke"))))
     '(hi-blue-b ((t (:foreground "deep sky blue" :weight bold))))
     '(hi-green ((t (:background "dark olive green" :foreground "white smoke"))))
     '(hi-pink ((t (:background "dark magenta" :foreground "white smoke"))))
     '(hi-red-b ((t (:foreground "red1" :weight bold))))
     '(hi-yellow ((t (:background "dark goldenrod" :foreground "white smoke"))))
     ;; isearch
     '(isearch ((t (:background "dark orange" :foreground "#292b2e"))))
     '(lazy-highlight ((t (:background "LightGoldenrod3" :foreground "gray10" :weight normal))))
     ;; font
     (font-latex-subscript-face ((t (:height 0.96))))
     (font-latex-superscript-face ((t (:height 0.96))))
     (font-latex-sectioning-0-face ((t (:foreground "lawn green" :weight bold :height 1.4))))
     (font-latex-sectioning-1-face ((t (:foreground "deep sky blue" :weight bold :height 1.4))))
     (font-latex-sectioning-2-face ((t (:foreground "royal blue" :weight bold :height 1.2))))
     (lazy-highlight ((t (:background "dark goldenrod" :foreground "gray10" :weight normal))))
     )))

(defun tddsg-custom-common ()
  ;; custom variables
  (custom-set-variables
   '(sp-highlight-wrap-overlay nil))
  ;; custom faces
  (custom-set-faces
   '(sp-pair-overlay-face ((t nil)))
   '(sp-wrap-overlay-face ((t nil)))
   '(sp-wrap-tag-overlay-face ((t nil)))))

(defun tddsg-override-theme ()
  (dolist (theme-settings tddsg-themes)
    (let ((theme (car theme-settings))
          (faces (cadr theme-settings)))
      (if (member theme custom-enabled-themes)
          (dolist (face faces)
            (custom-theme-set-faces theme face))))))

(defun tddsg/init-themes ()
  ;; load the custom theme
  (tddsg-custom-common)
  (tddsg-custom-theme-leuven)
  (tddsg-custom-theme-spacemacs-dark)
  (tddsg-override-theme)
  ;; and defadvice load-theme function
  (defadvice load-theme (after theme-set-overrides activate)
    "Set override faces for different custom themes."
    (tddsg-override-theme)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INIT SPACELINE


;; reuse code from spaceline-config.el
(defun tddsg--create-spaceline-theme (left second-left &rest additional-segments)
  "Convenience function for the spacemacs and emacs themes."
  (spaceline-install 'tddsg
                     `(,left
                       anzu
                       auto-compile
                       ,second-left
                       major-mode
                       (process :when active)
                       ((flycheck-error flycheck-warning flycheck-info)
                        :when active)
                       (minor-modes :when active)
                       (mu4e-alert-segment :when active)
                       (erc-track :when active)
                       (version-control :when active)
                       (org-pomodoro :when active)
                       (org-clock :when active)
                       nyan-cat)
                     `(which-function
                       (python-pyvenv :fallback python-pyenv)
                       (battery :when active)
                       selection-info
                       input-method
                       ((buffer-encoding-abbrev))
                       (global :when active)
                       ,@additional-segments
                       buffer-position
                       hud))

  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-tddsg)))))

;;; used for setting pdf-view page
;;; FIXME: to be removed when spaceline is updated
(declare-function pdf-view-current-page 'pdf-view)
(declare-function pdf-cache-number-of-pages 'pdf-view)

(defun tddsg--pdfview-page-number ()
  (format "(%d/%d)"
          (eval (pdf-view-current-page))
          (pdf-cache-number-of-pages)))

(spaceline-define-segment line-column
  "The current line and column numbers, or `(current page/number of pages)`
in pdf-view mode (enabled by the `pdf-tools' package)."
  (if (eq 'pdf-view-mode major-mode)
      (tddsg--pdfview-page-number)
    "%l:%2c"))


(defun tddsg--create-spaceline-final (&rest additional-segments)
  "Install the modeline used by Spacemacs.

ADDITIONAL-SEGMENTS are inserted on the right, between `global' and
`buffer-position'."
  (apply 'tddsg--create-spaceline-theme
         '((persp-name
            workspace-number
            window-number)
           :fallback evil-state
           :separator "|"
           :face highlight-face)
         '(buffer-modified
           point-position
           line-column
           ;; buffer-size
           buffer-id
           remote-host)
         additional-segments))

(dolist (s '((tddsg-face-unmodified "SteelBlue3" "Unmodified buffer face.")
             (tddsg-face-modified "DarkGoldenrod2" "Modified buffer face.")
             (tddsg-face-read-only "SteelBlue3" "Read-only buffer face.")))
  (eval `(defface, (nth 0 s)
           `((t (:background ,(nth 1 s)
                             :foreground "#3E3D31"
                             :inherit 'mode-line)))
           ,(nth 2 s)
           :group 'tddsg)))


(defun tddsg--spaceline-highlight-face ()
  "Set the highlight face depending on the buffer modified status.
Set `spaceline-highlight-face-func' to
`tddsg--spaceline-highlight-face' to use this."
  (cond
   (buffer-read-only 'tddsg-face-read-only)
   ((buffer-modified-p) 'tddsg-face-modified )
   (t 'tddsg-face-unmodified)))


(defun tddsg/init-spaceline ()
  (setq spaceline-highlight-face-func 'tddsg--spaceline-highlight-face)
  (tddsg--create-spaceline-final))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INIT CUSTOM

(defun tddsg/init-custom-vars ()
  (custom-set-variables
   '(hl-todo-keyword-faces
     (quote
      (("HOLD" . "red")
       ("TODO" . "red")
       ("NEXT" . "red")
       ("OKAY" . "red")
       ("DONT" . "red")
       ("FAIL" . "red")
       ("DONE" . "red")
       ("NOTE" . "red")
       ("HACK" . "red")
       ("FIXME" . "red")
       ("XXX" . "red")
       ("XXXX" . "red")
       ("???" . "red")
       ("BUG" . "red"))))))
