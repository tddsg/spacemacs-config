;;; package --- Summary

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOAD FILES
(load-file "~/.emacs.d/private/tddsg/local/golden-ratio/golden-ratio.el")


(require 'smartparens)
(require 'company)
(require 'powerline)
(require 'buffer-move)
(require 'pdf-sync)
(require 'spaceline-segments)
(require 'spaceline)
(require 'pdf-view)
(require 'pdf-tools)
(require 'face-remap)
(require 'magit-gitflow)
(require 'whitespace)
(require 'god-mode)
(require 'god-mode-isearch)
(require 'expand-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VARIABLES

(setq tddsg--auto-truncate-lines nil)

(setq tddsg--show-linum t)

(setq tddsg--show-header-line t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PRIVATE FUNCTIONS

(defun tddsg--blank-line-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[ \t]*$")))

(defun tddsg--blank-char-p (ch)
  (or (equal (string ch) " ") (equal (string ch) "\\t")))

(defun tddsg--set-mark ()
  (push-mark (point) t nil))

(defun tddsg--auto-truncate-lines-p ()
  (and tddsg--auto-truncate-lines
       (not (derived-mode-p 'pdf-view-mode))))

(defun tddsg--toggle-truncate-lines (arg)
  (let ((inhibit-message t))
    (toggle-truncate-lines arg)))

(defun tddsg--projectile-p ()
  "Check if a projectile exists in the current buffer."
  (and projectile-mode
       (not (string= (projectile-project-name) ""))
       (not (string= (projectile-project-name) "-"))))

(defun tddsg--fix-comint-window-size ()
  "Change process window size."
  (when (derived-mode-p 'comint-mode)
    (let ((process (get-buffer-process (current-buffer))))
      (when process
        (set-process-window-size process (window-height) (window-width))))))

(defun tddsg--create-backup-file-name (fpath)
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

(defun tddsg--latex-compile ()
  (interactive)
  (save-buffer)
  (setq TeX-after-compilation-finished-functions nil)
  (TeX-command "LaTeX" 'TeX-master-file -1))

(defun tddsg--latex-compile-sync-forward ()
  (interactive)
  (save-buffer)
  (setq TeX-after-compilation-finished-functions
        (lambda (file)
          (select-window (selected-window))
          (call-interactively 'pdf-sync-forward-search)))
  (TeX-command "LaTeX" 'TeX-master-file -1))

(defun tddsg--is-small-screen ()
  (string= (system-name) "pisces"))

(require 'popwin)
(defun tddsg--compilation-finish (buffer string)
  "Function run when a compilation finishes."
  ;; show compilation window when finish
  (get-buffer-window buffer t)
  (cond (popwin:popup-window
         (set-window-buffer popwin:popup-window buffer))
        ((cl-loop for window in (window-list)
                  always (not (eq buffer (window-buffer window)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HOOK FUNCTIONS

(defun tddsg--hook-change-major-mode ()
  ;; change some weird keys
  (keyboard-translate ?\C-\[ ?\H-\[)
  (keyboard-translate ?\C-i ?\H-i)
  (keyboard-translate ?\C-m ?\H-m)
  (define-key input-decode-map (kbd "C-M-[") (kbd "H-M-["))
  (define-key input-decode-map (kbd "C-S-I") (kbd "H-I"))
  (define-key input-decode-map (kbd "C-S-M") (kbd "H-M"))
  ;; change personal dictionary of ispell
  (if (tddsg--projectile-p)
      (setq ispell-personal-dictionary (concat (projectile-project-root)
                                               "user.dict"))))

(defun tddsg--hook-prog-text-mode ()
  (if tddsg--show-linum (linum-mode 1) (linum-mode -1))
  (smartparens-mode 1)
  (column-marker-3 80)
  (whitespace-mode 1))

(defun tddsg--hook-prog-mode ()
  (flycheck-mode 1))

(defun tddsg--hook-text-mode ()
  (flyspell-mode 1))

(defun tddsg--hook-shell-mode ()
  (add-hook 'window-configuration-change-hook
            'tddsg--fix-comint-window-size nil t)
  (toggle-truncate-lines -1)
  (visual-line-mode 1)
  (rainbow-delimiters-mode-enable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INTERACTIVE FUNCTIONS

(defun tddsg/show-path-current-buffer ()
  "Show path of the current buffer."
  (interactive)
  (message "Current path: %s" (buffer-file-name)))

(defun tddsg/previous-overlay ()
  "Go to previous overlay."
  (interactive)
  (let ((posn (-  (previous-overlay-change (point)) 1)))
    (if (not (null (overlays-at posn)))
        (goto-char posn)))
  ;; (goto-char (previous-overlay-change (point)))
  ;; (while (and (not (bobp))
  ;;             (not (memq (char-syntax (char-after)) '(?w))))
  ;;   (goto-char (previous-overlay-change (point))))
  )


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

(defun tddsg/term (&optional buffer)
  "Open a `term' in the current window."
  (interactive)
  (let ((old-buf (if (= (count-windows) 1) (current-buffer)
                   (progn
                     (other-window 1)
                     (let ((buf (window-buffer))) (other-window -1) buf))))
        (old-window (frame-selected-window))
        (current-prefix-arg 4) ;; allow using C-u
        ;; (shell-buf (call-interactively 'term "/bin/bash"))
        (shell-buf (term "/bin/bash"))
        )
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

(defun tddsg/dired-duplicate-files ()
  "Duplicate files to the current folder by adding suffix \" - COPY\"."
  (interactive)
  ;; TODO: how to deal with file names having no \".\". For example: TODO files
  (dired-do-copy-regexp "\\(.*\\)\\.\\(.*\\)" "\\1 - (COPY).\\2"))

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
  (let ((current-char (char-after))
        (previous-char (char-before)))
    (if (memq (char-syntax current-char) '(?w ?_))
        (if (and (not (null previous-char))
                 (memq (char-syntax previous-char) '(?w ?_))
                 (not (region-active-p)))
            (progn (backward-word)
                   (call-interactively 'er/expand-region))
          (call-interactively 'er/expand-region))
      (if (< (point) (mark)) (set-mark-command nil))
      (call-interactively  'tddsg/mark-sexp))))

(defun tddsg/mark-paragraph ()
  "Mark the paragraph."
  (interactive)
  (if (region-active-p) (forward-paragraph 1)
    (progn
      (beginning-of-line)
      (beginning-of-line)
      (backward-paragraph 1)
      (next-line)
      (backward-paragraph 1)
      (beginning-of-line)
      (beginning-of-line)
      (if (looking-at "[[:space:]]*$") (next-line 1))
      (beginning-of-line)
      (beginning-of-line)
      (set-mark-command nil)
      (forward-paragraph 1))))

(defun tddsg/comment-paragraph ()
  "Comment the paragraph."
  (interactive)
  (tddsg/mark-paragraph)
  (call-interactively 'comment-dwim-2))

(defun tddsg/smart-kill-sexp ()
  "Kill sexp smartly"
  (interactive)
  (defun delete-next-spaces ()
    (cl-loop while (equal (char-syntax (char-after)) ? )
             do (delete-char 1))
    (if (and (not (null (char-before)))
             (memq (char-syntax (char-before)) '(?w ))
             (memq (char-syntax (char-after)) '(?w ?( ?) ?_ )))
        (just-one-space)))
  (cond ((equal (char-syntax (char-after)) ? )
         (delete-next-spaces))
        ((or (memq (char-syntax (char-after)) '(?. ?' ?\\))
             (and (equal (char-syntax (char-after)) ?()
                  (null (sp-get-paired-expression)))
             (and (equal (char-syntax (char-after)) ?))
                  (null (sp-get-paired-expression t))))
         (delete-char 1)
         (delete-next-spaces))
        (t
         (call-interactively 'sp-kill-sexp)
         (delete-next-spaces))))

(defun tddsg/helm-do-ag (arg)
  "Search by Helm-Ag in the current directory, \
or in a custom directory when prefix-argument is given (C-u)."
  (interactive "P")
  (let ((text (if (region-active-p)
                  (let* ((begin (region-beginning))
                         (end (region-end))
                         (text (buffer-substring-no-properties begin end))
                         (text (string-trim text))
                         (text (replace-regexp-in-string " " "\\\\ " text)))
                    text)
                (thing-at-point 'word))))
    (progn
      (if (null arg)
          (progn
            (message "TEXT: %s" text)
            (helm-do-ag (expand-file-name default-directory) text))
        (call-interactively 'helm-do-ag)))))

(defun tddsg/join-with-beneath-line ()
  "Join the current line to the line beneath it."
  (interactive)
  (delete-indentation 1)
  (let ((current-char (char-after)))
    (if (memq (char-syntax current-char) '(?w ?_ ?\" ?\( ?< ?>))
        (just-one-space))))

(defun tddsg/join-to-above-line ()
  "Join the current line to the line above it."
  (interactive)
  (delete-indentation)
  (delete-horizontal-space)
  (let ((current-char (char-after)))
    (if (memq (char-syntax current-char) '(?w ?_ ?\" ?\( ?< ?>))
        (just-one-space))))

(defun tddsg/one-space ()
  "Just one space in a region or in the current location."
  (interactive)
  (if (region-active-p)
      (save-excursion
        (save-restriction
          (narrow-to-region (region-beginning) (region-end))
          (goto-char (point-min))
          (while (re-search-forward "\\s-+" nil t)
            (replace-match " "))))
    (just-one-space)))

(defun tddsg/one-or-zero-space ()
  "Delete the space if there is only 1 space,
replace all spaces by 1 if there is more than 1,
insert a new space if there is none"
  (interactive)
  (if (region-active-p)
      (save-excursion
        (save-restriction
          (narrow-to-region (region-beginning) (region-end))
          (goto-char (point-min))
          (while (re-search-forward "\\s-+" nil t)
            (replace-match " "))))
    (if (tddsg--blank-char-p (preceding-char)) (forward-char -1))
    (if (tddsg--blank-char-p (following-char))
        (if (or (tddsg--blank-char-p (preceding-char))
                (tddsg--blank-char-p (char-after (+ (point) 1))))
            (just-one-space)
          (delete-char 1))
      (just-one-space))))

(defun tddsg/one-space-or-blank-line ()
  "Just one space or one line in a region or in the current location."
  (interactive)
  (if (region-active-p)
      (save-excursion
        (save-restriction
          (narrow-to-region (region-beginning) (region-end))
          (goto-char (point-min))
          (while (re-search-forward "\\s-+" nil t)
            (replace-match " "))))
    (if (tddsg--blank-line-p)
        (delete-blank-lines)
      (tddsg/one-or-zero-space))))

(defun tddsg/kill-ring-save (arg)
  "Save the current region (or line) to the `kill-ring'
after stripping extra whitespace and new lines"
  (interactive "P")
  (if (null arg)
      (call-interactively 'kill-ring-save)
    (if (region-active-p)
        (let* ((begin (region-beginning))
               (end (region-end))
               (text (buffer-substring-no-properties begin end))
               (text (replace-regexp-in-string "\n" " " text))
               (text (replace-regexp-in-string "\\s-+" " " text))
               (text (string-trim text)))
          (kill-new text)
          (deactivate-mark))
      (call-interactively 'kill-ring-save))))

(defun tddsg/pdf-view-kill-ring-save (arg)
  "Save the current region (or line) to the `kill-ring'
after stripping extra whitespace and new lines"
  (interactive "P")
  (if (null arg)
      (call-interactively 'pdf-view-kill-ring-save)
    (pdf-view-assert-active-region)
    (let* ((text (pdf-view-active-region-text))
           (text (mapconcat 'identity text " "))
           (text (replace-regexp-in-string "\n" " " text))
           (text (replace-regexp-in-string "\\s-+" " " text))
           (text (string-trim text)))
      (pdf-view-deactivate-region)
      (kill-new text))))

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

(defun tddsg/enable-company-auto-suggest ()
  (interactive)
  (setq company-idle-delay 0.5))

(defun tddsg/disable-company-auto-suggest ()
  (interactive)
  (setq company-idle-delay 300))

(defun tddsg/toggle-auto-truncate-lines ()
  "Toggle auto truncate lines."
  (interactive)
  (cond (tddsg--auto-truncate-lines
         (message "Disable auto truncate lines")
         (setq tddsg--auto-truncate-lines nil))
        (t
         (message "Enable auto truncate lines")
         (setq tddsg--auto-truncate-lines t))))

(defun tddsg/toggle-show-mode-line ()
  (interactive)
  (if (bound-and-true-p mode-line-format)
      (setq mode-line-format nil)
    (setq mode-line-format (default-value 'mode-line-format))))

(defun tddsg/toggle-linum ()
  "Toggle show/hide line number."
  (interactive)
  (cond ((null linum-mode)
         (setq tddsg--show-linum t)
         (global-linum-mode 1))
        (t
         (setq tddsg--show-linum nil)
         (global-linum-mode -1))))

(defun tddsg/toggle-shell-scroll-to-bottomon-on-output ()
  "Toggle shell scroll to the last line on output."
  (interactive)
  (if (derived-mode-p 'shell-mode)
      (cond (comint-scroll-to-bottom-on-output
             (setq comint-scroll-to-bottom-on-output nil)
             (setq mode-name "Shell ⚡⚡⚡"))
            (t
             (setq comint-scroll-to-bottom-on-output t)
             (setq mode-name "Shell")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INIT CONFIGS

(defun tddsg/init-configs ()
  ;; specific setting for each machines
  (if (tddsg--is-small-screen)
      (progn
        (global-linum-mode -1)
        (set-default 'truncate-lines t)   ;; disable truncate line
        (setq tddsg--auto-truncate-lines t)
        (setq golden-ratio-adjust-factor 0.9)
        (setq golden-ratio-balance nil)
        (golden-ratio-mode))
    (setq tddsg--auto-truncate-lines nil)
    (setq golden-ratio-adjust-factor 1.618)
    (setq golden-ratio-balance nil)
    (golden-ratio-mode))

  ;; visual interface setting
  (display-time)                    ;; show time in mode line
  (global-hl-todo-mode 1)           ;; highlight current line
  (blink-cursor-mode 0)             ;; turn on blinking
  (setq blink-cursor-blinks 15)     ;; blink 15 times
  (setq-default fill-column 75)     ;; max size of a line for fill-or-unfill
  (setq text-scale-mode-step 1.1)   ;; scale changing font size
  (setq frame-title-format          ;; frame title
        '("" invocation-name " - "
          (:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name)) "%b"))))

  ;; windows setting
  (setq window-combination-resize nil)   ;; stop Emacs from automatically resize windows

  ;; scrolling
  (spacemacs/toggle-smooth-scrolling-off)  ;; disable smooth-scrolling
  (setq redisplay-dont-pause t
        scroll-conservatively 10000
        scroll-margin 5
        scroll-preserve-screen-position 't)

  ;; mode paragraph setting
  (setq paragraph-separate "[ \t\f]*$"
        paragraph-start "\f\\|[ \t]*$")

  ;; save
  ;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace)

  ;; zoom
  (require 'zoom-frm)

  ;; auto-completetion
  (setq dabbrev-case-replace nil)

  ;; visual line mode
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

  ;; spell
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra")
        ispell-dictionary "english"
        ispell-personal-dictionary "~/.emacs.d/user.dict")

  (setq set-mark-command-repeat-pop t)
  (defadvice find-file (before set-mark activate) (tddsg--set-mark))
  (defadvice isearch-update (before set-mark activate) (tddsg--set-mark))
  (defadvice beginning-of-buffer (before set-mark activate) (tddsg--set-mark))
  (defadvice end-of-buffer (before set-mark activate) (tddsg--set-mark))
  (defadvice merlin-locate (before set-mark activate) (tddsg--set-mark))

  ;; disable company auto suggest
  (setq company-idle-delay 300)
  (setq company-tooltip-idle-delay 300)

  ;; auto truncate lines when necessary on changing window
  (defun tddsg--truncate-lines (orig-func &rest args)
    (if (tddsg--auto-truncate-lines-p) (tddsg--toggle-truncate-lines 1))
    (apply orig-func args)
    (if (tddsg--auto-truncate-lines-p) (tddsg--toggle-truncate-lines -1)))
  ;; do not advice 'select-window since this causes buffer overflow
  (advice-add 'windmove-do-window-select :around #'tddsg--truncate-lines)
  (advice-add 'select-window-by-number :around #'tddsg--truncate-lines)
  (advice-add 'other-window :around #'tddsg--truncate-lines)

  ;; advice changing buffer
  (defun tddsg--enable-truncate-lines (orig-func &rest args)
    (apply orig-func args)
    (if (tddsg--auto-truncate-lines-p) (tddsg--toggle-truncate-lines 1)))
  (advice-add 'helm-mini :around #'tddsg--enable-truncate-lines)
  (advice-add 'helm-find-files :around #'tddsg--enable-truncate-lines)

  ;; pdf-view
  (defun update-pdf-view-theme ()
    (when (derived-mode-p 'pdf-view-mode)
      (cond ((eq spacemacs--cur-theme 'spacemacs-dark)
             (if (not (bound-and-true-p pdf-view-midnight-minor-mode))
                 (pdf-view-midnight-minor-mode)))
            ((eq spacemacs--cur-theme 'leuven)
             (if (bound-and-true-p pdf-view-midnight-minor-mode)
                 (pdf-view-midnight-minor-mode -1))))))
  (defadvice spacemacs/cycle-spacemacs-theme (after pdf-view activate)
    (mapc (lambda (window)
            (with-current-buffer (window-buffer window)
              (update-pdf-view-theme)))
          (window-list)))
  (add-hook 'pdf-view-mode-hook 'update-pdf-view-theme)

  ;; mode editing setting
  (electric-pair-mode t)
  (delete-selection-mode t)                            ;; delete selection by keypress
  (setq require-final-newline t)                       ;; newline at end of file
  (defadvice newline (after indent activate) (indent-according-to-mode))
  ;; (setq company-idle-delay 200)         ;; set delay time by default
  (global-company-mode)

  ;; some Emacs threshold
  (setq max-lisp-eval-depth 50000)
  (setq max-specpdl-size 50000)

  ;; mode-line setting
  (setq powerline-default-separator 'wave)

  ;; golden-ratio
  (defadvice keyboard-quit (after golden-ratio activate) (golden-ratio 1))
  (defadvice flyspell-correct-word-before-point (after golden-ratio activate)
    (golden-ratio 1))

  ;; themes
  (defun tddsg--update-cursor ()
    (cond ((or (bound-and-true-p god-mode)
               (bound-and-true-p god-global-mode))
           (set-cursor-color "purple"))
          ((eq spacemacs--cur-theme 'leuven)
           (set-cursor-color "dark orange"))
          ((eq spacemacs--cur-theme 'spacemacs-dark)
           (set-cursor-color "lime green"))))
  (add-hook 'buffer-list-update-hook 'tddsg--update-cursor)

  ;; isearch
  (defun tddsg--isearch-show-case-fold (orig-func &rest args)
    (apply orig-func args)
    (if isearch-case-fold-search
        (spacemacs|diminish isearch-mode "⚡ISearch[ci]⚡")
      (spacemacs|diminish isearch-mode "⚡ISearch[CS]⚡")))
  (advice-add 'isearch-mode :around #'tddsg--isearch-show-case-fold)
  (advice-add 'isearch-repeat :around #'tddsg--isearch-show-case-fold)
  (advice-add 'isearch-toggle-case-fold :around #'tddsg--isearch-show-case-fold)

  ;; compilation
  (setq compilation-ask-about-save nil
        compilation-window-height 15)
  (setq compilation-finish-functions
        (append compilation-finish-functions 'tddsg--compilation-finish))

  ;; shell
  (setq comint-prompt-read-only nil)
  (defadvice shell (after linum activate) (linum-mode 1))
  (setq shell-default-shell 'ansi-term)
  (add-hook 'shell-mode-hook 'tddsg--hook-shell-mode)

  ;; smartparens
  (smartparens-global-mode)

  ;; backup
  (setq make-backup-files t)

  (setq make-backup-file-name-function 'tddsg--create-backup-file-name)

  ;; evil mode
  (evil-mode -1)
  (setq-default evil-cross-lines t)

  ;; dired
  (add-to-list 'savehist-additional-variables 'helm-dired-history-variable)
  (setq dired-guess-shell-alist-user
        '(("\\.pdf\\'" "okular &")
          ("\\.txt\\'" "gedit")))

  ;; helm setting
  (setq helm-ag-insert-at-point 'symbol)     ;; insert symbol in helm-ag
  (setq helm-split-window-in-side-p t)
  (setq helm-split-window-default-side 'below)


  ;; diminish
  (spacemacs|diminish whitespace-mode "")
  (spacemacs|diminish super-save-mode "")
  (spacemacs|diminish company-mode "")
  (spacemacs|diminish which-key-mode "")
  (spacemacs|diminish yas-minor-mode "")
  (spacemacs|diminish utop-minor-mode "")
  (spacemacs|diminish golden-ratio-mode "")
  (spacemacs|diminish with-editor-mode "")
  (spacemacs|diminish compilation-in-progress "")
  (spacemacs|diminish server-buffer-clients "")
  (spacemacs|diminish pdf-view-midnight-minor-mode "")
  (spacemacs|diminish auto-revert-mode " ↺")
  (spacemacs|diminish god-local-mode " ⚡☢⚡☢⚡")
  (spacemacs|diminish abbrev-mode " ↹")
  (spacemacs|diminish smartparens-mode " ♓")
  (spacemacs|diminish rainbow-mode " ☔")
  (spacemacs|diminish auto-fill-function " ↪")
  (spacemacs|diminish visual-line-mode " ↩")
  (spacemacs|diminish merlin-mode " ♏")
  (spacemacs|diminish magit-gitflow-mode " ♒")
  (spacemacs|diminish flycheck-mode " ⚐")
  (spacemacs|diminish flyspell-mode " ✔")
  (spacemacs|diminish holy-mode " ☼")
  (spacemacs|diminish projectile-mode " ♖")
  (spacemacs|diminish compilation-minor-mode "⚡⚡⚡COMPILING⚡⚡⚡")

  ;; hooks, finally hook
  (add-hook 'LaTeX-mode-hook 'tddsg--hook-prog-text-mode)
  (add-hook 'TeX-mode-hook 'tddsg--hook-prog-text-mode)
  (add-hook 'tex-mode-hook 'tddsg--hook-prog-text-mode)
  (add-hook 'prog-mode-hook 'tddsg--hook-prog-text-mode)
  (add-hook 'text-mode-hook 'tddsg--hook-prog-text-mode)
  (add-hook 'prog-mode-hook 'tddsg--hook-prog-mode)
  (add-hook 'text-mode-hook 'tddsg--hook-text-mode)
  (add-hook 'change-major-mode-hook 'tddsg--hook-change-major-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INIT KEYS

(defun tddsg/init-keys ()
  ;; unbind some weird keys
  (global-set-key (kbd "<home>") 'crux-move-beginning-of-line)
  (global-set-key (kbd "<escape>") 'god-mode-all)

  (global-set-key (kbd "C-<backspace>") 'backward-kill-word)
  (global-set-key (kbd "C-<delete>") 'kill-word)
  (global-set-key (kbd "C-<left>") 'left-word)
  (global-set-key (kbd "C-<right>") 'right-word)
  (global-set-key (kbd "C-+") 'zoom-in)
  (global-set-key (kbd "C--") 'zoom-out)
  (global-set-key (kbd "C-`") 'goto-last-change)
  (global-set-key (kbd "C-j") 'avy-goto-word-1)
  (global-set-key (kbd "C-o") 'helm-semantic-or-imenu)
  (global-set-key (kbd "C-q") 'goto-last-change)
  (global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
  (global-set-key (kbd "C-/") 'undo)
  (global-set-key (kbd "C-;") 'iedit-mode)
  (global-set-key (kbd "C-*") 'evil-copy-from-above)
  (global-set-key (kbd "C-^") 'tddsg/join-with-beneath-line)
  (global-set-key (kbd "C-_") 'tddsg/join-to-above-line)
  (global-set-key (kbd "C-\\") 'sp-split-sexp)

  (global-set-key (kbd "C-S-<backspace>") 'kill-whole-line)
  (global-set-key (kbd "C-S-/") 'undo-tree-redo)
  (global-set-key (kbd "C-M-O") 'helm-imenu-anywhere)
  (global-set-key (kbd "C-M-k") 'tddsg/smart-kill-sexp)
  (global-set-key (kbd "C-M-SPC") 'tddsg/smart-mark-sexp)
  (global-set-key (kbd "C-M-_") 'flip-frame)
  (global-set-key (kbd "C-M-+") 'flop-frame)
  (global-set-key (kbd "C-M-;") 'tddsg/comment-paragraph)

  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x t") 'transpose-paragraphs)
  (global-set-key (kbd "C-x _") 'shrink-window)
  (global-set-key (kbd "C-x m") 'monky-status)
  (global-set-key (kbd "C-x g") 'magit-status)
  ;; (global-set-key (kbd "C-x G") 'magit-diff)
  (global-set-key (kbd "C-x w s") 'tddsg/save-file-as-and-open-file)

  (global-set-key (kbd "C-x C-d") 'helm-dired-history-view)
  (global-set-key (kbd "C-x C-b") 'switch-to-buffer)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)

  (global-set-key (kbd "C-x C-z") nil)

  (global-set-key [?\H-M] 'helm-mini)
  (global-set-key [?\H-m] 'helm-mini)
  (global-set-key [?\H-i] 'swiper)
  (global-set-key [?\H-I] 'swiper)

  (global-set-key (kbd "C-c f") 'projectile-find-file)
  (global-set-key (kbd "C-c o") 'helm-occur)
  (global-set-key (kbd "C-c i") 'ivy-resume)
  (global-set-key (kbd "C-c j") 'avy-resume)
  (global-set-key (kbd "C-c h") 'helm-resume)
  (global-set-key (kbd "C-c s") 'swiper)
  (global-set-key (kbd "C-c r") 'projectile-replace)
  (global-set-key (kbd "C-c R") 'projectile-replace-regexp)
  (global-set-key (kbd "C-c g") 'tddsg/helm-do-ag)
  (global-set-key (kbd "C-c d") 'crux-duplicate-current-line-or-region)
  (global-set-key (kbd "C-c M") 'tddsg/shell-other-window)
  (global-set-key (kbd "C-c m") 'tddsg/shell-current-window)
  (global-set-key (kbd "C-c t") 'tddsg/term)

  (global-set-key (kbd "C-c C-g") 'helm-projectile-ag)
  (global-set-key (kbd "C-c C-SPC") 'helm-all-mark-rings)
  (global-set-key (kbd "C-c C-c") 'tddsg/compile)

  (global-set-key (kbd "M-SPC") 'tddsg/one-space-or-blank-line)
  (global-set-key (kbd "M-<backspace>") 'backward-kill-word)
  (global-set-key (kbd "M-<delete>") 'kill-word)
  (global-set-key (kbd "M-w") 'tddsg/kill-ring-save)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "M-'") 'dabbrev-completion)
  (global-set-key (kbd "M--") 'delete-window)
  (global-set-key (kbd "M-+") 'delete-other-windows)
  (global-set-key (kbd "M--") 'delete-window)
  (global-set-key (kbd "M-_") 'split-window-below)
  (global-set-key (kbd "M-|") 'split-window-right)
  (global-set-key (kbd "M-=") 'transpose-frame)
  (global-set-key (kbd "M-\\") 'sp-splice-sexp)
  (global-set-key (kbd "M-;") 'comment-dwim-2)
  (global-set-key (kbd "M-?") 'company-complete)
  (global-set-key (kbd "C-M-?") 'helm-company)
  (global-set-key (kbd "M-H") 'tddsg/mark-line)
  (global-set-key (kbd "M-h") 'tddsg/mark-paragraph)

  (global-set-key (kbd "M-[") 'windmove-left)
  (global-set-key (kbd "M-]") 'windmove-right)
  (global-set-key (kbd "H-[") 'windmove-up)
  (global-set-key (kbd "C-]") 'windmove-down)

  (global-set-key (kbd "H-M-[") 'previous-buffer)
  (global-set-key (kbd "C-M-]") 'next-buffer)
  (global-set-key (kbd "C-M-{") 'winner-undo)
  (global-set-key (kbd "C-M-}") 'winner-redo)

  (global-set-key (kbd "M-S-<up>") 'move-text-up)
  (global-set-key (kbd "M-S-<down>") 'move-text-down)
  (global-set-key (kbd "M-S-SPC") 'delete-blank-lines)

  (global-set-key (kbd "M-m f p") 'tddsg/show-path-current-buffer)
  (global-set-key (kbd "M-m h g") 'helm-do-grep-ag)
  (global-set-key (kbd "M-m h o") 'helm-occur)
  (global-set-key (kbd "M-m h s") 'helm-semantic-or-imenu)
  (global-set-key (kbd "M-m s d") 'dictionary-search)
  (global-set-key (kbd "M-m S i") 'ispell-buffer)
  (global-set-key (kbd "M-m S s") 'ispell-continue)
  (global-set-key (kbd "M-m S p") 'flyspell-correct-previous-word-generic)
  (global-set-key (kbd "M-m S c") 'flyspell-correct-word-before-point)
  (global-set-key (kbd "M-m m S") 'shell)
  (global-set-key (kbd "M-m m s") 'tddsg/shell-other-window)
  (global-set-key (kbd "M-m w t") 'transpose-frame)
  (global-set-key (kbd "M-m w o") 'flop-frame)
  (global-set-key (kbd "M-m w i") 'flip-frame)
  (global-set-key (kbd "M-m T l") 'tddsg/toggle-show-mode-line)
  (global-set-key (kbd "M-m T h") 'tddsg/toggle-show-header-line)

  (define-key spacemacs-default-map-root-map (kbd "M-m l") nil)
  (global-set-key (kbd "M-m l c") 'langtool-check)
  (global-set-key (kbd "M-m l b") 'langtool-correct-buffer)
  (global-set-key (kbd "M-m l d") 'langtool-check-done)
  (global-set-key (kbd "M-m l n") 'langtool-goto-next-error)
  (global-set-key (kbd "M-m l p") 'langtool-goto-previous-error)
  (global-set-key (kbd "M-m l v") 'visual-line-mode)

  (global-set-key (kbd "M-s r") 'spacemacs/evil-search-clear-highlight)
  (global-set-key (kbd "M-s i") 'ispell-buffer)
  (global-set-key (kbd "M-s s") 'ispell-continue)
  (global-set-key (kbd "M-s f") 'flyspell-buffer)
  (global-set-key (kbd "M-s p") 'flyspell-correct-previous-word-generic)
  (global-set-key (kbd "M-s c") 'flyspell-correct-word-before-point)
  (global-set-key (kbd "M-s n") 'flyspell-goto-next-error)
  (global-set-key (kbd "M-s k") 'sp-splice-sexp-killing-around)


  ;; workspaces transient
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

  ;; isearch
  (define-key isearch-mode-map (kbd "C-.")
    'tddsg/yank-current-word-to-isearch-buffer)

  (define-key swiper-map (kbd "C-.")
    'tddsg/yank-current-word-to-minibuffer)

  ;; minibuffer
  (define-key minibuffer-local-map (kbd "C-.")
    'tddsg/yank-current-word-to-minibuffer)

  ;; shell
  (define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
  (define-key shell-mode-map (kbd "C-c C-s")
    'tddsg/toggle-shell-scroll-to-bottomon-on-output)

  ;; undo tree
  (define-key undo-tree-map (kbd "C-_") nil)
  (define-key undo-tree-map (kbd "M-_") nil)

  ;; magit
  (require 'magit)
  (define-key magit-mode-map (kbd "M-1") nil)
  (define-key magit-mode-map (kbd "M-2") nil)
  (define-key magit-mode-map (kbd "M-3") nil)
  (define-key magit-mode-map (kbd "M-4") nil)
  (define-key magit-mode-map (kbd "M-5") nil)
  (define-key magit-mode-map (kbd "M-6") nil)
  (define-key magit-mode-map (kbd "M-7") nil)
  (define-key magit-mode-map (kbd "M-8") nil)
  (define-key magit-mode-map (kbd "M-9") nil)
  (define-key magit-mode-map (kbd "M-0") nil)
  (define-key magit-status-mode-map (kbd "M-1") nil)
  (define-key magit-status-mode-map (kbd "M-2") nil)
  (define-key magit-status-mode-map (kbd "M-3") nil)
  (define-key magit-status-mode-map (kbd "M-4") nil)
  (define-key magit-status-mode-map (kbd "M-5") nil)
  (define-key magit-status-mode-map (kbd "M-6") nil)
  (define-key magit-status-mode-map (kbd "M-7") nil)
  (define-key magit-status-mode-map (kbd "M-8") nil)
  (define-key magit-status-mode-map (kbd "M-9") nil)
  (define-key magit-status-mode-map (kbd "M-0") nil)

  ;; god-mode
  (define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
  (define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)
  (define-key god-local-mode-map (kbd "<escape>") 'god-mode-all)
  (define-key god-local-mode-map (kbd "i") 'god-mode-all)

  ;; windmove
  (global-set-key (kbd "S-<left>") 'windmove-left)
  (global-set-key (kbd "S-<right>") 'windmove-right)
  (global-set-key (kbd "S-<up>") 'windmove-up)
  (global-set-key (kbd "S-<down>") 'windmove-down)

  ;; buffer-move
  (global-set-key (kbd "C-S-<left>") 'buf-move-left)
  (global-set-key (kbd "C-S-<right>") 'buf-move-right)
  (global-set-key (kbd "C-S-<up>") 'buf-move-up)
  (global-set-key (kbd "C-S-<down>") 'buf-move-down)
  (define-key spacemacs-default-map-root-map (kbd "M-m b m") nil)
  (global-set-key (kbd "M-m b m b") 'buf-move-left)
  (global-set-key (kbd "M-m b m n") 'buf-move-down)
  (global-set-key (kbd "M-m b m p") 'buf-move-up)
  (global-set-key (kbd "M-m b m f") 'buf-move-right)

  ;; buffer-clone
  (global-set-key (kbd "C-M-S-<left>") 'buf-clone-left)
  (global-set-key (kbd "C-M-S-<right>") 'buf-clone-right)
  (global-set-key (kbd "C-M-S-<up>") 'buf-clone-up)
  (global-set-key (kbd "C-M-S-<down>") 'buf-clone-down)
  (global-set-key (kbd "C-M-s-7") 'buf-clone-left)
  (global-set-key (kbd "C-M-s-8") 'buf-clone-down)
  (global-set-key (kbd "C-M-s-9") 'buf-clone-up)
  (global-set-key (kbd "C-M-s-0") 'buf-clone-right)

  ;; LaTeX-mode
  (define-key TeX-mode-map (kbd "<f5>") 'tddsg--latex-compile)
  (define-key TeX-mode-map (kbd "<f6>") 'tddsg--latex-compile-sync-forward)
  (define-key TeX-mode-map (kbd "C-j") nil)
  (eval-after-load 'latex
    '(progn
       (define-key LaTeX-mode-map (kbd "C-j") nil)
       (define-key LaTeX-mode-map (kbd "\"") nil)
       (define-key LaTeX-mode-map (kbd "C-c C-g") nil)))

  ;; Tuareg mode
  (define-key tuareg-mode-map (kbd "<f5>") (kbd "C-c C-c C-j"))
  (define-key tuareg-mode-map (kbd "M-q") nil)

  ;; pdf-tools
  (define-key pdf-view-mode-map (kbd "C-<home>") 'pdf-view-first-page)
  (define-key pdf-view-mode-map (kbd "C-<end>") 'pdf-view-last-page)
  (define-key pdf-view-mode-map (kbd "M-{") 'pdf-view-previous-page-command)
  (define-key pdf-view-mode-map (kbd "M-}") 'pdf-view-next-page-command)
  (define-key pdf-view-mode-map (kbd "M-w") 'tddsg/pdf-view-kill-ring-save)
  (define-key pdf-view-mode-map (kbd "<mouse-8>") 'pdf-history-backward)
  (define-key pdf-view-mode-map (kbd "<mouse-9>") 'pdf-history-forward)


  ;; flyspell
  (define-key flyspell-mode-map (kbd "C-;") nil)

  ;; dired mode
  (define-key dired-mode-map (kbd "C-^") 'tddsg/dired-home)
  (define-key dired-mode-map (kbd "M-C") 'tddsg/dired-duplicate-files)

  ;; smartparens
  (define-key smartparens-mode-map (kbd "M-s") nil)

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

  ;; company mode
  (define-key company-active-map (kbd "M-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-.") 'company-show-location)

  ;; reassign key-chords
  (key-chord-define-global "ji" 'indent-region)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INIT THEMES

(defcustom tddsg-themes nil
  "Association list of override faces to set for different custom themes.")

(defun tddsg--read-custom-themes (alist-symbol key value)
  "Set VALUE of a KEY in ALIST-SYMBOL."
  (set alist-symbol
       (cons (list key value) (assq-delete-all key (eval alist-symbol)))))

;; override some settings of the leuven theme
(defun tddsg--custom-theme-leuven ()
  (tddsg--read-custom-themes
   'tddsg-themes
   'leuven
   '((bold ((t (:foreground "salmon4" :weight bold))))
     (bold-italic ((t (:foreground "salmon4" :slant italic :weight bold))))
     ;; cursors & line
     (cursor ((t (:background "dark orange"))))
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
(defun tddsg--custom-theme-spacemacs-dark ()
  (tddsg--read-custom-themes
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
     (font-latex-sedate-face ((t (:foreground "#64A873" :weight normal))))
     (font-latex-subscript-face ((t (:height 0.96))))
     (font-latex-superscript-face ((t (:height 0.96))))
     (font-latex-sectioning-0-face ((t (:foreground "lawn green" :weight bold :height 1.4))))
     (font-latex-sectioning-1-face ((t (:foreground "deep sky blue" :weight bold :height 1.4))))
     (font-latex-sectioning-2-face ((t (:foreground "royal blue" :weight bold :height 1.2))))
     (lazy-highlight ((t (:background "dark goldenrod" :foreground "gray10" :weight normal))))
     )))

(defun tddsg--custom-common ()
  ;; custom variables
  (custom-set-variables
   '(sp-highlight-wrap-overlay nil))
  ;; custom faces
  (custom-set-faces
   '(sp-pair-overlay-face ((t nil)))
   '(sp-wrap-overlay-face ((t nil)))
   '(sp-wrap-tag-overlay-face ((t nil)))))

(defun tddsg--override-theme ()
  (dolist (theme-settings tddsg-themes)
    (let ((theme (car theme-settings))
          (faces (cadr theme-settings)))
      (if (member theme custom-enabled-themes)
          (dolist (face faces)
            (custom-theme-set-faces theme face))))))

(defun tddsg/init-themes ()
  ;; load the custom theme
  (tddsg--custom-common)
  (tddsg--custom-theme-leuven)
  (tddsg--custom-theme-spacemacs-dark)
  (tddsg--override-theme)
  ;; and defadvice load-theme function
  (defadvice load-theme (after theme-set-overrides activate)
    "Set override faces for different custom themes."
    (tddsg--override-theme)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INIT HEADER LINE

(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun tddsg--header-file-path ()
  "Create file path for the header line."
  (let* ((file-path (if buffer-file-name
                        (abbreviate-file-name buffer-file-name)
                      (buffer-name)))
         (dir-name  (if buffer-file-name
                        (file-name-directory file-path) ""))
         (file-name  (if buffer-file-name
                         (file-name-nondirectory buffer-file-name)
                       (buffer-name)))
         (name-len (length file-name))
         (dir-len (length dir-name))
         (drop-str "[...]")
         (dir-display-len (- (window-body-width) (length drop-str) 3
                               (length (projectile-project-name))
                               name-len 1)))
    (if (> (length file-path) dir-display-len)
        (if (> name-len dir-display-len)
            (concat "▷ " (with-face file-name :foreground "DarkOrange3"))
          (concat "▷ "
                  (with-face (substring dir-name 0 (/ dir-display-len 2))
                             :foreground "DeepSkyBlue3")
                  (with-face drop-str :foreground "DeepSkyBlue3")
                  (with-face (substring dir-name
                                        (- dir-len (/ dir-display-len 2))
                                        (- dir-len 1))
                             :foreground "DeepSkyBlue3")
                  (with-face "/" :foreground "DeepSkyBlue3")
                  (with-face file-name :foreground "DarkOrange3")))
      (concat "▷ "
              (with-face dir-name :foreground "DeepSkyBlue3")
              (with-face file-name :foreground "DarkOrange3")))))

(defun tddsg--header-project-path ()
  "Create project path for the header line."
  (if (tddsg--projectile-p)
      (concat "♖ "
              (with-face (projectile-project-name) :foreground "DarkOrange3")
              " ")
    ""))

;; set font of header line
(custom-set-faces
 '(header-line
   ((default :inherit mode-line)
    (((type tty))
     :foreground "black" :background "yellow" :inverse-video nil)
    (((class color grayscale) (background light))
     :background "grey90" :foreground "grey20" :box nil)
    (((class color grayscale) (background dark))
     :background "#212026" :foreground "gainsboro" :box nil)
    (((class mono) (background light))
     :background "white" :foreground "black"
     :inverse-video nil :box nil :underline t)
    (((class mono) (background dark))
     :background "black" :foreground "white"
     :inverse-video nil :box nil :underline t))))

(defun tddsg--create-header-line ()
  "Create the header line of a buffer."
  '("" ;; invocation-name
    (:eval
     (concat (tddsg--header-project-path)
             (tddsg--header-file-path)))))

;; List of buffer prefixes that the header-line is hidden
(defvar tddsg--excluded-buffer-prefix (list "*helm"
                                            "*spacemacs*"))

(defun tddsg--header-exclude-p (buffer-name)
  (cl-loop for buffer-prefix in tddsg--excluded-buffer-prefix
           thereis (string-match-p (regexp-quote buffer-prefix) buffer-name)))

(defun tddsg--update-header-line ()
  "Update header line of the active buffer and remove from all other."
  (if tddsg--show-header-line
      (cl-loop for window in (window-list) do
               (with-current-buffer (window-buffer window)
                 (when (not (tddsg--header-exclude-p
                             (buffer-name (window-buffer window))))
                   (if (eq (window-buffer window)
                           (window-buffer (selected-window)))
                       ;; activate header-line of the active buffer
                       (setq header-line-format (tddsg--create-header-line))
                     ;; dim header-line of inactive buffers
                     (setq header-line-format
                           `(:propertize ,(tddsg--create-header-line)
                                         face (list :foreground "grey55")))))))
    (cl-loop for window in (window-list) do
             (with-current-buffer (window-buffer window)
               (setq header-line-format nil)))))

(defun tddsg/toggle-show-header-line ()
  "Toggle show/hide header line of all buffers."
  (interactive)
  (if tddsg--show-header-line
      (setq tddsg--show-header-line nil)
    (setq tddsg--show-header-line t))
  (tddsg--update-header-line))

;; update header line of each buffer
(add-hook 'buffer-list-update-hook 'tddsg--update-header-line)
(add-hook 'window-configuration-change-hook 'tddsg--update-header-line)


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
                       (version-control :when active)
                       minor-modes
                       (process :when active)
                       ((flycheck-error flycheck-warning flycheck-info)
                        :when active)
                       (mu4e-alert-segment :when active)
                       (erc-track :when active)
                       (org-pomodoro :when active)
                       (org-clock :when active)
                       nyan-cat)
                     `((global :when (not active))
                       which-function
                       (python-pyvenv :fallback python-pyenv)
                       (battery :when active)
                       selection-info
                       ,@additional-segments
                       input-method
                       (buffer-encoding-abbrev :when active)
                       (buffer-position :when active)
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

(dolist (s '((tddsg-face-unmodified "SteelBlue3"
                                    "Unmodified buffer face.")
             (tddsg-face-modified "DarkGoldenrod2"
                                  "Modified buffer face.")
             (tddsg-face-read-only "SteelBlue3"
                                   "Read-only buffer face.")))
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
   '(golden-ratio-exclude-buffer-names
     (quote
      ("*which-key*"
       "*LV*"
       "*NeoTree*"
       "*ace-popup-menu*"
       "*compilation*")))
   '(LaTeX-indent-environment-list
     (quote
      (("verbatim" current-indentation)
       ("verbatim*" current-indentation)
       ("longtable" LaTeX-indent-tabular)
       ("Form" current-indentation)
       ("tabular")
       ("tabular*")
       ("align")
       ("align*")
       ("array")
       ("eqnarray")
       ("eqnarray*")
       ("displaymath")
       ("equation")
       ("equation*")
       ("picture")
       ("tabbing")
       ("figure")
       ("small"))))
   '(pdf-view-continuous nil)
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
       ("BUG" . "red")
       ("OK" . "red"))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FINALLY, OVERRIDE OTHER EMACS'S FUNCTION

;;;;; POPWIN mode

(require 'popwin)
(defun* popwin:popup-buffer (buffer
                             &key
                             (width popwin:popup-window-width)
                             (height popwin:popup-window-height)
                             (position popwin:popup-window-position)
                             noselect
                             dedicated
                             stick
                             tail)
  "Show BUFFER in a popup window and return the popup window. If
NOSELECT is non-nil, the popup window will not be selected. If
STICK is non-nil, the popup window will be stuck. If TAIL is
non-nil, the popup window will show the last contents. Calling
`popwin:popup-buffer' during `popwin:popup-buffer' is allowed. In
that case, the buffer of the popup window will be replaced with
BUFFER."
  (interactive "BPopup buffer:\n")
  (setq buffer (get-buffer buffer))
  (popwin:push-context)
  (run-hooks 'popwin:before-popup-hook)
  (multiple-value-bind (context context-stack)
      (popwin:find-context-for-buffer buffer :valid-only t)
    (if context
        (progn
          (popwin:use-context context)
          (setq popwin:context-stack context-stack))
      (let ((win-outline (car (popwin:window-config-tree))))
        (destructuring-bind (master-win popup-win win-map)
            (let ((size (if (popwin:position-horizontal-p position) width height))
                  (adjust popwin:adjust-other-windows))
              ;; <-- original line
              ;; (popwin:create-popup-window size position adjust)
              ;; <-- new code
              (let* ((popup-win-height (- popwin:popup-window-height))
                     (orig-window (selected-window))
                     (new-window (split-window orig-window popup-win-height 'below)))
                (set-window-buffer new-window buffer)
                (list orig-window new-window nil))
              )
          (setq popwin:popup-window popup-win
                popwin:master-window master-win
                popwin:window-outline win-outline
                popwin:window-map win-map
                popwin:window-config nil
                popwin:selected-window (selected-window)))
        (popwin:update-window-reference 'popwin:context-stack :recursive t)
        (popwin:start-close-popup-window-timer))
      (with-selected-window popwin:popup-window
        (popwin:switch-to-buffer buffer)
        (when tail
          (set-window-point popwin:popup-window (point-max))
          (recenter -2)))
      (setq popwin:popup-buffer buffer
            popwin:popup-last-config (list buffer
                                           :width width :height height :position position
                                           :noselect noselect :dedicated dedicated
                                           :stick stick :tail tail)
            popwin:popup-window-dedicated-p dedicated
            popwin:popup-window-stuck-p stick)))
  (if noselect
      (setq popwin:focus-window popwin:selected-window)
    (setq popwin:focus-window popwin:popup-window)
    (select-window popwin:popup-window))
  (run-hooks 'popwin:after-popup-hook)
  popwin:popup-window)

;;; customize helm-ag
(defsubst helm-ag--marked-input ()
  (when (use-region-p)
    (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
           (text (replace-regexp-in-string " " "\\\\ " text)))
      (deactivate-mark)
      text)))


;;;;; PDF-VIEW MODE

(require 'pdf-view)

;; ;;;;; customize to allow page-up/page-down by window-size
;; (defun pdf-view-next-page-command (&optional n)
;;   (declare (interactive-only pdf-view-next-page))
;;   (interactive "p")
;;   (unless n (setq n 1))
;;   (when (> (+ (pdf-view-current-page) n)
;;            (pdf-cache-number-of-pages))
;;     (user-error "Last page"))
;;   (when (< (+ (pdf-view-current-page) n) 1)
;;     (user-error "First page"))
;;   ;;; <-- new code is inserted here
;;   (let* ((pdf-view-inhibit-redisplay t)
;;          (wdn-height (window-height (selected-window)))
;;          (lines-to-move (cond ((> n 0) (- wdn-height 4))
;;                               ((< n 0) (- 4 wdn-height))
;;                               (t 0))))
;;     (when (= (window-vscroll) (image-next-line lines-to-move))
;;       (pdf-view-goto-page (+ (pdf-view-current-page) n))
;;       (if (> n 0) (image-bob) (image-eob))))
;;   ;;; --> end of new code
;;   (force-mode-line-update)
;;   (sit-for 0)
;;   (when pdf-view--next-page-timer
;;     (cancel-timer pdf-view--next-page-timer)
;;     (setq pdf-view--next-page-timer nil))
;;   (if (or (not (input-pending-p))
;;           (and (> n 0)
;;                (= (pdf-view-current-page)
;;                   (pdf-cache-number-of-pages)))
;;           (and (< n 0)
;;                (= (pdf-view-current-page) 1)))
;;       (pdf-view-redisplay)
;;     (setq pdf-view--next-page-timer
;;           (run-with-idle-timer 0.001 nil 'pdf-view-redisplay
;;                                (selected-window)))))

;;;;; customize to jump to the pdf-view window and display tooltip
(defun pdf-sync-forward-search (&optional line column)
  "Display the PDF location corresponding to LINE, COLUMN."
  (interactive)
  (cl-destructuring-bind (pdf page _x1 y1 _x2 _y2)
      (pdf-sync-forward-correlate line column)
    (let ((buffer (or (find-buffer-visiting pdf)
                      (find-file-noselect pdf))))
      (select-window (display-buffer buffer pdf-sync-forward-display-action))
      (other-window -1)
      (other-window 1)
      (pdf-util-assert-pdf-window)
      (pdf-view-goto-page page)
      (let ((top (* y1 (cdr (pdf-view-image-size)))))
        (pdf-util-tooltip-arrow (round top) 20))
      (with-current-buffer buffer (run-hooks 'pdf-sync-forward-hook)))))
