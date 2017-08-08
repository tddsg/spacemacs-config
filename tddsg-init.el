;;; package --- Summary

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOAD FILES

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
(require 'rtags)
(require 'engine-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PRIVATE VARIABLES

;; used to jump between faces
(defconst tddsg--face-change-types-default
  '(merlin-compilation-error-face
    merlin-compilation-warning-face
    rtags-fixitline rtags-warnline rtags-errline
    hi-yellow hi-pink hi-green hi-blue hi-black-b
    hi-blue-b hi-green-b hi-red-b hi-black-hb))
(defvar tddsg--face-change-types tddsg--face-change-types-default)

(defvar tddsg--show-header-line t)

(defvar tddsg--show-mode-line t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTILITY FUNCTIONS

(defun blank-char-p (char)
  "Check if CHAR is a SPACE or TAB character."
  (or (equal char ?\s) (equal char ?\t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PRIVATE FUNCTIONS

(defun tddsg--set-mark ()
  (push-mark (point) t nil))

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
  (let* ((backup-root "~/.emacs.d/private/backup/")
         ;; remove Windows driver letter in path, for example: “C:”
         (file-path (replace-regexp-in-string "[A-Za-z]:" "" fpath ))
         (backup-filepath (replace-regexp-in-string
                          "//" "/" (concat backup-root file-path "~"))))
    (make-directory (file-name-directory backup-filepath)
                    (file-name-directory backup-filepath))
    backup-filepath))

(defun tddsg--save-buffer ()
  "Save current buffer."
  (if (and (not buffer-read-only)
           (derived-mode-p 'text-mode 'prog-mode))
      (save-buffer)))

(defun tddsg--highlight-todos ()
  (font-lock-add-keywords nil '(("\\b\\(TODO\\|FIXME\\|BUG\\)\\b"
                                 1 (hl-todo-get-face) t)))
  (font-lock-add-keywords nil '(("\\b\\(NOTE\\|DONE\\|TRUNG\\)\\b"
                                 1 (hl-todo-get-face) t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INTERACTIVE FUNCTIONS

(defun tddsg/read-file-sexp ()
  (interactive)
  (let* ((config-file (concat (projectile-project-root) ".tddsg.el"))
         (configuration
          (with-temp-buffer
            (insert-file-contents-literally config-file)
            (read (current-buffer)))))
    (message (cdar configuration))))

(defun tddsg/describe-face-under-cursor ()
  "Describe the face information under the cursor."
  (interactive)
  (let* ((pos (point))
         (face (or (get-char-property pos 'read-face-name)
                   (get-char-property pos 'face))))
    (if face (message "Face under the cursor: %s" face)
      (message "Face not found under the cursor at: %d" pos))))

(defun tddsg/find-face-change-dwim (&optional direction)
  "Find face change dwim. DIRECTION can be 'backward or 'forward."
  (interactive)
  (defun get-face (pos)
    (or (get-char-property pos 'read-face-name)
        (get-char-property pos 'face)))
  (defun wanted-face-p (pos)
    (let ((face (get-face pos)))
      (or (member face tddsg--face-change-types)
          (and (listp face)
               (not (null (intersection face tddsg--face-change-types)))))))
  (defun find-face-change (pos)
    (let ((next-pos (cond ((eq direction 'forward) (1+ pos))
                          ((eq direction 'backward) (1- pos)))))
      (cond ((or (<= pos (point-min)) (>= pos (point-max)))
             (cond ((eq direction 'forward)
                    (message "No more next face change!"))
                   ((eq direction 'backward)
                    (message "No more previous face change!")))
             -2)
            ((eq (get-face pos) (get-face next-pos))
             (find-face-change next-pos))
            (t next-pos))))
  (let* ((pos (find-face-change (point))))
    ;; find wanted face
    (while (and (> pos (point-min))
                (< pos (point-max))
                (not (wanted-face-p pos)))
      (setq pos (find-face-change pos)))
    (when (and (>= pos 0) (wanted-face-p pos))
      ;; refine for backward finding
      (when (eq direction 'backward)
        (setq pos (1+ (find-face-change pos))))
      (goto-char pos)
      (tddsg/describe-face-under-cursor))))

(defun tddsg/next-face-change-dwim ()
  "Find next face change dwim."
  (interactive)
  (tddsg/find-face-change-dwim 'forward))

(defun tddsg/previous-face-change-dwim ()
  "Find previous face change dwim."
  (interactive)
  (tddsg/find-face-change-dwim 'backward))

(defun tddsg/traverse-upcase-dwim (&optional direction delete)
  "Traverse through character dwim. DIRECTION can be 'backward or 'forward."
  (interactive)
  (defun downcase-p (ch)
    (and (looking-at-p "[[:alpha:]]") (eq ch (downcase ch))))
  (defun upcase-p (ch)
    (and (looking-at-p "[[:alpha:]]") (eq ch (upcase ch))))
  (defun symbol-p (ch)
    (not (looking-at-p "[[:alpha:]]")))
  (defun not-downcase-p (ch) (not (downcase-p ch)))
  (defun not-upcase-p (ch) (not (upcase-p ch)))
  (defun not-symbol-p (ch) (not (symbol-p ch)))
  (defun traverse ()
    (cond ((eq direction 'forward) (forward-char))
          ((eq direction 'backward) (backward-char))))
  (defun traverse-until (checker)
    (while (and (> (point) (point-min))
                (< (point) (point-max))
                (not (funcall checker (char-after))))
      (traverse)))
  (let ((origin-pos (point)))
    (cond ((downcase-p (char-after))
           (traverse-until 'upcase-p))
          ((symbol-p (char-after))
           (traverse-until 'upcase-p))
          ((upcase-p (char-after))
           (traverse-until 'not-upcase-p)
           (traverse-until 'not-downcase-p)))
    (if delete (delete-char (- origin-pos (point))))))

(defun tddsg/next-upcase-dwim ()
  "Go to next contradict char."
  (interactive)
  (tddsg/traverse-upcase-dwim 'forward))

(defun tddsg/previous-upcase-dwim ()
  "Go to previous contradict char."
  (interactive)
  (tddsg/traverse-upcase-dwim 'backward))

(defun tddsg/delete-until-next-upcase-dwim ()
  "Delete until next contradict char."
  (interactive)
  (tddsg/traverse-upcase-dwim 'forward t))

(defun tddsg/delete-until-previous-upcase-dwim ()
  "Delete until previous contradict char."
  (interactive)
  (tddsg/traverse-upcase-dwim 'backward t))

(defun tddsg/shell-current-window (&optional buffer)
  "Open a `shell' in the current window."
  (interactive)
  (let ((window (selected-window))
        (window-config (current-window-configuration))
        (shell-buffer (call-interactively 'shell)))
    (set-window-configuration window-config)
    (select-window window)
    (switch-to-buffer shell-buffer)))

(defun tddsg/shell-other-window (&optional buffer)
  "Open a `shell' in a new window."
  (interactive)
  (when (equal (length (window-list)) 1)
    (call-interactively 'split-window-right))
  (call-interactively 'other-window)
  (call-interactively 'tddsg/shell-current-window))

(defun tddsg/next-shell-window ()
  "Jump to the next shell window, if available."
  (interactive)
  (defun next-shell-window (orgwin curwin)
    (let ((nextwin (next-window curwin nil t)))
      (if (equal nextwin orgwin) nil
        (with-current-buffer (window-buffer nextwin)
          (if (derived-mode-p 'shell-mode) (select-window nextwin)
            (next-shell-window orgwin nextwin))))))
  (let* ((curwin (selected-window))
         (shellwin (next-shell-window curwin curwin)))
    (if (not shellwin)
        (message "Next shell window not found!")
      (select-frame-set-input-focus (window-frame shellwin))
      (select-window shellwin))))

(defun tddsg/previous-window ()
  "Jump to the previous window."
  (interactive)
  (other-window -1 t))

;; TODO: create a full request to Spacemacs
(defun tddsg/save-file-as-and-open (filename)
  "Save current buffer into file FILENAME and open it in a new buffer."
  (interactive
   (list (if buffer-file-name
             (read-file-name "Save file as and open: " buffer-file-name)
           (read-file-name "Save file as and open: " default-directory))))
  (or (null filename) (string-equal filename "")
      (progn
        (let ((dir (file-name-directory filename)))
          (when (and (not (file-exists-p dir))
                     (yes-or-no-p (format "Create directory '%s'?" dir)))
            (make-directory dir t)))
        (and (file-exists-p filename)
             (or (y-or-n-p (format "File `%s' exists; overwrite? " filename))
                 (error "Canceled")))
        (write-region (point-min) (point-max) filename )
        (find-file filename))))

(defun tddsg/dired-home ()
  (interactive)
  (dired "~/"))

(defun tddsg/toggle-case-current-character ()
  "Toggle case of the current character."
  (interactive)
  (let ((current-char (char-after)))
    (cond ((eq current-char (upcase current-char))
           (delete-char 1)
           (insert (downcase current-char)))
          ((eq current-char (downcase current-char))
           (delete-char 1)
           (insert (upcase current-char))))))

(defun tddsg/dired-duplicate-files ()
  "Duplicate files to the current folder by adding suffix \" - COPY\"."
  (interactive)
  ;; TODO: how to deal with file names having no \".\". For example: TODO files
  (dired-do-copy-regexp "\\(.*\\)\\.\\(.*\\)" "\\1 - (COPY).\\2"))

(defun tddsg/duplicate-region-or-line ()
  "Duplicate a selected region or a line."
  (interactive)
  (let ((duplicate-line (not (region-active-p))))
    (when duplicate-line (tddsg/mark-line))
    (let ((size (- (region-end) (region-beginning)))
          (new-region (+ (region-end) 1)))
      (call-interactively 'kill-ring-save)
      (when duplicate-line (newline-and-indent))
      (yank)
      (indent-region new-region (+ new-region size)))))

(defun tddsg/mark-line ()
  "Select current line."
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(defun tddsg/mark-sexp (&optional backward)
  "Mark sexp using the smartparens package."
  (let ((step (if (null backward) 1 -1)))
    (if (region-active-p)
        (sp-forward-sexp step)
      (cond ((and (null backward)
                  (not (null (char-before)))
                  (memq (char-syntax (char-before)) '(?w ?_)))
             (backward-sexp))
            ((and (not (null backward))
                  (not (null (char-after)))
                  (memq (char-syntax (char-after)) '(?w ?_)))
             (forward-sexp)))
      (set-mark-command nil)
      (sp-forward-sexp step))))

(defun tddsg/mark-sexp-forward ()
  "Mark sexp forward, using the smartparens package."
  (interactive)
  (tddsg/mark-sexp))

(defun tddsg/mark-sexp-backward ()
  "Mark sexp backward, using the smartparens package."
  (interactive)
  (tddsg/mark-sexp t))

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

(defun tddsg/mark-environment ()
  "Mark the environment, depending on the major mode."
  (interactive)
  (cond ((derived-mode-p 'LaTeX-mode 'TeX-mode 'latex-mode 'tex-mode)
         (call-interactively 'LaTeX-mark-environment))
        (t
         (deactivate-mark t)
         (sp-backward-up-sexp)
         (sp-mark-sexp))))

(defun tddsg/comment-paragraph ()
  "Comment the paragraph."
  (interactive)
  (tddsg/mark-paragraph)
  (call-interactively 'comment-dwim-2))

(defun tddsg/vc-status-dwim ()
  "Show version control status (git, hg) of the project containing the current file."
  (interactive)
  (defun find-vc-tool (dir)
    (cond ((string-match-p (regexp-quote "..") dir) nil)
          ((file-exists-p (expand-file-name ".git/config" dir)) 'Git)
          ((file-exists-p (expand-file-name ".hg/hgrc" dir)) 'Hg)
          (t (find-vc-tool (expand-file-name ".." dir)))))
  (let* ((path (if buffer-file-name buffer-file-name default-directory))
         (vc-tool (find-vc-tool path)))
    (cond ((eq vc-tool 'Hg)
           (call-interactively 'monky-status))
          ((eq vc-tool 'Git)
           (call-interactively 'magit-status))
          (t (message "Error: unknown version control tool")))))

(defun tddsg/find-definition-dwim (&optional prefix)
  "Goto definition of a function or a variable."
  (interactive "P")
  (cond ((derived-mode-p 'cc-mode 'c-mode 'c++-mode)
         (if (and (not (rtags-find-symbol-at-point prefix))
                  rtags-last-request-not-indexed)
             (gtags-find-tag)))))

(defun tddsg/find-references-dwim (&optional prefix)
  "Goto definition of a function or a variable."
  (interactive "P")
  (cond ((derived-mode-p 'cc-mode 'c-mode 'c++-mode)
         (if (and (not (rtags-find-references-at-point prefix))
                  rtags-last-request-not-indexed)
             (gtags-find-rtag)))))

(defun tddsg/smart-kill-sexp (&optional backward)
  "Kill sexp smartly."
  (interactive)
  (defun kill-region-and-next-spaces (begin end &optional backward)
    (if backward
        (cl-loop
         while (and (blank-char-p (char-before begin))
                    (or (null (char-before (1+ begin)))
                        (null (char-after end))
                        (not (memq (char-syntax (char-before (1+ begin))) '(?w ?_)))
                        (not (memq (char-syntax (char-after end)) '(?w ?_)))))
         do (setq begin (1- begin)))
      (cl-loop
       while (and (blank-char-p (char-after end))
                  (or (null (char-before begin))
                      (null (char-after (1+ end)))
                      (not (memq (char-syntax (char-before begin)) '(?w ?_)))
                      (not (memq (char-syntax (char-after (1+ end))) '(?w ?_)))))
       do (setq end (1+ end))))
    (kill-region begin end)
    (when (and (not (null (char-before)))
               (memq (char-syntax (char-after)) '(?w ?_))
               (memq (char-syntax (char-before)) '(?w ?_ ?.)))
      (just-one-space)))
  (if (region-active-p) (delete-active-region t)
    (cond ((and backward
                (not (blank-char-p (char-after)))
                (not (null (char-before)))
                (blank-char-p (char-before)))
           (forward-char -1))
          ((and (not backward)
                (blank-char-p (char-after))
                (not (null (char-before)))
                (not (blank-char-p (char-before))))
           (forward-char 1)))
    (setq begin (if backward (1- (point)) (point)))
    (setq end (if backward (point) (1+ (point))))
    (setq thing (sp-get-thing backward))
    (when (and (not (null thing))
               (>= (point) (cadr thing))
               (<= (point) (cadddr thing)))
      (setq begin (cadr thing))
      (setq end (cadddr thing)))
    (kill-region-and-next-spaces begin end backward)
    (when (and (not (null (char-before)))
               (memq (char-syntax (char-after)) '(?w ?_))
               (memq (char-syntax (char-before)) '(?w ?_)))
      (just-one-space))))

(defun tddsg/smart-kill-sexp-forward ()
  "Kill sexp forward."
  (interactive)
  (tddsg/smart-kill-sexp))

(defun tddsg/smart-kill-sexp-backward ()
  "Kill sexp backward."
  (interactive)
  (tddsg/smart-kill-sexp t))

(defun tddsg/helm-do-ag (arg)
  "Search by Helm-Ag in the current directory, \
or in a custom directory when prefix-argument is given <C-u>."
  (interactive "P")
  (if (null arg)
      (let* ((text (if (region-active-p)
                       (buffer-substring (region-beginning) (region-end))
                     (thing-at-point 'word)))
             (text (if (null text) "" text))
             (text (replace-regexp-in-string " " "\\\\ " (string-trim text))))
        (helm-do-ag (expand-file-name default-directory) text))
    (call-interactively 'helm-do-ag)))

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
    (if (blank-char-p (preceding-char)) (forward-char -1))
    (if (blank-char-p (following-char))
        (if (or (blank-char-p (preceding-char))
                (blank-char-p (char-after (+ (point) 1))))
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
    (if (string-blank-p (thing-at-point 'line t))
        (delete-blank-lines)
      (message "Insert space")
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
               (text (replace-regexp-in-string "\t" " " text))
               (text (replace-regexp-in-string " +\n" "\n" text))
               ;; FIXME this is a very temporarily fix to maintain paragraphs
               (text (replace-regexp-in-string "\n\n" "<<<NN>>>" text))
               (text (replace-regexp-in-string "\n" " " text))
               (text (replace-regexp-in-string "\\s-+" " " text))
               (text (replace-regexp-in-string "<<<NN>>>" "\n\n" text))
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

(defun tddsg/pdf-view-enable-auto-slice ()
  (interactive)
  (pdf-view-auto-slice-minor-mode 1))

(defun tddsg/pdf-view-disable-auto-slice ()
  (interactive)
  (pdf-view-auto-slice-minor-mode -1)
  (pdf-view-reset-slice))

(defun tddsg/kill-active-region ()
  (interactive)
  (when (region-active-p) (call-interactively 'kill-region)))

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

(defun tddsg/beginning-of-line-dwim ()
  "Goto beginning of line."
  (interactive)
  (let* ((pos-current (point))
         (pos-begin-line (save-excursion (beginning-of-line) (point)))
         (pos-first-nonblank (save-excursion (beginning-of-line)
                                             (skip-syntax-forward "-")
                                             (point))))
    (cond ((eq pos-current pos-begin-line)
           (goto-char pos-first-nonblank))
          ((eq pos-current pos-first-nonblank)
           (goto-char pos-begin-line))
          (t (goto-char pos-first-nonblank)))))

(defun tddsg/unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))

(defun tddsg/compile ()
  "Find the closest Makefile and compile."
  (interactive)
  ;; (when (get-buffer "*compilation*") (kill-buffer "*compilation*"))
  (defun find-make-dir (dir)
    (cond ((string-match-p (regexp-quote "..") dir) "./")
          ((file-exists-p (expand-file-name "Makefile" dir)) dir)
          ((file-exists-p (expand-file-name "build/Makefile" dir))
           (expand-file-name "build" dir))
          (t (find-make-dir (expand-file-name ".." dir)))) )
  (when (string-match-p (regexp-quote "make") compile-command)
    (setq compile-command
          (format "make -k -C %s -j4" (find-make-dir default-directory))))
  (call-interactively 'compile))

(defun tddsg/recompile ()
  "Find the closest Makefile and compile."
  (interactive)
  ;; (when (get-buffer "*compilation*") (kill-buffer "*compilation*"))
  (if (string-match-p (regexp-quote "make -k -C") compile-command)
      (recompile)
    (call-interactively 'tddsg/compile)))

(defun tddsg/latex-compile ()
  "Run LaTex command from TeX Master commands."
  (interactive)
  (save-buffer)
  (TeX-command "LaTeX" 'TeX-master-file -1))

(defun tddsg/latex-beamer-compile-current-frame ()
  "Run pdflatex on current beamer frame."
  (interactive)
  (save-buffer)
  (setq TeX-region "__current_frame")
  (let (beg)
    (save-excursion
      (search-backward "\\begin{frame}")
      (setq beg (point))
      (forward-char 1)
      (LaTeX-find-matching-end)
      (TeX-pin-region beg (point))
      (letf (( (symbol-function 'TeX-command-query) (lambda (x) "LaTeX")))
        (TeX-command-region)))))

(defun tddsg/latex-compile-sync-forward ()
  "Compile LaTex and synchronize the output."
  (interactive)
  (call-interactively 'latex/compile-commands-until-done)
  (call-interactively 'pdf-sync-forward-search))

(defun tddsg/latex-compile-sync-other-window ()
  "Compile LaTex and synchronize the output, and jumpback."
  (interactive)
  (call-interactively 'latex/compile-commands-until-done)
  (call-interactively 'pdf-sync-forward-search)
  (call-interactively 'other-window -1))

(defun tddsg/close-special-windows ()
  "Close all special windows such as compilation, ..."
  (interactive)
  (dolist (buf-name '("*compilation*")) ;; list more windows here
    (let ((window (get-buffer-window buf-name)))
      (when window (delete-window window)))))

(defun tddsg/scroll-window-dwim (&optional direction other)
  "Scroll half window, DIRECTION can be 'upward or 'downward.
If OTHER is t then scroll other window."
  (interactive)
  (defun scroll-half ()
    (defun scroll-height ()
      (let ((height (window-height (selected-window))))
        (if (> height 20) (- height 5) height)))
    (if (derived-mode-p 'pdf-view-mode)
        (cond ((eq direction 'upward)
               (pdf-view-scroll-down-or-previous-page))
              ((eq direction 'downward)
               (pdf-view-scroll-up-or-next-page)))
      (cond ((eq direction 'upward)
             (scroll-down (scroll-height)))
            ((eq direction 'downward)
             (scroll-up (scroll-height))))))
  (if (and other (> (length (window-list)) 1)) (other-window 1))
  (scroll-half)
  (if (and other (> (length (window-list)) 1)) (other-window -1)))

(defun tddsg/scroll-window-upward-dwim ()
  "Scroll half window upward."
  (interactive)
  (tddsg/scroll-window-dwim 'upward nil))

(defun tddsg/scroll-window-downward-dwim ()
  "Scroll half window downward."
  (interactive)
  (tddsg/scroll-window-dwim 'downward nil))

(defun tddsg/scroll-other-window-upward-dwim ()
  "Scroll half window upward."
  (interactive)
  (tddsg/scroll-window-dwim 'upward t))

(defun tddsg/scroll-other-window-downward-dwim ()
  "Scroll half window downward."
  (interactive)
  (tddsg/scroll-window-dwim 'downward t))

(defun tddsg/enable-company-auto-suggest ()
  (interactive)
  (setq company-idle-delay 0.5))

(defun tddsg/disable-company-auto-suggest ()
  (interactive)
  (setq company-idle-delay 300))

(defun tddsg/restart-irony-mode ()
  (interactive)
  (irony-server-kill)
  (irony--mode-exit)
  (irony--mode-enter))

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

(defun tddsg/recent-dirs ()
  "Open recent dirs."
  (interactive)
  (defun parent-dirs (path)
    (let* ((path (directory-file-name path))
           (parent (directory-file-name (file-name-directory path))))
      (cond ((string= path parent) '(path))
            ((file-directory-p path) (cons path (parent-dirs parent)))
            ((file-exists-p path) (parent-dirs parent)))))
  (let* ((recentf-dirs (delete-dups
                        (apply #'append (mapcar 'parent-dirs recentf-list))))
         (dired-dirs (mapcar (lambda (item) (directory-file-name (car item)))
                             dired-buffers))
         (all-dirs (delete-dups (append recentf-dirs dired-dirs)))
         (all-dirs (sort all-dirs 'string<))
         (selected-dir (completing-read "Dired buffer name: " all-dirs)))
    (message selected-dir)
    (dired selected-dir)))

(defun tddsg/yank-recent-dir-files ()
  "Yank recent dir and files."
  (interactive)
  (defun parent-dirs (path)
    (let* ((path (directory-file-name path))
           (parent (directory-file-name (file-name-directory path))))
      (cond ((string= path parent) '(path))
            ((file-directory-p path) (cons path (parent-dirs parent)))
            ((file-exists-p path) (parent-dirs parent)))))
  (let* ((recent-files recentf-list)
         (recent-dirs (delete-dups
                       (apply #'append (mapcar 'parent-dirs recent-files))))
         (dired-dirs (mapcar (lambda (item) (directory-file-name (car item)))
                             dired-buffers))
         (all-dir-files (delete-dups (append recent-files recent-dirs dired-dirs)))
         (all-dirs (sort all-dir-files 'string<))
         (selected-dir-file (completing-read "Yank dir/file:" all-dir-files)))
    (insert selected-dir-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INIT CONFIGS

(defun tddsg/config-packages ()
  ;; visual interface setting
  (display-time)                    ;; show time in mode line
  (global-hl-todo-mode 1)           ;; highlight todo mode
  (blink-cursor-mode 0)             ;; turn off blinking
  (setq blink-cursor-blinks 15)     ;; blink 15 times
  (setq fill-column 75)             ;; max size of a line for fill-or-unfill
  (setq fast-but-imprecise-scrolling nil)
  (setq text-scale-mode-step 1.1)   ;; scale changing font size
  (setq frame-title-format          ;; frame title
        '("" invocation-name " - "
          (:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name)) "%b"))))

  ;; windows setting
  (setq window-combination-resize nil)   ;; stop automatically resize windows

  ;; scrolling
  (spacemacs/toggle-smooth-scrolling-off)  ;; disable smooth-scrolling
  (setq redisplay-dont-pause t
        scroll-conservatively 101
        scroll-margin 0                    ;; perfect setting for scrolling
        next-screen-context-lines 0        ;; perfect setting for scrolling
        scroll-preserve-screen-position 't)

  ;; mode paragraph setting
  (setq paragraph-separate "[ \t\f]*$"
        paragraph-start "\f\\|[ \t]*$")

  ;; save
  ;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace)

  ;; zoom frame
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

  ;; mark ring
  (setq set-mark-command-repeat-pop t)
  (defadvice find-file (before set-mark activate) (tddsg--set-mark))
  (defadvice isearch-update (before set-mark activate) (tddsg--set-mark))
  (defadvice beginning-of-buffer (before set-mark activate) (tddsg--set-mark))
  (defadvice end-of-buffer (before set-mark activate) (tddsg--set-mark))
  (defadvice merlin-locate (before set-mark activate) (tddsg--set-mark))

  ;; company-mode
  (setq company-idle-delay 300)
  (setq company-tooltip-idle-delay 300)
  (global-company-mode)

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
  (electric-pair-mode -1)         ;; electric-pair may conflict with smartparens
  (delete-selection-mode t)       ;; delete selection by keypress
  (setq require-final-newline t)                       ;; newline at end of file
  (defadvice newline (after indent activate) (indent-according-to-mode))
  (ad-unadvise 'kill-region)


  ;; some Emacs threshold
  (setq max-lisp-eval-depth 50000)
  (setq max-specpdl-size 50000)

  ;; mode-line setting
  (setq powerline-default-separator 'wave)

  ;; themes
  (defun hook-update-cursor ()
    (cond ((or (bound-and-true-p god-mode)
               (bound-and-true-p god-global-mode))
           (set-cursor-color "lime green"))
          ((eq spacemacs--cur-theme 'leuven)
           (set-cursor-color "dark orange"))
          ((eq spacemacs--cur-theme 'spacemacs-dark)
           (set-cursor-color "dark orange"))))
  (add-hook 'buffer-list-update-hook 'hook-update-cursor)

  ;; isearch
  (defun tddsg--isearch-show-case-fold (orig-func &rest args)
    (apply orig-func args)
    (if isearch-case-fold-search
        (spacemacs|diminish isearch-mode "⚡ISearch[ci]⚡")
      (spacemacs|diminish isearch-mode "⚡ISearch[CS]⚡")))
  (advice-add 'isearch-mode :around #'tddsg--isearch-show-case-fold)
  (advice-add 'isearch-repeat :around #'tddsg--isearch-show-case-fold)
  (advice-add 'isearch-toggle-case-fold :around #'tddsg--isearch-show-case-fold)
  ;; (add-hook 'isearch-update-post-hook 'update-pdf-view-theme)

  ;; compilation
  (setq compilation-ask-about-save nil
        compilation-window-height 16
        compilation-scroll-output t
        compilation-skip-threshold 2)
  ;; pin the compilation buffer into 1 frame
  ;; (push '("\\*compilation\\*" . (nil (reusable-frames . t))) display-buffer-alist)
  ;; reset all compilation hook, use the default one
  (setq compilation-mode-hook nil)

  ;; shell
  (setq comint-prompt-read-only nil
        shell-default-shell 'ansi-term
        shell-dynamic-complete-functions '(bash-completion-dynamic-complete
                                           comint-c-a-p-replace-by-expanded-history
                                           shell-environment-variable-completion
                                           shell-command-completion
                                           shell-c-a-p-replace-by-expanded-directory
                                           pcomplete-completions-at-point
                                           shell-filename-completion
                                           comint-filename-completion))
  (defun hook-shell-mode ()
    "Hook to run in shell mode."
    (add-hook 'window-configuration-change-hook
              'tddsg--fix-comint-window-size nil t)
    (set (make-local-variable 'scroll-margin) 1)
    (set (make-local-variable 'next-screen-context-lines ) 1)
    (rainbow-delimiters-mode-disable)
    (toggle-truncate-lines -1)
    (visual-line-mode 1))
  (add-hook 'shell-mode-hook 'hook-shell-mode)

  ;; automatically save buffer
  (defadvice magit-status (before save-buffer activate) (tddsg--save-buffer))
  (defadvice winum-select-window-by-number
      (before save-buffer activate) (tddsg--save-buffer))

  ;; which-key
  (setq which-key-idle-delay 1.2)

  ;; smartparens
  (smartparens-global-mode)

  ;; auto-revert
  (setq auto-revert-check-vc-info nil)

  ;; backup
  (setq make-backup-files t
        make-backup-file-name-function 'tddsg--create-backup-file-name)

  ;; evil mode
  (evil-mode -1)
  (setq-default evil-cross-lines t)

  ;; engine
  (defengine thefreedictionary
    "http://www.thefreedictionary.com/%s"
    :keybinding "d")

  ;; hippie expand
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol))

  ;; dired
  (add-to-list 'savehist-additional-variables 'helm-dired-history-variable)
  (setq dired-guess-shell-alist-user
        '(("\\.pdf\\'" "okular &")
          ("\\.html\\|\\.xml*" "google-chrome &")
          ("\\.txt\\'" "gedit &")
          ("\\.jpg*\\|\\.png*\\|\\.gif*\\|\\.svg*" "eog &")
          ("\\.doc*\\|\\.xls*\\|\\.ppt*" "libreoffice &")
          ("\\.ods*\\|\\.odt*" "libreoffice &")))

  ;; helm setting
  (setq helm-ag-insert-at-point 'symbol)     ;; insert symbol in helm-ag
  (setq helm-split-window-in-side-p t)
  (setq helm-split-window-default-side 'below)
  ;; make Helm split inside the active window in a few function
  (defun advise-helm-split-active-window (orig-func &rest args)
    (setq helm-display-function 'helm-default-display-buffer)
    (apply orig-func args)
    (setq helm-display-function 'spacemacs//display-helm-window))
  (advice-add 'helm-company :around #'advise-helm-split-active-window)
  (advice-add 'helm-semantic-or-imenu :around #'advise-helm-split-active-window)
  (advice-add 'helm-imenu :around #'advise-helm-split-active-window)
  (advice-add 'completion-at-point :around #'advise-helm-split-active-window)

  ;; browser
  (setq browse-url-browser-function 'browse-url-generic
        engine/browser-function 'browse-url-generic
        browse-url-generic-program (if (string= (system-name) "pisces")
                                       "chromium-browser" "google-chrome"))

  ;; minibuffer
  (setq resize-mini-windows t)
  (setq max-mini-window-height 30)

  ;; reason-mode
  ;; (tddsg/config-reason-mode)

  ;; ggtags
  (setq ggtags-process-environment '("GTAGSLIBPATH=/home/trungtq/.gtags"))

  ;; spacemacs
  (push "\\*magit\.\+" spacemacs-useful-buffers-regexp)
  (push "\\*monky\.\+\\*" spacemacs-useful-buffers-regexp)
  (push "\\*compilation\\*" spacemacs-useful-buffers-regexp)
  (setq-default dotspacemacs-excluded-packages '(window-purpose))

  ;; windmove
  (defun advise-windmove (orig-func &rest args)
    (if (derived-mode-p 'pdf-view-mode)
        ;; Fix bug of windmove for pdf-view-mode by
        ;; temporarily switch to the "*Messages*" buffer
        (let* ((cur-win (selected-window))
               (cur-buf (window-buffer cur-win)))
          (switch-to-buffer "*Messages*")
          (ignore-errors (apply orig-func args))
          (set-window-buffer cur-win cur-buf))
      (apply orig-func args))
    ;; reselect the current window to update its states
    (select-window (selected-window)))
  (advice-add 'windmove-left :around #'advise-windmove)
  (advice-add 'windmove-right :around #'advise-windmove)
  (advice-add 'windmove-up :around #'advise-windmove)
  (advice-add 'windmove-down :around #'advise-windmove)


  ;; whichkey
  (which-key-add-key-based-replacements "C-c !" "flycheck")
  (which-key-add-key-based-replacements "C-c ," "semantic")
  (which-key-add-key-based-replacements "C-c @" "hideshow")
  (which-key-add-key-based-replacements "C-c C-w" "eyebrowse")
  (which-key-add-key-based-replacements "C-x w" "highlight")
  (which-key-add-key-based-replacements "M-s h" "highlight")

  ;; diminish
  (spacemacs|diminish whitespace-mode "")
  (spacemacs|diminish shx-mode "")
  (spacemacs|diminish super-save-mode "")
  (spacemacs|diminish company-mode "")
  (spacemacs|diminish which-key-mode "")
  (spacemacs|diminish yas-minor-mode "")
  (spacemacs|diminish latex-extra-mode "")
  (spacemacs|diminish utop-minor-mode "")
  (spacemacs|diminish ggtags-mode "")
  (spacemacs|diminish irony-mode "")
  (spacemacs|diminish golden-ratio-mode "")
  (spacemacs|diminish with-editor-mode "")
  (spacemacs|diminish compilation-in-progress "")
  (spacemacs|diminish server-buffer-clients "")
  (spacemacs|diminish reftex-mode "")
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
  (defun hook-text-mode ()
    "Hook to run in 'text-mode'."
    (setq tddsg--face-change-types tddsg--face-change-types-default)
    (dolist (face '(flyspell-incorrect flyspell-duplicate langtool-errline))
      (add-to-list 'tddsg--face-change-types face))
    (tddsg--highlight-todos)
    (smartparens-global-mode 1)
    (column-marker-3 80)
    (whitespace-mode 1)
    (flyspell-mode 1))
  (defun hook-prog-mode ()
    "Hook to run in 'prog-mode'."
    (setq tddsg--face-change-types tddsg--face-change-types-default)
    (when (derived-mode-p 'c-mode 'c++-mode)
      (ggtags-mode 1))
    (smartparens-global-mode 1)
    (column-marker-3 80)
    (whitespace-mode 1)
    (flyspell-mode -1)
    (linum-mode 1)
    (flycheck-mode 1))
  (add-hook 'LaTeX-mode-hook 'hook-prog-mode)
  (add-hook 'TeX-mode-hook 'hook-prog-mode)
  (add-hook 'tex-mode-hook 'hook-prog-mode)
  (add-hook 'prog-mode-hook 'hook-prog-mode)
  (add-hook 'text-mode-hook 'hook-text-mode)

  (defun hook-change-major-mode ()
    ;; change some weird keys
    (keyboard-translate ?\C-\[ ?\H-\[)
    (keyboard-translate ?\C-i ?\H-i)
    (keyboard-translate ?\C-m ?\H-m)
    (define-key input-decode-map (kbd "C-M-m") (kbd "H-M-m"))
    (define-key input-decode-map (kbd "C-M-[") (kbd "H-M-["))
    (define-key input-decode-map (kbd "C-S-I") (kbd "H-I"))
    (define-key input-decode-map (kbd "C-S-M") (kbd "H-M"))
    ;; spaceline size
    (setq-default powerline-height 22)  ;; spaceline height
    (setq-default powerline-scale 1)
    (cond ((string= (system-name) "lmint")
           (set-face-attribute 'mode-line nil :font "DejaVu Sans Mono-12"))
          (t (set-face-attribute 'mode-line nil :font "DejaVu Sans Mono-10")))
    ;; change personal dictionary of ispell
    (if (tddsg--projectile-p)
        (setq ispell-personal-dictionary (concat (projectile-project-root)
                                                 "user.dict"))))
  (add-hook 'change-major-mode-hook 'hook-change-major-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INIT KEYS

(defun tddsg/config-keys ()
  ;; unbind some weird keys
  (global-set-key (kbd "<home>") 'tddsg/beginning-of-line-dwim)
  (global-set-key (kbd "<escape>") 'god-mode-all)
  (global-set-key (kbd "C-z") 'god-mode-all)
  (global-set-key (kbd "<f5>") 'tddsg/recompile)

  (global-set-key (kbd "C-<backspace>") 'backward-kill-word)
  (global-set-key (kbd "C-<delete>") 'kill-word)
  (global-set-key (kbd "C-<left>") 'left-word)
  (global-set-key (kbd "C-<right>") 'right-word)
  (global-set-key (kbd "C-+") 'zoom-in)
  (global-set-key (kbd "C--") 'zoom-out)
  (global-set-key (kbd "C-`") 'goto-last-change)
  (global-set-key (kbd "C-'") 'other-window)
  (global-set-key (kbd "C-j") 'avy-goto-word-1)
  (global-set-key (kbd "C-S-j") 'avy-goto-char)
  (global-set-key (kbd "C-o") 'helm-semantic-or-imenu)
  (global-set-key (kbd "C-a") 'tddsg/beginning-of-line-dwim)
  (global-set-key (kbd "C-w") 'tddsg/kill-active-region)
  (global-set-key (kbd "C-q") 'goto-last-change)
  (global-set-key (kbd "C-z") 'save-buffer)
  (global-set-key (kbd "C-/") 'undo)
  (global-set-key (kbd "C-;") 'iedit-mode)
  (global-set-key (kbd "C-^") 'tddsg/join-with-beneath-line)
  (global-set-key (kbd "C-_") 'tddsg/join-to-above-line)
  (global-set-key (kbd "C-\\") 'sp-split-sexp)

  (global-set-key (kbd "C-S-<backspace>") 'kill-whole-line)
  (global-set-key (kbd "C-S-q") 'tddsg/previous-window)
  (global-set-key (kbd "C-S-k") 'kill-whole-line)
  (global-set-key (kbd "C-S-/") 'undo-tree-redo)

  (global-set-key (kbd "C-M-o") 'helm-imenu-anywhere)
  (global-set-key (kbd "C-M-h") 'tddsg/mark-environment)
  (global-set-key (kbd "C-M-k") 'tddsg/smart-kill-sexp-forward)
  (global-set-key (kbd "C-M-S-k") 'tddsg/smart-kill-sexp-backward)
  (global-set-key (kbd "C-M-S-p") 'tddsg/scroll-other-window-upward-dwim)
  (global-set-key (kbd "C-M-S-n") 'tddsg/scroll-other-window-downward-dwim)
  (global-set-key (kbd "C-M-S-v") 'tddsg/scroll-other-window-upward-dwim)
  (global-set-key (kbd "C-M-v") 'tddsg/scroll-other-window-downward-dwim)
  (global-set-key (kbd "C-M-j") 'tddsg/join-with-beneath-line)
  (global-set-key (kbd "C-M-i") 'tddsg/join-to-above-line)
  (global-set-key (kbd "C-M-SPC") 'tddsg/mark-sexp-forward)
  (global-set-key (kbd "C-M-S-SPC") 'tddsg/mark-sexp-backward)
  (global-set-key (kbd "C-M-;") 'tddsg/comment-paragraph)

  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x t") 'transpose-paragraphs)
  (global-set-key (kbd "C-x g") 'tddsg/vc-status-dwim)
  (global-set-key (kbd "C-x {") 'shrink-window-horizontally)
  (global-set-key (kbd "C-x }") 'enlarge-window-horizontally)
  (global-set-key (kbd "C-x _") 'shrink-window)
  (global-set-key (kbd "C-x ^") 'enlarge-window)
  (global-set-key (kbd "C-x w s") 'tddsg/save-file-as-and-open)

  (global-set-key (kbd "C-x C-d") 'tddsg/recent-dirs)
  (global-set-key (kbd "C-x C-y") 'tddsg/yank-recent-dir-files)
  (global-set-key (kbd "C-x C-b") 'switch-to-buffer)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-z") nil)

  (global-set-key [?\H-m] 'helm-mini)
  (global-set-key [?\H-M] 'tddsg/recent-dirs)
  (global-set-key (kbd "H-M-m") 'projectile-find-file)
  (global-set-key [?\H-i] 'swiper)
  (global-set-key [?\H-I] 'swiper)

  (global-set-key (kbd "C-c f") 'projectile-find-file)
  (global-set-key (kbd "C-c r") 'projectile-replace)
  (global-set-key (kbd "C-c R") 'projectile-replace-regexp)
  (global-set-key (kbd "C-c k") 'kill-this-buffer)
  (global-set-key (kbd "C-c o") 'helm-occur)
  (global-set-key (kbd "C-c l") 'helm-locate)
  (global-set-key (kbd "C-c e") 'ediff)
  (global-set-key (kbd "C-c i") 'ivy-resume)
  (global-set-key (kbd "C-c j") 'avy-resume)
  (global-set-key (kbd "C-c h") 'helm-resume)
  (global-set-key (kbd "C-c s") 'sr-speedbar-toggle)
  (global-set-key (kbd "C-c g") 'tddsg/helm-do-ag)
  (global-set-key (kbd "C-c d") 'tddsg/duplicate-region-or-line)
  (global-set-key (kbd "C-c m") 'tddsg/shell-current-window)
  (global-set-key (kbd "C-c v") 'tddsg/describe-face-under-cursor)

  ;; (global-set-key (kbd "C-c C-c") nil)
  (global-set-key (kbd "C-c C-c") 'tddsg/compile)
  (global-set-key (kbd "C-c H-m") 'tddsg/next-shell-window)
  (global-set-key (kbd "C-c C-g") 'helm-projectile-ag)
  (global-set-key (kbd "C-c C-k") 'kill-matching-buffers)
  (global-set-key (kbd "C-c C-SPC") 'helm-all-mark-rings)

  (global-set-key (kbd "M-SPC") 'tddsg/one-space-or-blank-line)
  (global-set-key (kbd "M-<backspace>") 'backward-kill-word)
  (global-set-key (kbd "M-<delete>") 'kill-word)
  (global-set-key (kbd "M-w") 'tddsg/kill-ring-save)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "M-p") 'tddsg/scroll-window-upward-dwim)
  (global-set-key (kbd "M-n") 'tddsg/scroll-window-downward-dwim)
  (global-set-key (kbd "M-P") 'tddsg/previous-face-change-dwim)
  (global-set-key (kbd "M-N") 'tddsg/next-face-change-dwim)
  (global-set-key (kbd "M-B") 'tddsg/previous-upcase-dwim)
  (global-set-key (kbd "M-F") 'tddsg/next-upcase-dwim)
  (global-set-key (kbd "M-D") 'tddsg/delete-until-next-upcase-dwim)
  (global-set-key (kbd "M-C") 'tddsg/toggle-case-current-character)
  (global-set-key (kbd "M-K") 'backward-delete-char-untabify)
  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "M-'") 'other-window)
  (global-set-key (kbd "M--") 'delete-window)
  (global-set-key (kbd "M-+") 'delete-other-windows)
  (global-set-key (kbd "M--") 'delete-window)
  (global-set-key (kbd "M-_") 'split-window-below)
  (global-set-key (kbd "M-|") 'split-window-right)
  (global-set-key (kbd "M-=") 'transpose-frame)
  (global-set-key (kbd "M-\\") 'sp-splice-sexp)
  (global-set-key (kbd "M-;") 'comment-dwim-2)
  (global-set-key (kbd "M-?") 'company-complete)
  (global-set-key (kbd "C-M-/") 'helm-company)
  (global-set-key (kbd "M-H") 'tddsg/mark-line)
  (global-set-key (kbd "M-h") 'tddsg/mark-paragraph)
  (global-set-key (kbd "M-g |") 'vline-mode)

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
  (global-set-key (kbd "M-S-<backspace>") 'tddsg/delete-until-previous-upcase-dwim)

  (define-key spacemacs-default-map-root-map (kbd "M-m l") nil)
  (global-set-key (kbd "M-m h g") 'helm-do-grep-ag)
  (global-set-key (kbd "M-m l v") 'visual-line-mode)
  (global-set-key (kbd "M-m w t") 'transpose-frame)
  (global-set-key (kbd "M-m w o") 'flop-frame)
  (global-set-key (kbd "M-m w i") 'flip-frame)
  (global-set-key (kbd "M-m r t") 'purpose-toggle-window-buffer-dedicated)
  (global-set-key (kbd "M-m f C") 'tddsg/save-file-as-and-open)
  (global-set-key (kbd "M-m t M") 'tddsg/toggle-mode-line)
  (global-set-key (kbd "M-m t H") 'tddsg/toggle-header-line)
  (global-set-key (kbd "M-m t P") 'tddsg/toggle-presentation)

  (global-set-key (kbd "M-s d") 'dictionary-search)
  (global-set-key (kbd "M-s D") 'engine/search-thefreedictionary)
  (global-set-key (kbd "M-s g") 'engine/search-google)
  (global-set-key (kbd "M-s t") 'google-translate-at-point)
  (global-set-key (kbd "M-s T") 'google-translate-query-translate)
  (global-set-key (kbd "M-s r") 'ispell-region)
  (global-set-key (kbd "M-s r") 'ispell-region)
  (global-set-key (kbd "M-s w") 'ispell-word)
  (global-set-key (kbd "M-s s") 'ispell-continue)
  (global-set-key (kbd "M-s f") 'flyspell-buffer)
  (global-set-key (kbd "M-s p") 'flyspell-correct-previous-word-generic)
  (global-set-key (kbd "M-s c") 'flyspell-correct-word-before-point)
  (global-set-key (kbd "M-s n") 'flyspell-goto-next-error)
  (global-set-key (kbd "M-s l") 'langtool-check)
  (global-set-key (kbd "M-s M-l") 'langtool-check-done)

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
  (global-set-key (kbd "C-M-*") 'eyebrowse-rename-window-config)
  (global-set-key (kbd "C-M-(") 'eyebrowse-prev-window-config)
  (global-set-key (kbd "C-M-)") 'eyebrowse-next-window-config)
  (global-set-key (kbd "C-M-_") 'eyebrowse-close-window-config)
  (global-set-key (kbd "C-M-+") 'eyebrowse-create-window-config)
  (global-set-key (kbd "C-x M-<right>") 'eyebrowse-next-window-config)
  (global-set-key (kbd "C-x M-<left>") 'eyebrowse-prev-window-config)

  ;; isearch
  (define-key isearch-mode-map (kbd "C-.") 'tddsg/yank-current-word-to-isearch-buffer)
  (define-key isearch-mode-map (kbd "C-c C-v") 'pdf-isearch-sync-backward-current-match)
  (define-key isearch-mode-map (kbd "<f6>") 'pdf-isearch-sync-backward-current-match)

  ;; swiper
  (define-key swiper-map (kbd "C-.") 'tddsg/yank-current-word-to-minibuffer)

  ;; minibuffer
  (define-key minibuffer-local-map (kbd "C-.") 'tddsg/yank-current-word-to-minibuffer)
  (define-key minibuffer-local-map (kbd "C-M-i") nil)

  ;; elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-M-i") nil)

  ;; shell
  (define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
  (define-key shell-mode-map (kbd "C-z") nil)
  (define-key shell-mode-map (kbd "C-c C-s")
    'tddsg/toggle-shell-scroll-to-bottomon-on-output)


  ;; undo tree
  (define-key undo-tree-map (kbd "C-_") nil)
  (define-key undo-tree-map (kbd "M-_") nil)

  ;; dired-mode
  (defun hook-dired-mode ()
    (toggle-truncate-lines 1))
  (add-hook 'dired-mode-hook 'hook-dired-mode)
  (add-hook 'dired-after-readin-hook 'hook-dired-mode)

  ;; speedbar, make shortcut keys like dired mode
  (with-eval-after-load 'speedbar
    (define-key speedbar-key-map (kbd "+") 'speedbar-create-directory)
    (define-key speedbar-file-key-map (kbd "+") 'speedbar-create-directory)
    (define-key speedbar-key-map (kbd "^") 'speedbar-up-directory))

  ;; magit
  (with-eval-after-load 'magit
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
    (define-key magit-status-mode-map (kbd "M-0") nil))

  ;; god-mode
  (define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
  (define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)
  (define-key god-local-mode-map (kbd "<escape>") 'god-mode-all)
  (define-key isearch-mode-map (kbd "C-z") 'god-mode-isearch-activate)
  (define-key god-mode-isearch-map (kbd "C-z") 'god-mode-isearch-disable)
  (define-key god-local-mode-map (kbd "C-z") 'god-mode-all)
  (define-key god-local-mode-map (kbd "i") 'god-mode-all)
  (define-key god-local-mode-map (kbd "a") 'god-mode-all)

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

  ;; buffer-clone
  (global-set-key (kbd "C-M-S-<left>") 'buf-clone-left)
  (global-set-key (kbd "C-M-S-<right>") 'buf-clone-right)
  (global-set-key (kbd "C-M-S-<up>") 'buf-clone-up)
  (global-set-key (kbd "C-M-S-<down>") 'buf-clone-down)
  (global-set-key (kbd "M-m b c b") 'buf-clone-left)
  (global-set-key (kbd "M-m b c f") 'buf-clone-right)
  (global-set-key (kbd "M-m b c p") 'buf-clone-up)
  (global-set-key (kbd "M-m b c n") 'buf-clone-down)

  ;; LaTeX-mode
  (define-key TeX-mode-map (kbd "<f5>") 'tddsg/latex-compile)
  (define-key TeX-mode-map (kbd "<f6>") 'tddsg/latex-compile-sync-forward)
  (define-key TeX-mode-map (kbd "<f7>") 'tddsg/latex-compile-sync-other-window)
  (define-key TeX-mode-map (kbd "<f8>") 'tddsg/latex-beamer-compile-current-frame)
  (define-key TeX-mode-map (kbd "C-j") nil)
  (define-key TeX-mode-map (kbd "C-M-i") nil)
  (with-eval-after-load 'latex
    (define-key LaTeX-mode-map (kbd "C-M-h") nil)
    (define-key LaTeX-mode-map (kbd "C-j") nil)
    (define-key LaTeX-mode-map (kbd "\"") nil)
    (define-key LaTeX-mode-map (kbd "C-c C-g") nil)
    (define-key LaTeX-mode-map (kbd "C-c C-S-e") 'LaTeX-delete-environment)
    (define-key LaTeX-mode-map (kbd "C-c C-t") 'TeX-remove-macro)
    (define-key LaTeX-mode-map (kbd "C-o") 'helm-imenu)
    (define-key LaTeX-mode-map (kbd "C-c b") 'helm-bibtex)
    (define-key LaTeX-mode-map (kbd "C-M-o") 'reftex-toc)
    (define-key LaTeX-mode-map (kbd "M-g v") 'latex/font-sans-serif)
    (define-key LaTeX-mode-map (kbd "M-g i") 'latex/font-italic)
    (define-key LaTeX-mode-map (kbd "M-g b") 'latex/font-bold)
    (define-key LaTeX-mode-map (kbd "M-g l") 'latex/font-underline)
    (define-key LaTeX-mode-map (kbd "M-g e") 'latex/font-emphasis)
    (define-key LaTeX-mode-map (kbd "M-g r") 'latex/font-clear)
    (define-key LaTeX-mode-map (kbd "M-g c") 'latex/font-code)
    (define-key LaTeX-mode-map (kbd "M-g s") 'latex/font-small-caps)
    ;; (define-key LaTeX-mode-map (kbd "M-g u") 'latex/font-)
    (define-key LaTeX-mode-map (kbd "M-g a") 'latex/font-calligraphic)
    (define-key latex-extra-mode-map (kbd "C-M-f") nil)
    (define-key latex-extra-mode-map (kbd "C-M-b") nil)
    (define-key latex-extra-mode-map (kbd "C-M-n") nil)
    (define-key latex-extra-mode-map (kbd "C-M-p") nil))

  ;; org-mode
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "S-<left>") nil)
    (define-key org-mode-map (kbd "S-<right>") nil)
    (define-key org-mode-map (kbd "S-<up>") nil)
    (define-key org-mode-map (kbd "S-<down>") nil)
    (define-key org-mode-map (kbd "C-j") nil)
    (define-key org-mode-map (kbd "C-a") nil)
    (define-key org-mode-map (kbd "C-c l") nil)
    (define-key org-mode-map (kbd "M-g i") 'spacemacs/org-italic)
    (define-key org-mode-map (kbd "M-g b") 'spacemacs/org-bold)
    (define-key org-mode-map (kbd "M-g r") 'spacemacs/org-clear)
    (define-key org-mode-map (kbd "M-g c") 'spacemacs/org-code)
    (define-key org-mode-map (kbd "M-g u") 'spacemacs/org-underline)
    (define-key org-mode-map (kbd "M-g v") 'spacemacs/org-verbose))

  ;; Python mode
  (define-key python-mode-map (kbd "C-j") nil)

  ;; pdf-tools
  (define-key pdf-view-mode-map (kbd "s d") 'tddsg/pdf-view-disable-auto-slice)
  (define-key pdf-view-mode-map (kbd "s e") 'tddsg/pdf-view-enable-auto-slice)
  (define-key pdf-view-mode-map (kbd "C-<home>") 'pdf-view-first-page)
  (define-key pdf-view-mode-map (kbd "C-<end>") 'pdf-view-last-page)
  (define-key pdf-view-mode-map (kbd "[") 'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map (kbd "]") 'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "M-{") 'pdf-view-previous-page-command)
  (define-key pdf-view-mode-map (kbd "M-}") 'pdf-view-next-page-command)
  (define-key pdf-view-mode-map (kbd "M-w") 'tddsg/pdf-view-kill-ring-save)
  (define-key pdf-view-mode-map (kbd "M-SPC") 'pdf-view-scroll-down-or-previous-page)
  (define-key pdf-view-mode-map (kbd "RET") 'pdf-view-scroll-up-or-next-page)
  (define-key pdf-view-mode-map (kbd "<mouse-8>") 'pdf-history-backward)
  (define-key pdf-view-mode-map (kbd "<mouse-9>") 'pdf-history-forward)

  ;; doc-view
  (with-eval-after-load 'doc-view-mode
    (define-key doc-view-mode-map (kbd "RET") 'doc-view-scroll-up-or-next-page))

  ;; flyspell
  (define-key flyspell-mode-map (kbd "C-;") nil)
  (define-key flyspell-mode-map (kbd "C-,") nil)
  (define-key flyspell-mode-map (kbd "C-M-i") nil)

  ;; dired mode
  (define-key dired-mode-map (kbd "C-^") 'tddsg/dired-home)
  (define-key dired-mode-map (kbd "<backspace>") 'dired-up-directory)
  (define-key dired-mode-map (kbd "M-C") 'tddsg/dired-duplicate-files)

  ;; smartparens
  (define-key smartparens-mode-map (kbd "M-s") nil)

  ;; evil mode
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward)
  (define-key evil-motion-state-map (kbd "C-^") nil)
  (define-key evil-motion-state-map (kbd "C-_") nil)

  ;; ggtags
  (with-eval-after-load 'ggtags
    (define-key ggtags-mode-map (kbd "M-]") nil)
    (define-key ggtags-mode-map (kbd "M-.") 'tddsg/find-definition-dwim)
    (define-key ggtags-mode-map (kbd "M-,") 'tddsg/find-references-dwim)
    (define-key ggtags-mode-map (kbd "C-M-,") 'rtags-find-references)
    (define-key ggtags-mode-map (kbd "C-c M-r") 'tddsg/find-references-dwim))

  ;; irony
  (with-eval-after-load 'irony
    (define-key irony-mode-map (kbd "C-c C-t") 'irony-get-type))

  ;; cc
  (with-eval-after-load 'cc-mode
    (define-key c-mode-base-map (kbd "C-M-j") nil)
    (define-key c-mode-map (kbd "C-M-j") nil)
    (define-key c++-mode-map (kbd "C-M-j") nil))

  ;; company mode
  (define-key company-active-map (kbd "M-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-.") 'company-show-location)

  ;; flycheck
  (define-key flycheck-mode-map (kbd "M-g M-n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "M-g M-p") 'flycheck-previous-error)
  (define-key flycheck-mode-map (kbd "M-g M-f") 'flycheck-first-error)

  ;; reassign key-chords
  (key-chord-define-global "ji" 'indent-region))


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
     (cursor ((t (:background "dark orange"))))
     ;; dired
     (diredp-compressed-file-name ((t (:foreground "burlywood"))))
     (diredp-compressed-file-suffix ((t (:foreground "yellow green"))))
     (diredp-dir-name ((t (:foreground "medium sea green" :weight bold))))
     (diredp-file-name ((t (:foreground "burlywood"))))
     (diredp-file-suffix ((t (:foreground "powder blue"))))
     (diredp-ignored-file-name ((t nil)))
     (diredp-link-priv ((t (:foreground "dodger blue"))))
     (diredp-symlink ((t (:foreground "dodger blue"))))
     ;; monky
     (monky-diff-add ((t (:background "#335533" :foreground "#ddffdd"))))
     (monky-diff-del ((t (:background "#553333" :foreground "#ffdddd"))))
     (monky-diff-hunk-header ((t (:background "#34323e" :foreground "#9a9aba" :slant italic))))
     (monky-diff-title ((t (:background "#292e34" :foreground "#2aa1ae"))))
     (monky-header ((t (:foreground "#4f97d7"))))
     (monky-section-title ((t (:foreground "#4f97d7" :weight bold))))
     ;; hilock
     (hi-blue ((t (:background "medium blue" :foreground "white smoke"))))
     (hi-blue-b ((t (:foreground "deep sky blue" :weight bold))))
     (hi-green ((t (:background "dark olive green" :foreground "white smoke"))))
     (hi-pink ((t (:background "dark magenta" :foreground "white smoke"))))
     (hi-red-b ((t (:foreground "red1" :weight bold))))
     (hi-yellow ((t (:background "dark goldenrod" :foreground "white smoke"))))
     ;; isearch
     (isearch ((t (:background "dark orange" :foreground "#292b2e"))))
     (lazy-highlight ((t (:background "LightGoldenrod3" :foreground "gray10" :weight normal))))
     ;; speedbar
     (speedbar-file-face ((t (:foreground "PeachPuff3"))))
     ;; vline
     (vline ((t (:background "#34424D"))))
     ;; font
     (font-latex-slide-title-face ((t (:inherit font-lock-type-face :weight bold :height 1.3))))
     (font-latex-script-char-face ((t (:foreground "orange red"))))
     (font-latex-verbatim-face ((t (:inherit fixed-pitch :foreground "olive drab"))))
     (font-latex-sedate-face ((t (:foreground "#64A873" :weight normal))))
     (font-latex-subscript-face ((t (:height 0.9))))
     (font-latex-superscript-face ((t (:height 0.9))))
     (font-latex-sectioning-0-face ((t (:foreground "lawn green" :weight bold :height 1.4))))
     (font-latex-sectioning-1-face ((t (:foreground "deep sky blue" :weight bold :height 1.4))))
     (font-latex-sectioning-2-face ((t (:foreground "royal blue" :weight bold :height 1.2))))
     (lazy-highlight ((t (:background "dark goldenrod" :foreground "gray10" :weight normal)))))))

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

(defun tddsg/config-themes ()
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

;; https://www.emacswiki.org/emacs/HeaderLine

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
         (path-len (length file-path))
         (name-len (length file-name))
         (dir-len (length dir-name))
         (drop-str "[...]")
         (path-display-len (- (window-body-width)
                              (length (projectile-project-name)) 3))
         (dir-display-len (- path-display-len (length drop-str) name-len 2)))
    (cond ((< path-len path-display-len)
           (concat "▷ "
                   (with-face dir-name :foreground "DeepSkyBlue3")
                   (with-face file-name :foreground "DarkOrange3")))
          ((and (> dir-len dir-display-len) (> dir-display-len 3))
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
          (t (concat "▷ " (with-face file-name :foreground "DarkOrange3"))))))

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

(defun tddsg--update-header-line ()
  "Update header line of the active buffer and remove from all other."
  (defun exclude-buffer-p (buffer-name)
    (cl-loop for buffer-prefix in (list "*helm" "*spacemacs*")
             thereis (string-match-p (regexp-quote buffer-prefix) buffer-name)))
  (cl-loop for window in (window-list) do
           (with-current-buffer (window-buffer window)
             (when (not (exclude-buffer-p (buffer-name (window-buffer window))))
               (cond ((not tddsg--show-header-line)
                      (setq header-line-format nil))
                     ;; activate header-line of the active buffer
                     ((eq (window-buffer window) (window-buffer (selected-window)))
                      (setq header-line-format (tddsg--create-header-line)))
                     ;; dim header-line of inactive buffers
                     (t (setq header-line-format
                              `(:propertize ,(tddsg--create-header-line)
                                            face (:foreground "grey55")))))))))

(defun tddsg/toggle-header-line ()
  (interactive)
  (setq tddsg--show-header-line (not tddsg--show-header-line))
  (tddsg--update-header-line))

;; update header line of each buffer
(add-hook 'buffer-list-update-hook 'tddsg--update-header-line)
(add-hook 'window-configuration-change-hook 'tddsg--update-header-line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INIT SPACELINE

;;; spaceline segments

(spaceline-define-segment tddsg/line-column
  "The current line and column numbers, or `(current page/number of pages)`
in pdf-view mode (enabled by the `pdf-tools' package)."
  (if (derived-mode-p 'pdf-view-mode)
      (format "(%d/%d)" (pdf-view-current-page) (pdf-cache-number-of-pages))
    "%l:%1c"))

(spaceline-define-segment tddsg/buffer-id
  "Name of buffer."
  (let ((buffer-id (s-trim (powerline-buffer-id 'mode-line-buffer-id))))
    (if (< (length buffer-id) 35) buffer-id
      (concat (substring buffer-id 0 30)
              (propertize "... " 'face 'mode-line-buffer-id)) )))

(spaceline-define-segment tddsg/current-time
  "Current time."
  (format-time-string "%I:%M%p"))

(spaceline-define-segment tddsg/hud
  "A HUD that shows which part of the buffer is currently visible."
  (when (string-match "\%" (format-mode-line "%p"))
    (powerline-hud highlight-face default-face 1))
  :tight t)

;; reuse code from spaceline-config.el
(defun tddsg--create-spaceline-theme (left second-left &rest additional-segments)
  "Convenience function for the spacemacs and emacs themes."
  (spaceline-install 'tddsg
    `(,left
      anzu
      auto-compile
      ,second-left
      major-mode
      ((version-control :when active)
       purpose )
      minor-modes
      (process :when active)
      ((flycheck-error flycheck-warning flycheck-info)
       :when active)
      (mu4e-alert-segment :when active)
      (erc-track :when active)
      (org-pomodoro :when active)
      (org-clock :when active)
      nyan-cat)
    `(which-function
      ;; (python-pyvenv :fallback python-pyenv)
      (battery :when active)
      selection-info
      ,@additional-segments
      ;; (input-method
      ;;  ;; (buffer-encoding-abbrev
      ;;  ;;  :when (and active (not (derived-mode-p 'pdf-view-mode))))
      ;;  (buffer-position :when active))
      tddsg/current-time))
  (if (null mode-line-format)
      (setq mode-line-format '("%e" (:eval (spaceline-ml-tddsg))))
    (setq-default mode-line-format '("%e" (:eval (spaceline-ml-tddsg))))))

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
           tddsg/hud
           point-position
           tddsg/line-column
           ;; buffer-size
           tddsg/buffer-id
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

(defun tddsg/config-spaceline ()
  (interactive)
  (if (not tddsg--show-mode-line) (setq mode-line-format nil)
    (progn
      (setq spaceline-highlight-face-func 'tddsg--spaceline-highlight-face)
      (tddsg--create-spaceline-final))))

(defun tddsg/toggle-mode-line()
  (interactive)
  (setq tddsg--show-mode-line (not tddsg--show-mode-line))
  (tddsg/config-spaceline)
  (force-mode-line-update))

(defun tddsg/toggle-presentation()
  (interactive)
  (if (or tddsg--show-header-line tddsg--show-mode-line)
      (setq tddsg--show-header-line nil
            tddsg--show-mode-line nil)
    (setq tddsg--show-header-line t
          tddsg--show-mode-line t))
  (tddsg--update-header-line)
  (tddsg/config-spaceline))

;; update mode line after every 60 seconds
(run-at-time 60 60 #'force-mode-line-update)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INIT CUSTOM

(defun tddsg/config-custom-vars ()
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
       ("center")
       ("flushleft")
       ("flushright")
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
       ("TRUNG" . "red")
       ("HACK" . "red")
       ("FIXME" . "red")
       ("XXX" . "red")
       ("XXXX" . "red")
       ("???" . "red")
       ("BUG" . "red")
       ("OK" . "red"))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FINALLY, OVERRIDE OTHER EMACS'S FUNCTION


;;; Customize helm-do-ag

(require 'helm-ag)

;;;###autoload
(defun helm-projectile-ag (&optional options)
  "Helm version of projectile-ag."
  (interactive (if current-prefix-arg (list (read-string "option: " "" 'helm-ag--extra-options-history))))
  (if (require 'helm-ag nil t)
      (if (projectile-project-p)
          (let* ((ag-ignored-files (cl-union (projectile-ignored-files-rel)
                                             (projectile-patterns-to-ignore)))
                 (ag-ignored-dirs (projectile-ignored-directories-rel))
                 (ignored (mapconcat (lambda (i) (concat "--ignore " i))
                                     (append ag-ignored-files
                                             ag-ignored-dirs)
                                     " "))
                 (helm-ag-command-option options)
                 (helm-ag-base-command (concat helm-ag-base-command " " ignored))
                 (current-prefix-arg nil))
            (helm-do-ag (projectile-project-root) (car (projectile-parse-dirconfig-file))))
        (error "You're not in a project"))
    (when (yes-or-no-p "`helm-ag' is not installed. Install? ")
      (condition-case nil
          (progn
            (package-install 'helm-ag)
            (helm-projectile-ag options))
        (error (error "`helm-ag' is not available. Is MELPA in your `package-archives'?"))))))

(defun helm-ag-set-extra-option ()
  "Set extra options for helm-ag"
  (interactive)
  (let ((option (read-string "Helm-ag: set extra options: "
                             (or helm-ag--extra-options "")
                             'helm-ag--extra-options-history)))
    (setq helm-ag--extra-options option)))

;;;;; module helm-ag.el
;;;;; show ag options in helm-ag buffer
(defun helm-ag--put-result-in-save-buffer (result search-this-file-p)
  (setq buffer-read-only t)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "-*- mode: helm-ag -*-\n\n"
            (format "Ag Results for `%s'%s:\n\n"
                    helm-ag--last-query
                    (if (string= helm-ag--extra-options "") ""
                      (format ", with options `%s'" helm-ag--extra-options))))
    (save-excursion
      (insert result)))
  (helm-ag-mode)
  (unless (helm-ag--vimgrep-option)
    (setq-local helm-ag--search-this-file-p search-this-file-p))
  (setq-local helm-ag--default-directory default-directory))

;;;;; PDF-VIEW MODE

(require 'pdf-view)
(require 'pdf-isearch)
(require 'pdf-sync)

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
        (run-at-time 0.02 nil
                     (lambda (top)
                       ;; display tooltip by a timer to avoid being cleared
                       (when (derived-mode-p 'pdf-view-mode)
                         (pdf-util-tooltip-arrow (round top) 20)))
                     top)
        ;;; old code
        ;; (pdf-util-tooltip-arrow (round top) 20)
        )
      (with-current-buffer buffer (run-hooks 'pdf-sync-forward-hook)))))

;;;;;; customize pdf-isearch for syncing backward
(defun pdf-isearch-sync-backward-current-match ()
  "Sync backward to the LaTeX source of the current match."
  (interactive)
  (if pdf-isearch-current-match
      (let ((left (caar pdf-isearch-current-match))
            (top (cadar pdf-isearch-current-match)))
        (isearch-exit)
        (funcall 'pdf-sync-backward-search left top))))

;;;;;;; REASON MODE ;;;;;;;;

(defun tddsg/config-reason-mode ()
  (require 'reason-mode)
  (require 'merlin)
  (require 'merlin-imenu)
  (defun chomp-end (str)
    "Chomp tailing whitespace from STR."
    (replace-regexp-in-string (rx (* (any " \t\n")) eos)
                              ""
                              str))
  (defun my-reason-hook ()
    (add-hook 'before-save-hook 'refmt-before-save)
    (merlin-mode)
    (merlin-use-merlin-imenu))

  (let ((support-base-dir (concat (replace-regexp-in-string "refmt" "" (file-truename (chomp-end (shell-command-to-string "which refmt")))) ".."))
        (merlin-base-dir (concat (replace-regexp-in-string "ocamlmerlin" "" (file-truename (chomp-end (shell-command-to-string "which ocamlmerlin")))) "..")))
    ;; Add npm merlin.el to the emacs load path and tell emacs where to find ocamlmerlin
    (add-to-list 'load-path (concat merlin-base-dir "/share/emacs/site-lisp/"))
    (setq merlin-command (concat merlin-base-dir "/bin/ocamlmerlin"))

    ;; Add npm reason-mode to the emacs load path and tell emacs where to find refmt
    (add-to-list 'load-path (concat support-base-dir "/share/emacs/site-lisp"))
    (setq refmt-command (concat support-base-dir "/bin/refmt")))
  (setq merlin-ac-setup t)
  (add-hook 'reason-mode-hook 'my-reason-hook))
