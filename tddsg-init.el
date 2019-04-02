;;; package --- Summary

;;; Commentary:

;;; Some note:
;;  1. Using `buffer-list-update-hook` is costly, due to helm interfaces

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOAD FILES

(require 'smartparens)
(require 'company)
(require 'face-remap)
(require 'whitespace)
(require 'expand-region)
(require 'engine-mode)
(require 'hi-lock)
(require 'seq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PRIVATE VARIABLES

;; used to jump between faces
(defvar tddsg--face-change-types
  '(merlin-compilation-error-face merlin-compilation-warning-face
    flycheck-error
    ;; rtags-fixitline rtags-warnline rtags-errline
    writegood-weasels-face writegood-duplicates-face writegood-passive-voice-face
    langtool-errline
    evil-search-highlight-persist-highlight-face
    lazy-highlight
    hi-yellow hi-pink hi-green hi-blue hi-black-b
    hi-blue-b hi-green-b hi-red-b hi-black-hb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTILITY FUNCTIONS

(defun blank-char-p (char)
  "Check if CHAR is a SPACE or TAB character."
  (or (equal char ?\s) (equal char ?\t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PRIVATE FUNCTIONS

(defun tddsg--fix-comint-window-size ()
  "Change process window size."
  (when (derived-mode-p 'comint-mode)
    (let ((process (get-buffer-process (current-buffer))))
      (when process
        (set-process-window-size process (window-height) (window-width))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INTERACTIVE FUNCTIONS

(defun tddsg/describe-face-under-cursor ()
  "Describe the face information under the cursor."
  (interactive)
  (let* ((pos (point))
         (face (or (get-char-property pos 'read-face-name)
                   (get-char-property pos 'face))))
    (if face (message "Face under the cursor: %s" face)
      (message "Face not found under the cursor at: %d" pos))))

(defun tddsg/find-face-change (&optional direction)
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
      (cond ((or (and (eq direction 'forward) (>= pos (point-max)))
                 (and (eq direction 'backward) (<= pos (point-min))))
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
    (while (and (<= pos (point-max))
                (>= pos (point-min))
                (not (wanted-face-p pos)))
      (setq pos (find-face-change pos)))
    (when (and (>= pos 0) (wanted-face-p pos))
      ;; refine for backward finding
      (when (eq direction 'backward) (setq pos (1+ (find-face-change pos))))
      (goto-char pos)
      (tddsg/describe-face-under-cursor))))

(defun tddsg/next-face-change ()
  "Find next face change"
  (interactive)
  (tddsg/find-face-change 'forward))

(defun tddsg/previous-face-change ()
  "Find previous face change"
  (interactive)
  (tddsg/find-face-change 'backward))

(defun tddsg/traverse-upcase (&optional direction delete)
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
    (while (and (<= (point) (point-max))
                (>= (point) (point-min))
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

(defun tddsg/next-upcase ()
  "Go to next contradict char."
  (interactive)
  (tddsg/traverse-upcase 'forward))

(defun tddsg/previous-upcase ()
  "Go to previous contradict char."
  (interactive)
  (tddsg/traverse-upcase 'backward))

(defun tddsg/delete-until-next-upcase ()
  "Delete until next contradict char."
  (interactive)
  (tddsg/traverse-upcase 'forward t))

(defun tddsg/delete-until-previous-upcase ()
  "Delete until previous contradict char."
  (interactive)
  (tddsg/traverse-upcase 'backward t))

(defun tddsg/highlight-symbol-at-point-or-region ()
  "Highligh thing at point or region"
  (interactive)
  (if (region-active-p)
      (let* ((begin (region-beginning))
             (end (region-end))
             (regexp (concat "\\<"
                             (buffer-substring-no-properties begin end) "\\>"))
	           (hi-lock-auto-select-face t)
	           (face (hi-lock-read-face-name)))
        (or (facep face) (setq face 'hi-yellow))
        (unless hi-lock-mode (hi-lock-mode 1))
        (hi-lock-set-pattern regexp face)
        (deactivate-mark))
    (call-interactively 'highlight-symbol-at-point)))

(defun tddsg/open-with ()
  "Open the underlying file of a buffer in an external program."
  (interactive)
  (defun ask-command (default)
    (let* ((prompt (format "Open the current file with: [%s]: " default))
           (command (read-shell-command prompt nil
                                        'dired-shell-command-history)))
      (if (equal command "") default command)))
  (defun get-shell-command ()
    (let* ((extension (file-name-extension (buffer-file-name)))
           (default-command
             (cond ((string= extension "pdf")
                    (if (derived-mode-p 'pdf-view-mode)
                        (format "okular -p %d" (pdf-view-current-page))
                      "okular"))
                   ((member extension '("txt" "el" "ml" "c" "cpp" "java"))
                    "geany")
                   ((member extension '("html" "htm" "xml" "md"))
                    "google-chrome")
                   ((eq system-type 'darwin) "open")
                   ((member system-type '(gnu gnu/linux)) "xdg-open")
                   (t ""))))
      (ask-command default-command)))
  (let* ((command (get-shell-command)))
    (call-process-shell-command (concat command " " buffer-file-name "&"))))

;; swith between shell buffer
(defun tddsg/switch-buffer-of-mode (direction mode)
  "Jump to the next or previous buffer of the same mode,
DIRECTION is 'next or 'previous."
  (defun change-buffer (org-buff cur-buff)
    (let ((new-buff (cond ((eq direction 'next) (next-buffer))
                          ((eq direction 'prev) (previous-buffer))
                          (t nil))))
      (if (equal new-buff org-buff) nil
        (with-current-buffer new-buff
          (if (equal mode major-mode) new-buff
            (change-buffer org-buff new-buff))))))
  (let* ((cur-buff (current-buffer))
         (new-buff (change-buffer cur-buff cur-buff)))
    (if (not new-buff)
        (message "New buffer of mode '%s' not found!" mode)
      (set-buffer new-buff))))

(defun tddsg/next-buffer-same-mode ()
  "Jump to the next buffer of the same mode."
  (interactive)
  (tddsg/switch-buffer-of-mode 'next major-mode))

(defun tddsg/previous-buffer-same-mode ()
  "Jump to the previous buffer of the same mode."
  (interactive)
  (tddsg/switch-buffer-of-mode 'prev major-mode))

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
      (while (looking-at "[[:space:]]*$") (next-line 1))
      (backward-paragraph 1)
      (if (not (eq (point) (point-min))) (next-line))
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

(defun tddsg/vc-status ()
  "Show version control status (git, hg) of the current project."
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

(defun tddsg/find-definition (&optional prefix)
  "Goto definition of a function or a variable."
  (interactive "P")
  (cond ((derived-mode-p 'cc-mode 'c-mode 'c++-mode)
         (if (and (not (rtags-find-symbol-at-point prefix))
                  rtags-last-request-not-indexed)
             (gtags-find-tag)))))

(defun tddsg/find-references (&optional prefix)
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
  (if (not (null arg))
      (call-interactively 'pdf-view-kill-ring-save)
    (pdf-view-assert-active-region)
    (let* ((text (pdf-view-active-region-text))
           (text (mapconcat 'identity text ""))
           (text (replace-regexp-in-string "-\n" "" text)) ;; "-" at EOL
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

(defun tddsg/yank-word-minibuffer ()
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

(defun tddsg/yank-current-word-isearch ()
  "Pull current word from buffer into search string."
  (interactive)
  (save-excursion
    (skip-syntax-backward "w_")
    (isearch-yank-internal
     (lambda ()
       (skip-syntax-forward "w_")
       (point)))))

(defun tddsg/beginning-of-line ()
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
  ;; save editing buffers
  (let ((root (projectile-project-root)))
    (save-some-buffers (and root (not compilation-ask-about-save))
                       (lambda ()
                         (projectile-project-buffer-p (current-buffer) root))))
  (when (string-match-p (regexp-quote "make") compile-command)
    (setq compile-command
          (format "make -k -C %s -j4" (find-make-dir default-directory))))
  (call-interactively 'compile))

(defun tddsg/recompile ()
  "Find the closest Makefile and compile."
  (interactive)
  (when (derived-mode-p 'prog-mode 'tuareg-mode)
    ;; save editing buffers
    (let ((root (projectile-project-root)))
      (save-some-buffers
       (and root (not compilation-ask-about-save))
       (lambda () (projectile-project-buffer-p (current-buffer) root))))
    ;; compile
    (if (string-match-p (regexp-quote "make -k -C") compile-command)
        (recompile)
      (call-interactively 'tddsg/compile))))

(defun tddsg/latex-compile-project ()
  "Compile the main file of a LaTeX project."
  (interactive)
  ;; save files
  (let ((root (projectile-project-root)))
    (save-some-buffers (and root (not compilation-ask-about-save))
                       (lambda () (projectile-project-buffer-p (current-buffer)
                                                               root))))
  ;; set main
  (let* ((extension (file-name-extension (buffer-file-name)))
         (main-tex (cond ((and (string= extension "tex") (stringp TeX-master))
                          TeX-master)
                         ((and (string= extension "tex") TeX-master)
                          (buffer-file-name))
                         (t (concat default-directory "main.tex")))))
    (setq TeX-master main-tex)
    (TeX-command "LaTeX" 'TeX-master-file -1)))

(defun tddsg/latex-beamer-compile-frame ()
  "Run pdflatex on current beamer frame."
  (interactive)
  (save-buffer)
  (setq TeX-region "frame_beamer")
  (let (beg)
    (save-excursion
      (search-backward "\\begin{frame}")
      (setq beg (point))
      (forward-char 1)
      (LaTeX-find-matching-end)
      (TeX-pin-region beg (point))
      (letf (( (symbol-function 'TeX-command-query) (lambda (x) "LaTeX")))
        (TeX-command-region)))))

(defun tddsg/latex-beamer-view-frame ()
  "View PDF of the frame."
  (interactive)
  (let* ((frame-name "frame_beamer.pdf")
         (frame-path (concat default-directory frame-name))
         (frame-window (get-window-with-predicate
                        (lambda (window)
                          (let ((buff-name (buffer-name (window-buffer window))))
                            (string-match-p frame-name buff-name))))))
    (if frame-window (select-window frame-window)
      (progn (other-window 1) (find-file frame-path)))
    (pdf-view-first-page)))

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

(defun tddsg/toggle-shell-scroll-output ()
  "Toggle shell scroll to the last line on output."
  (interactive)
  (if (derived-mode-p 'shell-mode)
      (cond (comint-scroll-to-bottom-on-output
             (setq comint-scroll-to-bottom-on-output nil)
             (setq mode-name "Shell ⚡⚡⚡"))
            (t
             (setq comint-scroll-to-bottom-on-output t)
             (setq mode-name "Shell")))))

(defun tddsg/open-shell (arg)
  "Open a new shell or the most recent shell."
  (interactive "P")
  (defun select-new-window ()
    (other-window 1)
    (while (equal popwin:popup-window (selected-window))
      (other-window 1)))
  (if (not (null arg)) (call-interactively 'shell)
    (let ((has-shell nil))
      (cl-loop for buffer in (buffer-list)
               until (with-current-buffer buffer
                       (if (equal major-mode 'shell-mode)
                           (progn (select-new-window)
                                  (switch-to-buffer buffer)
                                  (setq has-shell t)
                                  t))))
      (if (not has-shell) (call-interactively 'shell)))))

(defun tddsg/recent-dirs ()
  "Open a directory from a list of recent dirs"
  (interactive)
  (let ((recent-dirs
         (delete-dups
          (mapcar (lambda (f)
                    (if (file-directory-p f) f (file-name-directory f)))
                  recentf-list))))
    (let ((dir (ivy-read "Choose a directory: "
                         recent-dirs
                         :re-builder #'ivy--regex
                         :sort nil
                         :initial-input nil)))
      (dired dir))))

(defun tddsg/scroll-up-fast ()
  (interactive)
  (scroll-up-command 10))

(defun tddsg/scroll-down-fast ()
  (interactive)
  (scroll-down-command 10))

(defun tddsg/clean-recentf-list (pattern)
  (interactive "sEnter a file pattern that will be cleaned: ")
  (add-to-list 'recentf-exclude pattern)
  (recentf-cleanup)
  (setq recentf-exclude (remove pattern recentf-exclude)))

(defun tddsg/show-special-whitespaces ()
  "Display special whitespace characters."
  (interactive)
  (defun show-white-space ()
    (whitespace-mode -1)
    ;; Code is adopted from: http://ergoemacs.org/emacs/whitespace-mode.html
    (setq whitespace-style
          '(face spaces tabs newline space-mark tab-mark newline-mark ))
    (setq whitespace-display-mappings
          '((space-mark 32 [183] [46]) ; SPACE,「·」
            (newline-mark 10 [182 10]) ; LINE FEED, “¶”
            (tab-mark 9 [9655 9] [92 9]) ; tab, “▷”
            ))
    (whitespace-mode 1))
  (cl-loop for buffer in (buffer-list) do
           (with-current-buffer buffer (show-white-space))))

(defun tddsg/hide-special-whitespaces ()
  "Do not display special whitespace characters."
  (interactive)
  (defun hide-white-space ()
    (whitespace-mode -1)
    (setq whitespace-style '(face tabs))
    (whitespace-mode 1))
  (cl-loop for buffer in (buffer-list) do
           (with-current-buffer buffer (hide-white-space))))

(defun tddsg/disable-ocp-indent ()
  (interactive)
  (setq indent-line-function 'indent-relative))

(defun tddsg/enable-ocp-indent ()
  (interactive)
  (setq indent-line-function 'ocp-indent-line))

(defun tddsg/config-buffer-specific ()
  (when (buffer-file-name)
    (cond ((derived-mode-p 'tuareg-mode)
           ;; disable ocp-ident for specific files
           (let ((extension (file-name-extension (buffer-file-name))))
             (cond ((member extension '("mly" "mll"))
                    (tddsg/disable-ocp-indent))
                   (t (tddsg/enable-ocp-indent)))))
          ((derived-mode-p 'c-mode 'c++-mode)
           ;; (guess-style-guess-all)
           )
          ((derived-mode-p 'python-mode)
           (guess-style-guess-all)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INIT CONFIGS

(defun tddsg/config-packages ()
  ;; visual interface setting
  (global-hl-todo-mode 1)
  (blink-cursor-mode 1)
  (setq blink-cursor-blinks 0)
  (setq tab-width 2)
  (setq text-scale-mode-step 1.1)
  (setq powerline-default-separator 'bar)
  (spacemacs/add-evil-cursor "emacs" "Orange" 'box)

  ;; scrolling
  (setq scroll-margin 5)
  (setq scroll-step 0)
  (setq fast-but-imprecise-scrolling t)

  ;; purpose mode
  (purpose-mode -1)

  ;; popwin mode
  (popwin-mode 1)

  ;; windows setting
  (setq window-combination-resize nil)   ;; stop automatically resize windows
  (setq pupo-split-active-window t)

  ;; mode paragraph setting
  (setq paragraph-separate "[ \t\f]*$"
        paragraph-start "\f\\|[ \t]*$")

  ;; save
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
  (setq spell-checking-enable-by-default nil)

  ;; isearch
  (setq lazy-highlight-cleanup nil)
  (defun isearch-yank-marked-text ()
    (when (region-active-p)
      (let* ((begin (region-beginning))
             (end (region-end))
             (searched-text (buffer-substring-no-properties begin end)))
        (deactivate-mark)
        (goto-char begin)
        (isearch-yank-string searched-text))))
  (add-hook 'isearch-mode-hook 'isearch-yank-marked-text)

  ;; company-mode
  (setq company-idle-delay 300)
  (setq company-tooltip-idle-delay 300)
  (global-company-mode)

  ;; mode editing setting
  (electric-pair-mode -1)         ;; electric-pair may conflict with smartparens
  (delete-selection-mode t)       ;; delete selection by keypress
  (setq require-final-newline t)                       ;; newline at end of file
  (defadvice newline (after indent activate) (indent-according-to-mode))
  (ad-unadvise 'kill-region)

  ;; some Emacs threshold
  (setq max-lisp-eval-depth 50000)
  (setq max-specpdl-size 50000)
  (setq dotspacemacs-large-file-size 10)   ;; large file of spacemacs is 10MB

  ;; compilation
  (setq compilation-ask-about-save nil
        compilation-window-height 12
        compilation-scroll-output t
        compilation-skip-threshold 2)
  ;; reset all compilation hook, use the default one
  (setq compilation-mode-hook nil)

  ;; undo tree
  (global-undo-tree-mode 0)

  ;; shell
  (setq comint-prompt-read-only nil
        comint-scroll-show-maximum-output t
        comint-completion-autolist t
        comint-input-ignoredups t
        comint-completion-addsuffix nil
        shell-default-shell 'ansi-term)
  (defadvice comint-clear-buffer (before comint activate) (end-of-buffer))
  (defun hook-shell-mode ()
    "Hook to run in shell mode."
    (add-hook 'window-configuration-change-hook
              'tddsg--fix-comint-window-size nil t)
    (rainbow-delimiters-mode-disable)
    (toggle-truncate-lines -1)
    (visual-line-mode 1))
  (add-hook 'shell-mode-hook 'hook-shell-mode)

  ;; which-key
  (setq which-key-idle-delay 1.2)

  ;; smartparens
  (smartparens-global-mode)

  ;; auto-revert
  (global-auto-revert-mode t)
  (setq auto-revert-check-vc-info nil)

  ;; buffer specific
  (add-hook 'first-change-hook 'tddsg/config-buffer-specific)

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

  ;; helm setting
  (with-eval-after-load 'helm-mode
    (setq helm-ag-insert-at-point 'symbol     ;; insert symbol in helm-ag
          helm-split-window-inside-p t
          projectile-completion-system 'helm
          helm-ff-file-name-history-use-recentf t
          helm-ff-transformer-show-only-basename nil)
    (add-to-list 'helm-sources-using-default-as-input
                 'helm-source-grep-ag)
    (substitute-key-definition 'find-tag 'helm-etags-select global-map)
    ;; make Helm split inside the active window in a few function
    (defun helm-split-active-window (orig-func &rest args)
      (setq helm-display-function 'helm-default-display-buffer)
      (apply orig-func args)
      (setq helm-display-function 'spacemacs//display-helm-window))
    (cl-loop for function in `(helm-company
                               helm-semantic-or-imenu
                               helm-imenu
                               completion-at-point
                               flyspell-correct-previous-word-generic)
             do (advice-add function :around #'helm-split-active-window)))

  ;; ag-search
  (setq helm-ag-use-agignore t)

  ;; browser
  (setq browse-url-generic-program "sensible-browser"     ;; use default browser
        browse-url-browser-function 'browse-url-generic
        engine/browser-function 'browse-url-generic)

  ;; minibuffer
  (setq resize-mini-windows t)
  (setq max-mini-window-height 30)

  ;; latex
  (with-eval-after-load 'latex-mode
    (setq LaTeX-font-list
          (quote
           ((?\C-a ""              ""  "\\mathcal{"    "}")
            (?\C-b "\\textbf{"     "}" "\\mathbf{"     "}")
            (?\C-c "\\textsc{"     "}")
            (?\C-e "\\emph{"       "}")
            (?\C-f "\\textsf{"     "}" "\\mathsf{"     "}")
            (?\C-i "\\textit{"     "}" "\\mathit{"     "}")
            (?\C-m "\\textmd{"     "}")
            (?\C-n "\\textnormal{" "}" "\\mathnormal{" "}")
            (?\C-r "\\textrm{"     "}" "\\mathrm{"     "}")
            (?\C-s "\\textsl{"     "}" "\\mathbb{"     "}")
            (?\C-t "\\texttt{"     "}" "\\mathtt{"     "}")
            (?\C-u "\\underline{"     "}")
            (?\C-d "" "" t)))))

  ;; dired-mode
  (add-to-list 'savehist-additional-variables 'helm-dired-history-variable)
  (setq dired-guess-shell-alist-user
        '(("\\.pdf\\'" "okular")
          ("\\.html\\|\\.xml*" "sensible-browser")
          ("\\.txt\\|\\.log" "gedit")
          ("\\.el\\|\\.ml\\|\\.h*\\|\\.c*\\|\\.java" "gedit")
          ("\\.tex\\|\\.bib" "texstudio")
          ("\\.jpg*\\|\\.png*\\|\\.gif*\\|\\.svg*" "eog")
          ("\\.doc*\\|\\.xls*\\|\\.ppt*" "libreoffice")
          ("\\.ods*\\|\\.odt*" "libreoffice")
          ("\\..*" "xdg-open")))
  (defun hook-dired-mode ()
    (toggle-truncate-lines 1))
  (add-hook 'dired-mode-hook 'hook-dired-mode)
  (add-hook 'dired-after-readin-hook 'hook-dired-mode)

  ;; web-mode
  (with-eval-after-load 'web-mode
    (setq web-mode-code-indent-offset 2
          web-mode-indent-style 2
          web-mode-css-indent-offset 2
          web-mode-markup-indent-offset 2))

  ;; pdf-tools
  (defadvice pdf-sync-forward-search (after comint activate) (other-window 1))

  ;; spaceline
  (spaceline-toggle-buffer-encoding-abbrev-off)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-position-on)
  (spaceline-toggle-hud-off)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-version-control-off)

  ;; python-mode
  (defun hook-python-mode ()
    (elpy-mode))
  (add-hook 'python-mode-hook 'hook-python-mode)

  ;; ggtags
  (setq ggtags-process-environment '("GTAGSLIBPATH=/home/trungtq/.gtags"))

  ;; spacemacs
  (push "\\*magit\.\+" spacemacs-useful-buffers-regexp)
  (push "\\*monky\.\+\\*" spacemacs-useful-buffers-regexp)
  (push "\\*compilation\\*" spacemacs-useful-buffers-regexp)

  ;; whichkey
  (which-key-add-key-based-replacements "C-c !" "flycheck")
  (which-key-add-key-based-replacements "C-c ," "semantic")
  (which-key-add-key-based-replacements "C-c @" "hideshow")
  (which-key-add-key-based-replacements "C-c C-w" "eyebrowse")
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
  (spacemacs|diminish auto-revert-mode "")
  (spacemacs|diminish abbrev-mode "")
  (spacemacs|diminish smartparens-mode "")
  (spacemacs|diminish rainbow-mode "")
  (spacemacs|diminish auto-fill-function "")
  (spacemacs|diminish visual-line-mode "")
  (spacemacs|diminish merlin-mode "")
  (spacemacs|diminish magit-gitflow-mode "")
  (spacemacs|diminish flycheck-mode "")
  (spacemacs|diminish flyspell-mode "")
  (spacemacs|diminish holy-mode "")
  (spacemacs|diminish projectile-mode "")
  (spacemacs|diminish compilation-minor-mode "⚡⚡⚡COMPILING⚡⚡⚡")

  ;; hook text mode
  (defun hook-text-mode ()
    "Hook to run in 'text-mode'."
    (setq fill-column 72)
    (smartparens-global-mode 1)
    (column-marker-3 80)
    (whitespace-mode 1)
    (rainbow-delimiters-mode-enable)
    (linum-mode 1))
  (add-hook 'text-mode-hook 'hook-text-mode)

  ;; hook prog mode
  (defun hook-prog-mode ()
    "Hook to run in 'prog-mode'."
    (setq fill-column 72)
    (smartparens-global-mode 1)
    (column-marker-3 80)
    (whitespace-mode 1)
    (linum-mode 1)
    (flycheck-mode -1))
  (add-hook 'prog-mode-hook 'hook-prog-mode)

  (defun hook-change-major-mode ()
    ;; change some weird keys
    (keyboard-translate ?\C-\[ ?\H-\[)
    (keyboard-translate ?\C-i ?\H-i)
    (keyboard-translate ?\C-m ?\H-m)
    (define-key input-decode-map (kbd "C-M-m") (kbd "H-M-m"))
    (define-key input-decode-map (kbd "C-M-[") (kbd "H-M-["))
    (define-key input-decode-map (kbd "C-s-[") (kbd "H-s-["))
    (define-key input-decode-map (kbd "C-S-I") (kbd "H-I"))
    (define-key input-decode-map (kbd "C-S-M") (kbd "H-M")))
  (add-hook 'change-major-mode-hook 'hook-change-major-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INIT KEYS

(defun tddsg/config-keys ()
  ;; unbind some weird keys
  (global-set-key (kbd "<home>") 'tddsg/beginning-of-line)
  (global-set-key (kbd "<esc>") 'evil-force-normal-state)
  (global-set-key (kbd "<f5>") 'tddsg/recompile)
  (global-set-key (kbd "<f6>") 'tddsg/open-shell)

  (global-set-key (kbd "C-<backspace>") 'backward-kill-word)
  (global-set-key (kbd "C-<delete>") 'kill-word)
  (global-set-key (kbd "C-<left>") 'left-word)
  (global-set-key (kbd "C-<right>") 'right-word)
  (global-set-key (kbd "C-+") 'zoom-in)
  (global-set-key (kbd "C--") 'zoom-out)
  (global-set-key (kbd "C-'") 'other-window)
  (global-set-key (kbd "C-j") 'avy-goto-word-1)
  (global-set-key (kbd "M-j") 'avy-goto-word-1)
  (global-set-key (kbd "C-S-j") 'avy-goto-char)
  (global-set-key (kbd "C-o") 'helm-semantic-or-imenu)
  (global-set-key (kbd "C-a") 'tddsg/beginning-of-line)
  (global-set-key (kbd "C-w") 'tddsg/kill-active-region)
  (global-set-key (kbd "C-q") 'goto-last-change)
  (global-set-key (kbd "C-/") 'undo)
  (global-set-key (kbd "C-;") 'iedit-mode)
  (global-set-key (kbd "C-^") 'tddsg/join-with-beneath-line)
  (global-set-key (kbd "C-_") 'tddsg/join-to-above-line)
  (global-set-key (kbd "C-\\") 'sp-split-sexp)

  (global-set-key (kbd "C-S-<backspace>") 'kill-whole-line)
  (global-set-key (kbd "C-S-k") 'kill-whole-line)
  (global-set-key (kbd "C-S-/") 'undo-tree-redo)
  (global-set-key (kbd "C-S-p") 'tddsg/scroll-down-fast)
  (global-set-key (kbd "C-S-n") 'tddsg/scroll-up-fast)

  (global-set-key (kbd "C-M-o") 'helm-imenu-anywhere)
  (global-set-key (kbd "C-M-h") 'tddsg/mark-environment)
  (global-set-key (kbd "C-M-k") 'tddsg/smart-kill-sexp-forward)
  (global-set-key (kbd "C-M-S-k") 'tddsg/smart-kill-sexp-backward)
  (global-set-key (kbd "C-M-j") 'tddsg/join-with-beneath-line)
  (global-set-key (kbd "C-M-i") 'tddsg/join-to-above-line)
  (global-set-key (kbd "C-M-SPC") 'tddsg/mark-sexp-forward)
  (global-set-key (kbd "C-M-S-SPC") 'tddsg/mark-sexp-backward)
  (global-set-key (kbd "C-M-;") 'tddsg/comment-paragraph)
  (global-set-key (kbd "C-M-a") 'sp-beginning-of-sexp)
  (global-set-key (kbd "C-M-e") 'sp-end-of-sexp)

  (global-set-key (kbd "C-x p") 'purpose-mode)
  (global-set-key (kbd "C-x t") 'transpose-paragraphs)
  (global-set-key (kbd "C-x g") 'tddsg/vc-status)
  (global-set-key (kbd "C-x {") 'shrink-window-horizontally)
  (global-set-key (kbd "C-x }") 'enlarge-window-horizontally)
  (global-set-key (kbd "C-x _") 'shrink-window)
  (global-set-key (kbd "C-x ^") 'enlarge-window)
  (global-set-key (kbd "C-x w s") 'tddsg/save-file-as-and-open)
  (global-set-key (kbd "C-x w d") 'spacemacs/toggle-current-window-dedication)

  (global-set-key (kbd "C-x C-d") 'tddsg/recent-dirs)
  (global-set-key (kbd "C-x C-b") 'switch-to-buffer)
  (global-set-key (kbd "C-x C-p") 'popwin-mode)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-z") nil)
  (global-set-key (kbd "C-z") nil)

  (global-set-key [?\H-m] 'helm-mini)
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
  (global-set-key (kbd "C-c s") 'projectile-save-project-buffers)
  (global-set-key (kbd "C-c c") 'flycheck-mode)
  (global-set-key (kbd "C-c i") 'ivy-resume)
  (global-set-key (kbd "C-c j") 'avy-resume)
  (global-set-key (kbd "C-c h") 'helm-resume)
  (global-set-key (kbd "C-c g") 'tddsg/helm-do-ag)
  (global-set-key (kbd "C-c d") 'tddsg/duplicate-region-or-line)
  (global-set-key (kbd "C-c m") 'tddsg/open-shell)
  (global-set-key (kbd "C-c v") 'tddsg/describe-face-under-cursor)
  (global-set-key (kbd "C-c @ s") 'tddsg/show-special-whitespaces)
  (global-set-key (kbd "C-c @ h") 'tddsg/hide-special-whitespaces)

  ;; (global-set-key (kbd "C-c C-c") nil)
  (global-set-key (kbd "C-c C-c") 'tddsg/compile)
  (global-set-key (kbd "C-c C-g") 'helm-projectile-ag)
  (global-set-key (kbd "C-c C-k") 'kill-matching-buffers)
  (global-set-key (kbd "C-c C-s") 'projectile-save-project-buffers)
  (global-set-key (kbd "C-c C-SPC") 'helm-all-mark-rings)

  (global-set-key (kbd "M-SPC") 'tddsg/one-space-or-blank-line)
  (global-set-key (kbd "M-<backspace>") 'backward-kill-word)
  (global-set-key (kbd "M-<delete>") 'kill-word)
  (global-set-key (kbd "M-w") 'tddsg/kill-ring-save)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "M-k") 'backward-kill-word)
  (global-set-key (kbd "M-p") 'backward-paragraph)
  (global-set-key (kbd "M-n") 'forward-paragraph)
  (global-set-key (kbd "M-P") 'tddsg/previous-face-change)
  (global-set-key (kbd "M-N") 'tddsg/next-face-change)
  (global-set-key (kbd "M-B") 'tddsg/previous-upcase)
  (global-set-key (kbd "M-F") 'tddsg/next-upcase)
  (global-set-key (kbd "M-S") 'save-buffer)
  (global-set-key (kbd "M-s M-s") 'save-buffer)
  (global-set-key (kbd "M-D") 'tddsg/delete-until-next-upcase)
  (global-set-key (kbd "M-K") 'tddsg/delete-until-previous-upcase)
  (global-set-key (kbd "M-C") 'tddsg/toggle-case-current-character)
  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "M--") 'delete-window)
  (global-set-key (kbd "M-+") 'delete-other-windows)
  (global-set-key (kbd "M--") 'delete-window)
  (global-set-key (kbd "M-_") 'split-window-below)
  (global-set-key (kbd "M-|") 'split-window-right)
  (global-set-key (kbd "M-\\") 'sp-splice-sexp)
  (global-set-key (kbd "M-;") 'comment-dwim-2)
  (global-set-key (kbd "M-?") 'company-complete)
  (global-set-key (kbd "C-M-/") 'helm-company)
  (global-set-key (kbd "M-H") 'tddsg/mark-line)
  (global-set-key (kbd "M-h") 'tddsg/mark-paragraph)
  (global-set-key (kbd "M-g |") 'vline-mode)

  (global-set-key (kbd "M-[") 'windmove-left)
  (global-set-key (kbd "M-]") 'windmove-right)
  (global-set-key (kbd "M-=") 'windmove-up)
  (global-set-key (kbd "M-'") 'windmove-down)
  (global-set-key (kbd "H-[") 'windmove-up)
  (global-set-key (kbd "C-]") 'windmove-down)

  (global-set-key (kbd "H-M-[") 'previous-buffer)
  (global-set-key (kbd "C-M-]") 'next-buffer)
  (global-set-key (kbd "H-s-[") 'tddsg/previous-buffer-same-mode)
  (global-set-key (kbd "C-s-]") 'tddsg/next-buffer-same-mode)
  (global-set-key (kbd "C-M-{") 'winner-undo)
  (global-set-key (kbd "C-M-}") 'winner-redo)

  (global-set-key (kbd "M-S-<up>") 'move-text-up)
  (global-set-key (kbd "M-S-<down>") 'move-text-down)
  (global-set-key (kbd "M-S-SPC") 'delete-blank-lines)
  (global-set-key (kbd "M-S-<backspace>") 'tddsg/delete-until-previous-upcase)

  (define-key spacemacs-default-map-root-map (kbd "M-m l") nil)
  (global-set-key (kbd "M-m h g") 'helm-do-grep-ag)
  (global-set-key (kbd "M-m l v") 'visual-line-mode)
  (global-set-key (kbd "M-m w t") 'transpose-frame)
  (global-set-key (kbd "M-m w o") 'flop-frame)
  (global-set-key (kbd "M-m w i") 'flip-frame)
  (global-set-key (kbd "M-m r t") 'purpose-toggle-window-buffer-dedicated)
  (global-set-key (kbd "M-m f C") 'tddsg/save-file-as-and-open)
  (global-set-key (kbd "M-m f o") 'tddsg/open-with)

  (global-set-key (kbd "M-s h .") 'tddsg/highlight-symbol-at-point-or-region)
  (global-set-key (kbd "M-s g") 'engine/search-google)
  (global-set-key (kbd "M-s G") 'browse-url)
  (global-set-key (kbd "M-s t") 'google-translate-at-point)
  (global-set-key (kbd "M-s T") 'google-translate-query-translate)
  (global-set-key (kbd "M-s k") 'evil-search-highlight-persist-remove-all)
  (global-set-key (kbd "M-s b") 'ispell-buffer)
  (global-set-key (kbd "M-s r") 'ispell-region)
  (global-set-key (kbd "M-s w") 'ispell-word)
  (global-set-key (kbd "M-s s") 'ispell-continue)
  (global-set-key (kbd "M-s f") 'flyspell-buffger)
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
  (define-key isearch-mode-map (kbd "C-.") 'tddsg/yank-current-word-isearch)
  (define-key isearch-mode-map (kbd "C-c C-v") 'pdf-isearch-sync-backward)
  (define-key isearch-mode-map (kbd "<f6>") 'pdf-isearch-sync-backward)

  ;; org
  (setq org-reveal-root "file:///home/trungtq/workspace/misc/reveal.js/js/reveal.js")

  ;; swiper
  (with-eval-after-load 'swiper
    (define-key swiper-map (kbd "C-.") 'tddsg/yank-word-minibuffer))

  ;; minibuffer
  (define-key minibuffer-local-map (kbd "C-.") 'tddsg/yank-word-minibuffer)
  (define-key minibuffer-local-map (kbd "C-M-i") nil)

  ;; elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-M-i") nil)
  (define-key emacs-lisp-mode-map (kbd "C-M-q") nil)

  ;; clean-aindent-mode-map
  (define-key clean-aindent-mode--keymap [remap backward-kill-word] nil)

  ;; shell
  (define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
  (define-key shell-mode-map (kbd "C-c d") 'comint-clear-buffer)
  (define-key shell-mode-map (kbd "C-c C-s") 'tddsg/toggle-shell-scroll-output)

  ;; undo tree
  (define-key undo-tree-map (kbd "C-_") nil)
  (define-key undo-tree-map (kbd "M-_") nil)
  (define-key undo-tree-map (kbd "C-/") nil)

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

  ;; LaTeX-mode
  (define-key TeX-mode-map (kbd "<f5>") 'tddsg/latex-compile-project)
  (define-key TeX-mode-map (kbd "<f6>") 'pdf-sync-forward-search)
  (define-key TeX-mode-map (kbd "<f7>") 'tddsg/latex-beamer-compile-frame)
  (define-key TeX-mode-map (kbd "<f8>") 'tddsg/latex-beamer-view-frame)
  (define-key TeX-mode-map (kbd "C-j") nil)
  (define-key TeX-mode-map (kbd "C-M-i") nil)
  (with-eval-after-load 'latex
    (define-key LaTeX-mode-map (kbd "C-M-h") nil)
    (define-key LaTeX-mode-map (kbd "C-j") nil)
    (define-key LaTeX-mode-map (kbd "\"") nil)
    (define-key LaTeX-mode-map (kbd "C-c C-g") nil)
    (define-key LaTeX-mode-map (kbd "$") 'self-insert-command)  ;; temporarily bug fix for smartparens with LaTeX
    (define-key LaTeX-mode-map (kbd "C-c C-M-e") 'LaTeX-delete-environment)
    (define-key LaTeX-mode-map (kbd "C-c C-t") 'TeX-remove-macro)
    (define-key LaTeX-mode-map (kbd "C-o") 'helm-imenu)
    (define-key LaTeX-mode-map (kbd "C-c b") 'helm-bibtex)
    (define-key LaTeX-mode-map (kbd "C-M-o") 'reftex-toc)
    (define-key LaTeX-mode-map (kbd "M-g v") 'latex/font-sans-serif)
    (define-key LaTeX-mode-map (kbd "M-g i") 'latex/font-italic)
    (define-key LaTeX-mode-map (kbd "M-g b") 'latex/font-bold)
    (define-key LaTeX-mode-map (kbd "M-g u") 'latex/font-upright)
    (define-key LaTeX-mode-map (kbd "M-g l") 'latex/font-underline)
    (define-key LaTeX-mode-map (kbd "M-g p") 'latex/font-pack)
    (define-key LaTeX-mode-map (kbd "M-g e") 'latex/font-emphasis)
    (define-key LaTeX-mode-map (kbd "M-g r") 'latex/font-clear)
    (define-key LaTeX-mode-map (kbd "M-g t") 'latex/font-code)
    (define-key LaTeX-mode-map (kbd "M-g s") 'latex/font-small-caps)
    (define-key LaTeX-mode-map (kbd "M-g a") 'latex/font-calligraphic)
    (define-key latex-extra-mode-map (kbd "C-M-f") nil)
    (define-key latex-extra-mode-map (kbd "C-M-b") nil)
    (define-key latex-extra-mode-map (kbd "C-M-n") nil)
    (define-key latex-extra-mode-map (kbd "C-M-p") nil))

  ;; markdown
  (with-eval-after-load 'markdown-mode
    (define-key markdown-mode-map (kbd "M-l") nil)
    (define-key markdown-mode-map (kbd "M-h") nil))

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
  (with-eval-after-load 'python
    (define-key python-mode-map (kbd "C-j") nil))

  ;; pdf-tools
  (with-eval-after-load 'pdf-view
    (define-key pdf-view-mode-map (kbd "s d") 'tddsg/pdf-view-disable-auto-slice)
    (define-key pdf-view-mode-map (kbd "s e") 'tddsg/pdf-view-enable-auto-slice)
    (define-key pdf-view-mode-map (kbd "M-s o") 'pdf-occur)
    (define-key pdf-view-mode-map (kbd "C-<home>") 'pdf-view-first-page)
    (define-key pdf-view-mode-map (kbd "C-<end>") 'pdf-view-last-page)
    (define-key pdf-view-mode-map (kbd "M-w") 'tddsg/pdf-view-kill-ring-save)
    (define-key pdf-view-mode-map (kbd "RET") 'pdf-view-scroll-up-or-next-page)
    (define-key pdf-view-mode-map (kbd "C-<mouse-1>") 'pdf-sync-backward-search-mouse)
    (define-key pdf-view-mode-map (kbd "<mouse-8>") 'pdf-history-backward)
    (define-key pdf-view-mode-map (kbd "<mouse-9>") 'pdf-history-forward))

  ;; doc-view
  (with-eval-after-load 'doc-view-mode
    (define-key doc-view-mode-map (kbd "RET") 'doc-view-scroll-up-or-next-page))

  ;; flyspell
  (define-key flyspell-mode-map (kbd "C-;") nil)
  (define-key flyspell-mode-map (kbd "C-,") nil)
  (define-key flyspell-mode-map (kbd "C-M-i") nil)

  ;; dired mode
  (define-key dired-mode-map (kbd "M-+") 'nil)
  (define-key dired-mode-map (kbd "<backspace>") 'dired-up-directory)

  ;; smartparens
  (define-key smartparens-mode-map (kbd "M-s") nil)
  (define-key smartparens-mode-map (kbd "M-S") nil)

  ;; evil mode
  (define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward)
  (define-key evil-motion-state-map (kbd "C-^") nil)
  (define-key evil-motion-state-map (kbd "C-_") nil)
  (define-key evil-emacs-state-map (kbd "C-z") nil)

  ;; ggtags
  (with-eval-after-load 'ggtags
    (define-key ggtags-mode-map (kbd "M-]") nil)
    (define-key ggtags-mode-map (kbd "M-.") 'tddsg/find-definition)
    (define-key ggtags-mode-map (kbd "M-,") 'tddsg/find-references)
    (define-key ggtags-mode-map (kbd "C-M-,") 'rtags-find-references)
    (define-key ggtags-mode-map (kbd "C-c M-r") 'tddsg/find-references))

  ;; irony
  (with-eval-after-load 'irony
    (define-key irony-mode-map (kbd "C-c C-t") 'irony-get-type))

  ;; cc
  (with-eval-after-load 'cc-mode
    ;; fix to make smartparens can enclose ()
    (define-key c-mode-base-map (kbd "(") 'self-insert-command)
    (define-key c-mode-base-map (kbd "C-M-q") nil)
    (define-key c-mode-base-map (kbd "C-M-j") nil)
    (define-key c-mode-map (kbd "C-M-j") nil)
    (define-key c++-mode-map (kbd "C-M-j") nil))

  ;; company mode
  (define-key company-active-map (kbd "M-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-.") 'company-show-location)

  ;; flycheck
  (with-eval-after-load 'flycheck-mode
    (define-key flycheck-mode-map (kbd "M-g M-n") 'flycheck-next-error)
    (define-key flycheck-mode-map (kbd "M-g M-p") 'flycheck-previous-error)
    (define-key flycheck-mode-map (kbd "M-g M-f") 'flycheck-first-error)))


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
   '(;; dired
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
     ;; vline
     (vline ((t (:background "#34424D"))))
     ;; font
     (font-lock-keyword-face ((t (:inherit bold :foreground "#4f97d7"))))
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
   '(sp-wrap-tag-overlay-face ((t nil)))
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
       :inverse-video nil :box nil :underline t)))))

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
   '(LaTeX-begin-regexp "begin\\b\\|\\[\\b\\|If\\b\\|For")
   '(LaTeX-command "latex --synctex=1")
   '(LaTeX-end-regexp "end\\b\\|\\]\\b\\|EndIf\\b\\|EndFor")
   '(LaTeX-font-list
     (quote
      ((100 "" "" t)
       (117 "\\textup{" "}")
       (108 "\\underline{" "}")
       (116 "\\texttt{" "}" "\\mathtt{" "}")
       (115 "\\textsl{" "}" "\\mathbb{" "}")
       (114 "\\textrm{" "}" "\\mathrm{" "}")
       (110 "\\textnormal{" "}" "\\mathnormal{" "}")
       (109 "\\textmd{" "}")
       (112 "\\,{" "}\\,")
       (105 "\\textit{" "}" "\\mathit{" "}")
       (102 "\\textsf{" "}" "\\mathsf{" "}")
       (101 "\\emph{" "}")
       (99 "\\textsc{" "}")
       (98 "\\textbf{" "}" "\\mathbf{" "}")
       (97 "" "" "\\mathcal{" "}")
       (100 "" "" t)
       (1 "" "" "\\mathcal{" "}")
       (2 "\\textbf{" "}" "\\mathbf{" "}")
       (3 "\\textsc{" "}")
       (5 "\\emph{" "}")
       (6 "\\textsf{" "}" "\\mathsf{" "}")
       (9 "\\textit{" "}" "\\mathit{" "}")
       (16 "\\,{" "}\\,")
       (13 "\\textmd{" "}")
       (14 "\\textnormal{" "}" "\\mathnormal{" "}")
       (18 "\\textrm{" "}" "\\mathrm{" "}")
       (19 "\\textsl{" "}" "\\mathbb{" "}")
       (20 "\\texttt{" "}" "\\mathtt{" "}")
       (21 "\\textup{" "}")
       (12 "\\underline{" "}")
       (4 "" "" t))))
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

;;; Override SPACELINE to show file path in mode-line
(defmacro with-face (str &rest properties)
  `(propertize ,str
               'face (list ,@properties)
               'mouse-face 'mode-line-highlight
               'help-echo (concat "Buffer name\n"
                                  "mouse-1: Previous buffer\n"
                                  "mouse-3: Next buffer")
               'local-map (let ((map (make-sparse-keymap)))
               			        (define-key map [mode-line mouse-1]
                              'mode-line-previous-buffer)
               			        (define-key map [mode-line mouse-3]
                              'mode-line-next-buffer)
               			        map)))

(defun shorten-left (name len)
  (if (> (length name) len)
      (concat (apply #'propertize "[...]"
                     (text-properties-at 1 name))
              (substring name (- (- len 10)) nil))
    name))

(defun shorten-right (name len)
  (if (> (length name) len)
      (concat (substring name 0 (- len 10) )
              (apply #'propertize "[...]"
                     (text-properties-at (- (length name) 1) name)))
    name))

(spaceline-define-segment buffer-id
  "Name of buffer."
  (let* ((max-length (- (window-body-width) 10))
         (display-length (max 45 (- max-length 30)))
         (path (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 ""))
         (main-face (if active
                        'mode-line-buffer-id
                      'mode-line-buffer-id-inactive))
         (buffer-id (if (and (buffer-file-name) (< (length path) max-length))
                        (let* ((file-name (with-face
                                           (file-name-nondirectory path)
                                           main-face))
                               (dir-name (with-face
                                          (file-name-directory path)
                                          :foreground "gray"))
                               (buffer-name (concat dir-name file-name)))
                          (shorten-left buffer-name display-length))
                      (shorten-right (with-face
                                      (powerline-buffer-id)
                                      main-face)
                                     display-length))))
    (s-trim buffer-id)))

;;; tddsg-init.el ends here
