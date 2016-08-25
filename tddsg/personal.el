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

;; get last message of the message buffer
(defun tddsg-last-message ()
  (save-excursion
    (set-buffer "*Messages*")
    (save-excursion
      (forward-line)
      (backward-char)
      (let ((end (point)))
        (forward-line 0)
        (buffer-substring-no-properties (point) end)))))

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

(defun tddsg-yank-current-word-to-isearch-buffer ()
  "Pull current word from buffer into search string."
  (interactive)
  (save-excursion
    (skip-syntax-backward "w_")
    (isearch-yank-internal
     (lambda ()
       (skip-syntax-forward "w_")
       (point)))))

(defun tddsg-current-line-empty-p ()
  (interactive)
  (string-blank-p (buffer-substring (line-beginning-position)
                                    (line-end-position))))

(defun tddsg-jump-to-same-indent (direction)
  "Jump to the line of the same indent, ignore the blank lines"
  (interactive "P")
  (let ((start-indent (current-indentation)))
    (while
        (and (not (bobp))
             (zerop (forward-line direction))
             (or
              (not (= (current-indentation) start-indent))
              (tddsg-current-line-empty-p))))
    (back-to-indentation)))

(defun tddsg-jump-to-same-indent-forward ()
  (interactive)
  (tddsg-jump-to-same-indent 1)
  (back-to-indentation))

(defun tddsg-jump-to-same-indent-backward ()
  (interactive)
  (tddsg-jump-to-same-indent -1)
  (back-to-indentation))

(require 'god-mode)
(defun tddsg-update-cursor ()
  (if god-local-mode
      (set-cursor-color "purple")
    (set-cursor-color "lime green")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keys that won't be overriden will be set in tddsg-mode
(defvar tddsg-mode-map (make-keymap) "tddsg-mode keymap.")

(define-key tddsg-mode-map (kbd "<home>") 'crux-move-beginning-of-line)
(define-key tddsg-mode-map (kbd "<detete>") 'delete-forward-char)
(define-key tddsg-mode-map (kbd "C-<left>") 'left-word)
(define-key tddsg-mode-map (kbd "C-<right>") 'right-word)
(define-key tddsg-mode-map (kbd "M-<down>") '(lambda () (interactive) (next-line 5)))
(define-key tddsg-mode-map (kbd "M-<up>") '(lambda () (interactive) (previous-line 5)))
(define-key tddsg-mode-map (kbd "M-<left>") '(lambda () (interactive) (left-char 5)))
(define-key tddsg-mode-map (kbd "M-<right>") '(lambda () (interactive) (right-char 5)))

(define-key tddsg-mode-map (kbd "C-_") 'join-line)
(define-key tddsg-mode-map (kbd "M-;") 'comment-dwim-2)
(define-key tddsg-mode-map (kbd "M-k") 'sp-kill-sexp)
(define-key tddsg-mode-map (kbd "C-M-k") 'tddsg-kill-line-backwards)
(define-key tddsg-mode-map (kbd "S-<backspace>") 'crux-kill-whole-line)
(define-key tddsg-mode-map (kbd "C-S-<backspace>") 'crux-kill-whole-line)
(define-key tddsg-mode-map (kbd "M-s") 'sp-splice-sexp)
(define-key tddsg-mode-map (kbd "M-S") 'isearch-forward-symbol-at-point)
(define-key tddsg-mode-map (kbd "C-s") 'isearch-forward)
(define-key tddsg-mode-map (kbd "M-H") 'tddsg-select-current-line)
(define-key tddsg-mode-map (kbd "M-h") 'mark-paragraph)
(define-key tddsg-mode-map (kbd "C-x _") 'shrink-window)
(define-key tddsg-mode-map (kbd "C-x m") 'monky-status)
(define-key tddsg-mode-map (kbd "C-x g") 'magit-status)
(define-key tddsg-mode-map (kbd "C-x G") 'magit-diff)
(define-key tddsg-mode-map (kbd "C-x f") 'helm-find)
(define-key tddsg-mode-map (kbd "<escape>") 'god-local-mode)
(define-key tddsg-mode-map (kbd "C-z") 'god-local-mode)
(define-key tddsg-mode-map (kbd "M-?") 'company-complete)
(define-key tddsg-mode-map (kbd "M-/") 'hippie-expand)
(define-key tddsg-mode-map (kbd "M-[") 'helm-company)
(define-key tddsg-mode-map (kbd "M-]") 'helm-dabbrev)
(define-key tddsg-mode-map (kbd "M-m") 'tddsg-set-default-mode-line)

(define-key tddsg-mode-map (kbd "C-c C-\\") 'goto-last-change)
(define-key tddsg-mode-map (kbd "C-c C-\\") 'goto-last-change)
(define-key tddsg-mode-map (kbd "C-c C-w") 'tddsg-save-file-as-and-open-file)
(define-key tddsg-mode-map (kbd "C-c C-r") 'eval-region)
(define-key tddsg-mode-map (kbd "C-c C-f") 'projectile-find-file)
(define-key tddsg-mode-map (kbd "C-c C-g") 'helm-do-grep-ag)
(define-key tddsg-mode-map (kbd "C-c C-i") 'helm-imenu-anywhere)
(define-key tddsg-mode-map (kbd "C-c C-s") 'dictionary-search)
(define-key tddsg-mode-map (kbd "C-c M-m") 'tddsg-shell-current-window)

(define-key tddsg-mode-map (kbd "C-c f") 'helm-recentf)
(define-key tddsg-mode-map (kbd "C-c m") 'tddsg-shell-other-window)
(define-key tddsg-mode-map (kbd "C-c e") 'flyspell-mode)
(define-key tddsg-mode-map (kbd "C-c o") 'helm-occur)
(define-key tddsg-mode-map (kbd "C-c i") 'helm-semantic-or-imenu)
(define-key tddsg-mode-map (kbd "C-c u") 'crux-view-url)
(define-key tddsg-mode-map (kbd "C-c k") 'crux-kill-other-buffers)
(define-key tddsg-mode-map (kbd "C-c g") 'helm-projectile-grep)
(define-key tddsg-mode-map (kbd "C-c s") 'flyspell-mode)
(define-key tddsg-mode-map (kbd "C-c r") 'projectile-replace)
(define-key tddsg-mode-map (kbd "C-c w") 'ace-window)
(define-key tddsg-mode-map (kbd "C-c q") 'ace-delete-window)
(define-key tddsg-mode-map (kbd "C-c |") 'vline-mode)
(define-key tddsg-mode-map (kbd "C-c _") 'global-hl-line-mode)
(define-key tddsg-mode-map (kbd "C-c t") 'transpose-frame)
(define-key tddsg-mode-map (kbd "C-c v") 'visual-line-mode)
(define-key tddsg-mode-map (kbd "C-c )") 'check-parens)
(define-key tddsg-mode-map (kbd "C-c y") 'yafolding-mode)
(define-key tddsg-mode-map (kbd "C-c C-SPC") 'tddsg-unpop-to-mark-command)
(define-key tddsg-mode-map (kbd "C-c SPC") 'helm-all-mark-rings)

(define-key tddsg-mode-map (kbd "C-c R") 'crux-rename-file-and-buffer)
(define-key tddsg-mode-map (kbd "C-c D") 'crux-delete-file-and-buffer)

;; moving the cursor around windows (requiring windmove package)
(define-key tddsg-mode-map (kbd "S-<left>") 'windmove-left)
(define-key tddsg-mode-map (kbd "S-<right>") 'windmove-right)
(define-key tddsg-mode-map (kbd "S-<up>") 'windmove-up)
(define-key tddsg-mode-map (kbd "S-<down>") 'windmove-down)

;; moving buffer around (requiring buffer-move package)
(define-key tddsg-mode-map (kbd "C-s-S-<left>") 'buf-move-left)
(define-key tddsg-mode-map (kbd "C-s-S-<right>") 'buf-move-right)
(define-key tddsg-mode-map (kbd "C-s-S-<up>") 'buf-move-up)
(define-key tddsg-mode-map (kbd "C-s-S-<down>") 'buf-move-down)
(define-key tddsg-mode-map (kbd "C-c c J") 'buf-move-left)
(define-key tddsg-mode-map (kbd "C-c c L") 'buf-move-right)
(define-key tddsg-mode-map (kbd "C-c c I") 'buf-move-up)
(define-key tddsg-mode-map (kbd "C-c c K") 'buf-move-down)

;; moving buffer around (requiring buffer-clone package)
(define-key tddsg-mode-map (kbd "s-S-<left>") 'buf-clone-left)
(define-key tddsg-mode-map (kbd "s-S-<right>") 'buf-clone-right)
(define-key tddsg-mode-map (kbd "s-S-<up>") 'buf-clone-up)
(define-key tddsg-mode-map (kbd "s-S-<down>") 'buf-clone-down)
(define-key tddsg-mode-map (kbd "C-c c j") 'buf-clone-left)
(define-key tddsg-mode-map (kbd "C-c c l") 'buf-clone-right)
(define-key tddsg-mode-map (kbd "C-c c i") 'buf-clone-up)
(define-key tddsg-mode-map (kbd "C-c c k") 'buf-clone-down)

;; define key for other minor mode
(define-key isearch-mode-map (kbd "C-.") 'tddsg-yank-current-word-to-isearch-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Various configuration

(setq behave-like-something-actually-usable-by-humans t)


;; automatically setting mark for certain commands
(setq global-mark-ring-max 1000
      mark-ring-max 200)
(defadvice find-file (before set-mark activate) (tddsg-set-mark))
(defadvice isearch-update (before set-mark activate) (tddsg-set-mark))
(defadvice beginning-of-buffer (before set-mark activate) (tddsg-set-mark))
(defadvice end-of-buffer (before set-mark activate) (tddsg-set-mark))
(defadvice merlin-locate (before set-mark activate) (tddsg-set-mark))

;; visual interface setting
(disable-theme 'zenburn)
(load-theme 'leuven t)                              ; beautiful theme
(setq text-scale-mode-step 1.1)                     ; scale changing font size
(set-scroll-bar-mode 'right)                        ; scroll-bar to the right
(setq frame-title-format                            ; frame title
      '(""
        invocation-name
        " - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; mode paragraph setting
(setq paragraph-separate "[ \t\f]*$"
      paragraph-start "\f\\|[ \t]*$")

;; mode editing setting
(delete-selection-mode t)                           ; delete selection by keypress
(setq require-final-newline t)                      ; newline at end of file
(defadvice newline                                  ; indent after new line
    (after newline-after activate)
  (indent-according-to-mode))

;; coding setting
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; some Emacs threshold
(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 10000)


;; visual-line mode setting
(setq truncate-lines 1
      visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(defun activate-visual-line-mode ()
  (interactive)
  (visual-line-mode))
(add-hook 'prog-mode-hook 'activate-visual-line-mode)
(add-hook 'text-mode-hook 'activate-visual-line-mode)
(add-hook 'shell-mode-hook 'activate-visual-line-mode)
(add-hook 'compilation-mode-hook 'activate-visual-line-mode)
(add-hook 'compilation-minor-hook 'activate-visual-line-mode)





(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

(use-package buffer-move
  :ensure t)

(use-package buffer-clone
  :load-path "personal/modules/")

(use-package transpose-frame
  :ensure t)

(use-package vline
  :ensure t)

(use-package hydra
  :ensure t)

(use-package langtool
  :ensure t
  :config
  (setq langtool-default-language "en-US")
  (setq langtool-language-tool-jar
        "/home/trungtq/Programs/LanguageTool-2.6/languagetool-commandline.jar"))

(use-package dictionary
  :ensure t
  :config
  (setq dictionary-use-single-buffer t))

(use-package comment-dwim-2
  :ensure t)

(use-package dired
  :bind (("C-c o" . crux-open-with))
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)
  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)
  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))

(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (defadvice anzu-query-replace (around wrap-query-replace activate)
    (save-excursion
      (let ((start (point)))
        ad-do-it
        (goto-char (point-min))
        ad-do-it)))
  (global-anzu-mode))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))


(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  ;; default of prelude
  ;; (setq whitespace-style '(face tabs empty trailing lines-tail)) ;;
  (setq whitespace-style '(face tabs trailing)))



(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :ensure t)


(use-package company
  :ensure t
  :config
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
    (define-key company-active-map (kbd "M-.") 'company-show-location)
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous))
  (setq company-idle-delay 200)         ;; set delay time by default
  (global-company-mode))

(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

(use-package imenu-anywhere
  :ensure t)

(use-package synonyms
  :ensure t)

(use-package thesaurus
  :ensure t
  :config
  (setq thesaurus-bhl-api-key "ae85c3f7512736eafebfb13b4da713c8"))

(use-package flyspell
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra")
        ispell-dictionary "english"
        prelude-flyspell nil)
  (add-hook 'text-mode-hook #'flyspell-mode))

(use-package flycheck
  :ensure t
  :config
  ;; checker for text-mode
  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message (one-or-more not-newline)
                       (zero-or-more "\n" (any " ")
                                     (one-or-more not-newline)))
              line-end))
    :modes (text-mode markdown-mode gfm-mode))
  (add-to-list 'flycheck-checkers 'proselint)
  ;; hook to flycheck
  (defun disable-flycheck-mode ()
    (interactive)
    (flycheck-mode -1))
  (add-hook 'markdown-mode-hook #'flycheck-mode)
  (add-hook 'text-mode-hook #'flycheck-mode)
  (add-hook 'prog-mode-hook 'disable-flycheck-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package super-save
  :ensure t
  :config
  (super-save-mode +1))

(use-package crux
  :ensure t
  :bind (("M-o" . crux-smart-open-line)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-M-z" . crux-indent-defun)
         ("C-c e" . crux-eval-and-replace)
         ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
         ("C-c I" . crux-find-user-init-file)
         ("C-c S" . crux-find-shell-init-file)
         ("C-^" . crux-top-join-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ([remap kill-whole-line] . crux-kill-whole-line)))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(use-package undo-tree
  :ensure t
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode -1))

(use-package column-marker
  :ensure t
  :config
  (defun activate-column-marker ()
    (interactive)
    (column-marker-1 80))
  (add-hook 'prog-mode-hook 'activate-column-marker)
  (add-hook 'text-mode-hook 'activate-column-marker))

(use-package key-chord
  :ensure t
  :config
  (setq key-chord-one-key-delay 0.18
        key-chord-two-key-delay 0.1)
  ;; reassign key-chords
  (key-chord-define-global ",." 'helm-mini)
  (key-chord-define-global "xz" 'helm-mini)
  (key-chord-define-global "xz" 'helm-mini)
  (key-chord-define-global "jj" 'avy-goto-word-1)
  (key-chord-define-global "jl" 'avy-goto-word-1)
  (key-chord-define-global "jk" 'previous-buffer)
  (key-chord-define-global "kl" 'next-buffer)
  (key-chord-define-global "ji" 'indent-according-to-mode)
  (key-chord-define-global "JK" 'windmove-left)
  (key-chord-define-global "KL" 'windmove-right)
  (key-chord-define-global "JI" 'windmove-up)
  (key-chord-define-global "IL" 'windmove-down))

(use-package make-mode
  :mode ("\\Makefile\\'" . makefile-mode)
  :ensure t
  :config
  (defun my-makefile-mode ()
    (setq indent-line-function 'indent-relative))
  (add-hook 'makefile-mode-hook 'my-makefile-mode))


(use-package merlin-imenu
  :load-path "personal/modules/")

(use-package smartparens-ocaml
  :load-path "personal/modules/")

(use-package org
  :ensure t
  :config
  (setq org-agenda-files
        (list "~/workspace/html/tddsg/orgs/work.org"
              "~/workspace/html/tddsg/orgs/school.org"
              "~/workspace/html/tddsg/orgs/home.org"))
  (defun my-org-mode ()
    (org-indent-mode t)
    (setq indent-line-function 'indent-relative))
  (add-hook 'org-mode-hook 'my-org-mode))

(use-package songbird-mode
  :load-path "personal/modules/"
  :mode (("\\.slk\\'" . songbird-mode)
         ("\\.ss\\'" . songbird-mode)
         ("\\.sb\\'" . songbird-mode))
  :config
  (defun my-songbird-hook ()
    ;; customize syntax table for slurping/barfing parentheses
    (dolist (symbol (list ?. ?, ?\; ?: ?+ ?- ?@ ?! ?> ?<))
      (modify-syntax-entry symbol "'" songbird-syntax-table)))
  (add-hook 'songbird-mode-hook 'my-songbird-hook 'append))

(use-package lisp
  :mode (("\\.smt2\\'" . lisp-mode)
         ("\\.smt\\'" . lisp-mode)))

;;; Finally, provide tddsg-minor-mode
(define-minor-mode tddsg-mode
  "A minor mode so that my key settings override annoying major modes."
  t
  " tddsg"
  :keymap 'tddsg-mode-map)
(tddsg-mode 1)

;;; minibuffer mode setting
(defun my-minibuffer-setup-hook ()
  (tddsg-mode 0)
  (local-set-key (kbd "C-.") 'tddsg-yank-current-word-to-minibuffer))
  

(defun my-minibuffer-exit-hook ()
  (tddsg-mode 1))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook 'my-minibuffer-exit-hook)
