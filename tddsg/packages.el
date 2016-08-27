;;; packages.el --- tddsg layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: trungtq <trungtq@polaris>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `tddsg-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `tddsg/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `tddsg/pre-init-PACKAGE' and/or
;;   `tddsg/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst tddsg-packages
  '(comment-dwim-2
    tuareg
    auctex
    org
    latex-extra
    shell
    ace-popup-menu
    smartparens
    yasnippet
    hi-lock
    helm
    projectile
    expand-region
    goto-chg
    paren
    autorevert
    windmove
    transpose-frame
    key-chord
    super-save
    flyspell
    whitespace
    move-text
    company
    anzu
    dictionary
    langtool
    imenu-anywhere
    buffer-move
    diminish
    vline
    crux
    undo-tree
    column-marker
    god-mode
    (songbird :location local)
    (buffer-clone :location local)
    )
  "The list of Lisp packages required by the tddsg layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SOME FUNCTIONS

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

;; get the closet parent folder containing a Makefile
(cl-defun tddsg-get-closest-build-path (&optional (file "Makefile"))
  "Get path of the closest parent folder that contains a Makefile"
  (let ((root (expand-file-name "/"))) ; the win32 must reconsider it
    (loop
     for d = default-directory then (expand-file-name ".." d)
     if (file-exists-p (expand-file-name file d))
     return d
     if (equal d root)
     return nil)))

;; select the current line
(defun tddsg-mark-line ()
  "Select current line"
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

;; mark ring setting
(defun tddsg-set-mark ()
  (interactive)
  (push-mark (point) t nil))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SOME KEYBINDINGS

(global-set-key (kbd "<home>") 'spacemacs/smart-move-beginning-of-line)
(global-set-key (kbd "<detete>") 'delete-forward-char)
(global-set-key (kbd "C-S-<backspace>") 'kill-whole-line)
(global-set-key (kbd "C-<left>") 'left-word)
(global-set-key (kbd "C-<right>") 'right-word)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)


(global-set-key (kbd "M-H") 'tddsg-mark-line)
(global-set-key (kbd "C-x _") 'shrink-window)
(global-set-key (kbd "C-x m") 'monky-status)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x G") 'magit-diff)
(global-set-key (kbd "M-s p") 'check-parens)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c C-w") 'tddsg-save-file-as-and-open-file)
(global-set-key (kbd "C-c C-SPC") 'tddsg-unpop-to-mark-command)

(define-key isearch-mode-map (kbd "C-.") 'tddsg-yank-current-word-to-isearch-buffer)
(define-key minibuffer-local-map (kbd "C-.") 'tddsg-yank-current-word-to-minibuffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONFIGS

;; visual interface setting
(global-linum-mode 1)
(setq-default fill-column 80)
(setq text-scale-mode-step 1.1)                     ; scale changing font size
(setq frame-title-format                            ; frame title
      '("" invocation-name " - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))

;; mode paragraph setting
(setq paragraph-separate "[ \t\f]*$"
      paragraph-start "\f\\|[ \t]*$")

;; mode electric-pair
(electric-pair-mode t)

;; automatically setting mark for certain commands
(setq global-mark-ring-max 1000
      mark-ring-max 200)
(defadvice find-file (before set-mark activate) (tddsg-set-mark))
(defadvice isearch-update (before set-mark activate) (tddsg-set-mark))
(defadvice beginning-of-buffer (before set-mark activate) (tddsg-set-mark))
(defadvice end-of-buffer (before set-mark activate) (tddsg-set-mark))
(defadvice merlin-locate (before set-mark activate) (tddsg-set-mark))

;; mode editing setting
(delete-selection-mode t)                           ; delete selection by keypress
(setq require-final-newline t)                      ; newline at end of file
(defadvice newline                                  ; indent after new line
    (after newline-after activate)
  (indent-according-to-mode))

;; some Emacs threshold
(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 10000)

;; mode-line setting
(setq powerline-default-separator 'wave)

;; fix page-up/page-down problems in smooth-scroll
(setq scroll-conservatively 101
      scroll-margin 3
      scroll-preserve-screen-position 't)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CUSTOM THEMES

(defcustom theme-overrides nil
  "Association list of override faces to set for different custom themes.")

(defun alist-set (alist-symbol key value)
  "Set VALUE of a KEY in ALIST-SYMBOL."
  (set alist-symbol
        (cons (list key value) (assq-delete-all key (eval alist-symbol)))))

;; override some settings of the leuven theme
(alist-set 'theme-overrides
           'leuven
           '((cursor ((t (:background "lime green"))))
             (font-latex-bold-face ((t (:foreground "gray26" :weight bold))))
             (font-latex-math-face ((t (:foreground "DeepSkyBlue4"))))
             (font-latex-sedate-face ((t (:foreground "green4"))))
             (font-latex-subscript-face ((t (:height 0.96))))
             (font-latex-superscript-face ((t (:height 0.96))))
             (font-latex-verbatim-face ((t (:inherit nil :background "white" :foreground "light coral"))))
             (font-lock-constant-face ((t (:foreground "dark goldenrod"))))
             (font-lock-doc-face ((t (:foreground "#8959a8"))))
             (font-lock-function-name-face ((t (:foreground "dark orchid" :weight normal))))
             (font-lock-keyword-face ((t (:foreground "blue" :weight normal))))
             (font-lock-string-face ((t (:foreground "#3e999f"))))
             (font-lock-type-face ((t (:foreground "MediumOrchid4" :weight normal))))
             (font-lock-variable-name-face ((t (:foreground "DodgerBlue3" :weight normal))))
             (company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
             (company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
             (powerline-active1 ((t (:inherit mode-line :background "#163365"))))))

(defun tddsg-override-theme ()
  (dolist (theme-settings theme-overrides)
    (let ((theme (car theme-settings))
          (faces (cadr theme-settings)))
      (if (member theme custom-enabled-themes)
          (dolist (face faces)
            (custom-theme-set-faces theme face))))))

;; load the theme
(tddsg-override-theme)

;; and defadvice load-theme function
(defadvice load-theme (after theme-set-overrides activate)
  "Set override faces for different custom themes."
  (tddsg-override-theme))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INIT PACKAGES

;;; configuration for comment-dwim-2
(defun tddsg/init-comment-dwim-2 ()
  (global-set-key (kbd "M-;") 'comment-dwim-2))

;;; configuration for ace-popup-menu
(defun tddsg/init-ace-popup-menu ()
  (ace-popup-menu-mode 1))

;;; configuration for super-save
(defun tddsg/init-super-save ()
  (super-save-mode 1))

;;; configuration for ace-popup-menu
(defun tddsg/init-transpose-frame ()
  (global-set-key (kbd "M-m w t") 'transpose-frame))

;;; configuration for tuareg
(defun tddsg/post-init-tuareg ()
  ;; setup environments
  (use-package merlin-imenu :load-path "private/tddsg/")
  (use-package smartparens-ocaml :load-path "private/tddsg/")
  (dolist (var (car (read-from-string
                     (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var)))
  (setq exec-path (split-string (getenv "PATH") path-separator)
        opam-share (substring (shell-command-to-string
                               "opam config var share 2> /dev/null") 0 -1))
  ;; fix syntax highlight of cs Prelude for OCaml
  (font-lock-add-keywords
   'tuareg-mode
   '(("\\<\\(let\\|in\\|open\\|module\\|rec\\)\\>" . font-lock-keyword-face)
     ("\\<\\(with\\|and\\|type\\|include\\)\\>" . font-lock-keyword-face)
     ("\\<\\(struct\\|mutable\\|begin\\|end\\)\\>" . font-lock-keyword-face)
     ("\\<\\(sig\\|val\\|functor\\|raise\\)\\>" . font-lock-keyword-face)
     ("\\<\\(class\\|object\\|method\\|inherit\\)\\>" . font-lock-keyword-face)
     ("\\<\\(external\\)\\>" . font-lock-keyword-face)
     ("\\<[A-Z][A-Za-z0-9_']*\\>" . font-lock-constant-face)))
  (defun disable-ocp-indent ()
    (interactive)
    (setq indent-line-function 'indent-relative))
  (defun enable-ocp-indent ()
    (interactive)
    (setq indent-line-function 'ocp-indent-line))
  (defun my-tuareg-hook ()
    (interactive)
    (merlin-mode)
    (merlin-use-merlin-imenu)
    (enable-ocp-indent)
    (setq indent-line-function 'ocp-indent-line)   ;; ocp-indent
    ;; customize syntax table for forward/backward slurping/barfing sexp
    (dolist (symbol (list ?, ?\; ?: ?+ ?- ?/ ?@ ?! ?> ?<))
      (modify-syntax-entry symbol "'" tuareg-mode-syntax-table))
    ;; customize compilation
    (local-set-key (kbd "C-c C-c") 'compile)
    (key-chord-define-local "xc" 'compile)
    (setq compile-command (format "make -k -C %s"
                                  (tddsg-get-closest-build-path))))
  ;; append this hook to the last of the hook function list
  (add-hook 'tuareg-mode-hook 'my-tuareg-hook 'append))

;;; configuration for auctex
(defun tddsg/post-init-auctex ()
   ;; (add-to-list 'TeX-command-list '("Make" "make" TeX-run-compile nil t))
  (custom-set-variables
   '(TeX-save-query nil)
   '(TeX-source-correlate-method (quote synctex))
   '(TeX-source-correlate-mode t)
   '(TeX-source-correlate-start-server t)
   '(TeX-view-program-list (quote (("Okular" "okular --unique %o#src:%n%b"))))
   '(TeX-view-program-selection
     (quote ((engine-omega "dvips and gv")
             (output-dvi "xdvi")
             (output-pdf "Okular")
             (output-html "xdg-open")))))
  (defun my-latex-hook ()
    (setq TeX-newline-function 'newline-and-indent
          paragraph-separate "[ \t\f]*$"
          paragraph-start "\f\\|[ \t]*$")
    (require 'smartparens-latex))
  (add-hook 'LaTeX-mode-hook 'my-latex-hook 'append)
  (add-hook 'tex-mode-hook 'my-latex-hook 'append))

(defun tddsg/post-init-shell ()
  (global-set-key (kbd "C-c m") 'tddsg-shell-other-window)
  (global-set-key (kbd "C-c M-m") 'tddsg-shell-current-window)
  (defun my-shell-hook ()
    (local-set-key (kbd "C-j") 'newline))
  (add-hook 'shell-mode-hook 'my-shell-hook))

(defun tddsg/post-init-smartparens ()
  (sp-use-paredit-bindings)
  (define-key smartparens-mode-map (kbd "C-<left>") nil)
  (define-key smartparens-mode-map (kbd "C-<right>") nil)
  (define-key smartparens-mode-map (kbd "M-s") nil)
  (define-key smartparens-mode-map (kbd "M-s s") 'sp-splice-sexp)
  (smartparens-global-mode t))

(defun tddsg/post-init-yasnippet ()
  (yas-global-mode  t))

(defun tddsg/post-init-hi-lock ()
  (global-hi-lock-mode 1))

(defun tddsg/post-init-helm ()
  (global-set-key (kbd "M-[") 'helm-company)
  (global-set-key (kbd "M-]") 'helm-dabbrev)
  (global-set-key (kbd "M-m h o") 'helm-occur)
  (global-set-key (kbd "M-m h s") 'helm-semantic-or-imenu))

(defun tddsg/post-init-projectile ()
  (projectile-global-mode 1))

(defun tddsg/post-init-expand-region ()
  (global-set-key (kbd "C-=") 'er/expand-region))

(defun tddsg/init-goto-chg ()
  (global-set-key (kbd "C-c C-\\") 'goto-last-change))

(defun tddsg/init-autorevert ()
  (global-auto-revert-mode t))

(defun tddsg/init-paren ()
  (show-paren-mode t))

(defun tddsg/init-windmove ()
  (windmove-default-keybindings)
  (global-set-key (kbd "S-<left>") 'windmove-left)
  (global-set-key (kbd "S-<right>") 'windmove-right)
  (global-set-key (kbd "S-<up>") 'windmove-up)
  (global-set-key (kbd "S-<down>") 'windmove-down))

(defun tddsg/init-key-chord ()
  (require 'key-chord)
  (key-chord-mode 1)
  (setq key-chord-one-key-delay 0.18
        key-chord-two-key-delay 0.1)
  ;; reassign key-chords
  (key-chord-define-global ",." 'helm-mini)
  (key-chord-define-global "xz" 'helm-mini)
  (key-chord-define-global "JK" 'previous-buffer)
  (key-chord-define-global "KL" 'next-buffer)
  (key-chord-define-global "ji" 'indent-according-to-mode)
  (key-chord-define-global "jk" 'avy-goto-word-1)
  (key-chord-define-global "jj" 'avy-goto-char-2)
  (key-chord-define-global "jl" 'avy-goto-line)
  (key-chord-define-global "IL" 'windmove-down))

(defun tddsg/post-init-flyspell ()
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra")
        ispell-dictionary "english"
        prelude-flyspell nil)
  (global-set-key (kbd "M-m S s") 'flyspell-mode)
  (add-hook 'text-mode-hook #'flyspell-mode))

(defun tddsg/post-init-whitespace ()
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face tabs trailing)))

(defun tddsg/post-init-move-text ()
  (global-set-key (kbd "M-S-<up>") 'move-text-up)
  (global-set-key (kbd "M-S-<down>") 'move-text-down))

(defun tddsg/post-init-buffer-move ()
  ;; buffer-move
  (global-set-key (kbd "C-s-S-<left>") 'buf-move-left)
  (global-set-key (kbd "C-s-S-<right>") 'buf-move-right)
  (global-set-key (kbd "C-s-S-<up>") 'buf-move-up)
  (global-set-key (kbd "C-s-S-<down>") 'buf-move-down))

(defun tddsg/post-init-company ()
  (require 'company)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-.") 'company-show-location)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (global-set-key (kbd "M-?") 'company-complete)
  ;; (setq company-idle-delay 200)         ;; set delay time by default
  (global-company-mode))

(defun tddsg/init-anzu ()
  (global-set-key  (kbd "M-%") 'anzu-query-replace)
  (global-set-key  (kbd "C-M-%") 'anzu-query-replace-regexp)
  (defadvice anzu-query-replace (around wrap-query-replace activate)
    (save-excursion
      (let ((start (point)))
        ad-do-it
        (goto-char (point-min))
        ad-do-it)))
  (global-anzu-mode))

(defun tddsg/post-init-dictionary ()
  (global-set-key (kbd "M-m s d") 'dictionary-search)
  (setq dictionary-use-single-buffer t))

(defun tddsg/init-langtool ()
  (setq langtool-default-language "en-US")
  (setq langtool-language-tool-jar
        "/home/trungtq/Programs/LanguageTool-2.6/languagetool-commandline.jar"))

(defun tddsg/init-imenu-anywhere ())

(defun tddsg/init-vline ())

(defun tddsg/post-init-undo-tree ()
  (define-key undo-tree-map (kbd "C-_") nil)
  (global-set-key (kbd "C-/") 'undo)
  (global-set-key (kbd "C-S-/") 'undo-tree-redo))

(defun tddsg/init-crux ()
  (require 'crux)
  (global-set-key (kbd "<home>") 'crux-move-beginning-of-line)
  (global-set-key (kbd "C-^") 'crux-top-join-line)
  (global-set-key (kbd "C-_") 'join-line)
  (global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
  (global-set-key (kbd "C-c d") 'crux-duplicate-current-line-or-region))

(defun tddsg/post-init-column-marker ()
  (require 'column-marker)
  (defun tddsg-activate-column-marker ()
    (interactive)
    (column-marker-3 80))
  (add-hook 'prog-mode-hook 'tddsg-activate-column-marker)
  (add-hook 'latex-mode-hook 'tddsg-activate-column-marker)
  (add-hook 'text-mode-hook 'tddsg-activate-column-marker))

(defun tddsg/init-god-mode ()
  (require 'god-mode)
  (require 'god-mode-isearch)
  ;;; define switch key
  (define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
  (define-key isearch-mode-map (kbd "C-z") 'god-mode-isearch-activate)
  (define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)
  (define-key god-mode-isearch-map (kbd "C-z") 'god-mode-isearch-disable)
  (define-key god-local-mode-map (kbd "z") 'repeat)
  (define-key god-local-mode-map (kbd "<escape>") 'god-local-mode)
  (global-set-key (kbd "<escape>") 'god-local-mode)
  ;;; update cursor
  (defun update-cursor ()
    (if god-local-mode
        (set-cursor-color "purple")
      (set-cursor-color "lime green")))
  (defadvice other-window (after update activate) (update-cursor))
  (defadvice windmove-do-window-select (after update activate) (update-cursor))
  (defadvice split-window (after update activate) (update-cursor))
  (defadvice set-buffer (after update activate) (update-cursor))
  (defadvice switch-to-buffer (after update activate) (update-cursor))
  (defadvice save-buffer (after update activate) (update-cursor))
  (defadvice pop-to-buffer (after update activate) (update-cursor))
  (defadvice previous-buffer (after update activate) (update-cursor))
  (defadvice next-buffer (after update activate) (update-cursor))
  (defadvice keyboard-quit (after update activate) (update-cursor))
  ;; mode-line setting
  (defvar mode-line-god-mode
    '(:propertize
      (:eval (if god-local-mode " G " " N "))
      help-echo (if god-local-mode "God-mode is enabled" "God-mode is disable"))
    "Mode line format for God-mod")
  (put 'mode-line-god-mode 'risky-local-variable t)
  (defun my-god-hook ()
    (add-to-list 'mode-line-format mode-line-god-mode)
    (update-cursor))
  (add-hook 'god-mode-enabled-hook 'my-god-hook)
  (add-hook 'god-mode-disabled-hook 'my-god-hook))

(defun tddsg/post-init-org ()
  ;; unbind Shift + arrow
  (defun my-org-hook ()
    (define-key org-mode-map (kbd "S-<left>") nil)
    (define-key org-mode-map (kbd "S-<right>") nil)
    (define-key org-mode-map (kbd "S-<up>") nil)
    (define-key org-mode-map (kbd "S-<down>") nil))
  (add-hook 'org-mode-hook 'my-org-hook) 'append)

(defun tddsg/post-init-diminish ()
  (eval-after-load "abbrev" '(diminish 'abbrev-mode " ↹"))
  (eval-after-load "whitespace" '(diminish 'whitespace-mode " ␣"))
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
  (eval-after-load "projectile" (diminish 'projectile-mode " π")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOCAL PACKAGES

(defun tddsg/init-songbird ()
  (require 'songbird)
  (add-to-list 'auto-mode-alist '("\\.sb\\'" . songbird))
  (add-to-list 'auto-mode-alist '("\\.ss\\'" . songbird))
  (add-to-list 'auto-mode-alist '("\\.slk\\'" . songbird))
  (defun my-songbird-hook ()
    ;; customize syntax table for slurping/barfing parentheses
    (dolist (symbol (list ?. ?, ?\; ?: ?+ ?- ?@ ?! ?> ?<))
      (modify-syntax-entry symbol "'" songbird-syntax-table)))
  (add-hook 'songbird-hook 'my-songbird-hook 'append))

;; buffer-clone
(defun tddsg/init-buffer-clone ()
  (require 'buffer-clone)
  (global-set-key (kbd "s-S-<left>") 'buf-clone-left)
  (global-set-key (kbd "s-S-<right>") 'buf-clone-right)
  (global-set-key (kbd "s-S-<up>") 'buf-clone-up)
  (global-set-key (kbd "s-S-<down>") 'buf-clone-down)
  (global-set-key (kbd "M-m b m h") 'buf-clone-left)
  (global-set-key (kbd "M-m b c l") 'buf-clone-right)
  (global-set-key (kbd "M-m b c k") 'buf-clone-up)
  (global-set-key (kbd "M-m b c j") 'buf-clone-down))


;;; packages.el ends here
