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
    ace-popup-menu
    smartparens
    yasnippet
    hi-lock
    projectile
    expand-region
    goto-chg
    paren
    autorevert
    windmove
    transpose-frame
    key-chord
    super-save
    whitespace
    anzu
    dictionary
    langtool
    imenu-anywhere
    vline
    crux
    column-marker
    god-mode
    (songbird :location local)
    (buffer-clone :location local)
    (merlin-imenu :location local)
    (smartparens-ocaml :location local)
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

(defun tddsg/init-comment-dwim-2 ()
  (require 'comment-dwim-2))

(defun tddsg/init-transpose-frame ()
  (require 'transpose-frame))

(defun tddsg/init-ace-popup-menu ()
  (ace-popup-menu-mode 1))

(defun tddsg/init-super-save ()
  (super-save-mode 1))

(defun tddsg/init-merlin-imenu ())

(defun tddsg/init-smartparens-ocaml ()
  (require 'smartparens-ocaml))

(defun tddsg/post-init-tuareg ()
  (require 'merlin-imenu)
  (dolist (var (car (read-from-string
                     (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var)))
  (setq exec-path (split-string (getenv "PATH") path-separator)
        opam-share (substring (shell-command-to-string
                               "opam config var share 2> /dev/null") 0 -1))
  ;; fix syntax highlight for OCaml
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
  (add-hook 'tuareg-mode-hook 'my-tuareg-hook 'append))

(defun tddsg/post-init-auctex ()
  (require 'tex)
  (add-to-list 'TeX-command-list '("Make" "make" TeX-run-compile nil t))
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

(defun tddsg/post-init-smartparens ()
  (sp-use-paredit-bindings)
  (define-key smartparens-mode-map (kbd "C-<left>") nil)
  (define-key smartparens-mode-map (kbd "C-<right>") nil)
  (define-key smartparens-mode-map (kbd "M-s") nil)
  (define-key smartparens-mode-map (kbd "M-s s") 'sp-splice-sexp)
  (smartparens-global-mode t))

(defun tddsg/post-init-yasnippet ()
  (yas-global-mode t))

(defun tddsg/post-init-hi-lock ()
  (global-hi-lock-mode 1))

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

(defun tddsg/post-init-whitespace ()
  (add-hook 'before-save-hook #'whitespace-cleanup)
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face tabs trailing)))


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
  (require 'langtool)
  (setq langtool-default-language "en-US")
  (setq langtool-language-tool-jar
        "/home/trungtq/Programs/LanguageTool-2.6/languagetool-commandline.jar"))

(defun tddsg/init-imenu-anywhere ()
  (require 'imenu-anywhere))

(defun tddsg/init-vline ()
  (require 'vline))

(defun tddsg/init-column-marker ()
  (require 'column-marker))

(defun tddsg/init-crux ()
  (require 'crux))

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
