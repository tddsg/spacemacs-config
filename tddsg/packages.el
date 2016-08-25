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
  '(
    comment-dwim-2
    bufclone
    tuareg
    auctex
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
    crux
    buffer-move
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
  (defun my-shell-hook ()
    (local-set-key (kbd "C-j") 'newline))
  (add-hook 'shell-mode-hook 'my-shell-hook))

(defun tddsg/post-init-smartparens ()
  (smartparens-global-mode t))

(defun tddsg/post-init-yasnippet ()
  (yas-global-mode  t))

(defun tddsg/post-init-hi-lock ()
  (global-hi-lock-mode 1))

(defun tddsg/post-init-helm ()
  ;; (add-to-list 'helm-sources-using-default-as-input
  ;;              'helm-source-grep-ag)
  )

(defun tddsg/post-init-projectile ()
  (projectile-global-mode 1))

(defun tddsg/init-expand-region ()
  (global-set-key (kbd "C-=") 'er/expand-region))

(defun tddsg/init-goto-chg ()
  (global-set-key (kbd "C-c C-\\") 'goto-last-change))

(defun tddsg/init-autorevert ()
  (global-auto-revert-mode t))

(defun tddsg/init-paren ()
  (show-paren-mode t))

(defun tddsg/init-windmove ()
  (windmove-default-keybindings))

(defun tddsg/init-key-chord ()
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

(defun tddsg/init-flyspell ()
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra")
        ispell-dictionary "english"
        prelude-flyspell nil)
  (add-hook 'text-mode-hook #'flyspell-mode))

(defun tddsg/init-whitespace ()
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  ;; default of prelude
  ;; (setq whitespace-style '(face tabs empty trailing lines-tail)) ;;
  (setq whitespace-style '(face tabs trailing)))

(defun tddsg/init-move-text ()
  (global-set-key (kbd "M-S-<up>") 'move-text-up)
  (global-set-key (kbd "M-S-<down>") 'move-text-down))

(defun tddsg/post-init-company ()
  (use-package company
    :config 
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
    (define-key company-active-map (kbd "M-.") 'company-show-location)
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous)
    ;; (setq company-idle-delay 200)         ;; set delay time by default
    (global-company-mode)))

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

(defun tddsg/init-dictionary ()
  (setq dictionary-use-single-buffer t))

(defun tddsg/init-langtool ()
  (setq langtool-default-language "en-US")
  (setq langtool-language-tool-jar
        "/home/trungtq/Programs/LanguageTool-2.6/languagetool-commandline.jar"))

(defun tddsg/init-imenu-anywhere ())

(defun tddsg/init-crux ()
  (global-set-key (kbd "C-^") 'crux-top-join-line)
  (global-set-key (kbd "C-_") 'join-line)
  (global-set-key (kbd "<home>") 'crux-move-beginning-of-line)
  (global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
  )


;;; packages.el ends here
