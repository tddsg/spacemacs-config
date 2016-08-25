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
    windmove)
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

;;; configuration for comment-dwim-2
(defun tddsg/init-comment-dwim-2 ()
  (global-set-key (kbd "M-;") 'comment-dwim-2))

;;; configuration for ace-popup-menu
(defun tddsg/init-ace-popup-menu ()
  (ace-popup-menu-mode 1))

;;; configuration for tuareg
(defun tddsg/post-init-tuareg ()
  ;; setup environments
  (dolist (var (car (read-from-string
                     (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var)))
  (setq exec-path (split-string (getenv "PATH") path-separator)
        opam-share (substring (shell-command-to-string
                               "opam config var share 2> /dev/null")
                              0 -1))
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

;;; packages.el ends here
