;;; packages.el --- tddsg layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 TDDSG
;;
;; Author: trungtq <thedaydreamersg(*)g-m-a-i-l.com>
;; URL: https://tddsg.github.io
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
    noflet
    tuareg
    auctex
    cc-mode
    org
    latex-extra
    ace-popup-menu
    smartparens
    yasnippet
    hi-lock
    projectile
    helm
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
    buffer-move
    dired+
    pdf-tools
    elmacro
    helm-dired-history
    (songbird :location local)
    (buffer-clone :location local)
    (merlin-imenu :location local)
    (merlin-eldoc :location local)
    (smartparens-ocaml :location local))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INIT PACKAGES

(defun tddsg/init-comment-dwim-2 ()
  (require 'comment-dwim-2))

(defun tddsg/init-noflet ()
  (require 'noflet))

(defun tddsg/init-transpose-frame ()
  (require 'transpose-frame))

(defun tddsg/init-ace-popup-menu ()
  (ace-popup-menu-mode 1))

(defun tddsg/init-super-save ()
  (super-save-mode 1))

(defun tddsg/init-merlin-imenu ()
  (with-eval-after-load 'merlin
    (require 'merlin-imenu)))

(defun tddsg/init-merlin-eldoc ()
  (with-eval-after-load 'merlin
    (require 'merlin-eldoc)))

(defun tddsg/init-smartparens-ocaml ()
  (require 'smartparens-ocaml))

;; Don't know why can't use post-init- for cc-mode. Must use init-
(defun tddsg/init-cc-mode ()
  (defun my-c-mode-hook ()
    (c-set-style "linux")
    (setq c-basic-offset 4))
  (add-hook 'c-mode-hook 'my-c-mode-hook 'append))

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
  (defun merlin-locate-this-window ()
    (interactive)
    (setq merlin-locate-in-new-window 'never)
    (call-interactively 'merlin-locate))
  (defun merlin-locate-other-window ()
    (interactive)
    (setq merlin-locate-in-new-window 'always)
    (call-interactively 'merlin-locate))
  (defun my-tuareg-hook ()
    (interactive)
    (merlin-mode)
    (merlin-use-merlin-imenu)
    (eldoc-mode -1)
    (enable-ocp-indent)
    (setq indent-line-function 'ocp-indent-line)   ;; ocp-indent
    (setq merlin-locate-in-new-window 'diff)
    ;; customize syntax table for forward/backward slurping/barfing sexp
    (dolist (symbol (list ?, ?\; ?: ?+ ?- ?/ ?@ ?! ?> ?<))
      (modify-syntax-entry symbol "'" tuareg-mode-syntax-table))
    ;; unbind some keys
    (local-set-key (kbd "C-c C-i") nil)
    (local-set-key (kbd "C-c C-c") nil)
    (global-set-key (kbd "C-c l") nil)
    (define-key merlin-mode-map (kbd "C-c C-l") 'merlin-locate-this-window)
    (define-key merlin-mode-map (kbd "C-c l") 'merlin-locate-other-window)
    (define-key merlin-mode-map (kbd "M-.") 'merlin-locate-this-window)
    (define-key merlin-mode-map (kbd "C-M-.") 'merlin-locate-other-window))
  (add-hook 'tuareg-mode-hook 'my-tuareg-hook 'append))

(defun tddsg/post-init-auctex ()
  (require 'tex)
  (add-to-list 'TeX-command-list '("Make" "make" TeX-run-compile nil t))
  (custom-set-variables
   '(TeX-save-query nil)
   '(TeX-source-correlate-method (quote synctex))
   '(TeX-source-correlate-mode t)
   '(TeX-source-correlate-start-server t)
   '(LaTeX-command "latex --synctex=1")
   '(TeX-view-program-list
     (quote (("pdf-tools" "TeX-pdf-tools-sync-view"))))
   '(TeX-view-program-selection
     (quote ((engine-omega "dvips and gv")
             (output-dvi "xdvi")
             (output-pdf "pdf-tools")
             (output-html "xdg-open")))))
  ;; hook
  (defun my-latex-hook ()
    (setq TeX-newline-function 'newline-and-indent
          paragraph-separate "[ \t\f]*$"
          paragraph-start "\f\\|[ \t]*$")
    (require 'smartparens-latex)
    (linum-mode 1)
    (turn-on-auto-fill)
    (latex/auto-fill-mode)
    (pdf-sync-minor-mode)
    (abbrev-mode +1)
    (set-fill-column 75)
    (show-smartparens-mode)
    (smartparens-mode +1)
    (latex-extra-mode)
    (local-set-key (kbd "<f9>") (kbd "C-c C-c C-j")))
  (add-hook 'LaTeX-mode-hook 'my-latex-hook 'append)
  (add-hook 'tex-mode-hook 'my-latex-hook 'append))

(defun tddsg/post-init-smartparens ()
  ;; bindings
  (sp-use-paredit-bindings)
  (define-key smartparens-mode-map (kbd "C-<left>") nil)
  (define-key smartparens-mode-map (kbd "C-<right>") nil)
  (define-key smartparens-mode-map (kbd "M-s") nil)
  (define-key smartparens-mode-map (kbd "M-s s") 'sp-splice-sexp)
  ;; smartparens for ocaml
  (sp-with-modes '(tuareg-mode)
    (sp-local-pair "(*" "*)" :post-handlers '(sp-insert-pair)))
  ;; smartparens for latex
  (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
    (sp-local-pair "`" "'"
                   :actions '(:rem autoskip)
                   :skip-match 'sp-latex-skip-match-apostrophe
                   :unless '(sp-latex-point-after-backslash))
    (sp-local-pair "\\begin" "\\end" :post-handlers
                   '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\begin{enumerate}" "\\end{enumerate}" :post-handlers
                   '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\begin{itemize}" "\\end{itemize}" :post-handlers
                   '(sp-latex-insert-spaces-inside-pair)))
  ;; enable smartparens
  (smartparens-global-mode 1))

(defun tddsg/post-init-yasnippet ()
  (yas-global-mode t))

(defun tddsg/post-init-hi-lock ()
  (global-hi-lock-mode 1))

(defun tddsg/post-init-projectile ()
  (projectile-global-mode 1))

(defun tddsg/post-init-helm ()
  (require 'helm)
  (add-to-list 'helm-sources-using-default-as-input
               'helm-source-grep-ag)
  (substitute-key-definition 'find-tag 'helm-etags-select global-map)
  (setq projectile-completion-system 'helm
        helm-ff-file-name-history-use-recentf t
        helm-ff-transformer-show-only-basename nil)
  (require 'helm-config)
  (helm-mode 1))

(defun tddsg/post-init-expand-region ()
  (global-set-key (kbd "C-=") 'er/expand-region))

(defun tddsg/init-goto-chg ()
  (global-set-key (kbd "C-c C-\\") 'goto-last-change))

(defun tddsg/init-autorevert ()
  (global-auto-revert-mode t))

(defun tddsg/init-paren ()
  (show-paren-mode t))

(defun tddsg/init-windmove ()
  (windmove-default-keybindings))

;; buffer-clone
(defun tddsg/post-init-buffer-move ()
  (require 'buffer-move))

;; buffer-clone
(defun tddsg/init-buffer-clone ()
  (require 'buffer-clone))

(defun tddsg/init-key-chord ()
  (require 'key-chord)
  (setq key-chord-one-key-delay 0.18
        key-chord-two-key-delay 0.1)
  ;; reassign key-chords
  (key-chord-define-global ",." 'helm-mini)
  (key-chord-define-global "JK" 'previous-buffer)
  (key-chord-define-global "KL" 'next-buffer)
  (key-chord-define-global "ji" 'indent-according-to-mode)
  (key-chord-define-global "jj" 'avy-goto-char-2)
  (key-chord-define-global "jk" 'avy-goto-word-1)
  (key-chord-define-global "jl" 'avy-goto-line)
  (key-chord-mode 1))

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

(defun tddsg/init-dictionary ()
  (require 'dictionary)
  (setq dictionary-use-single-buffer t))

(defun tddsg/init-langtool ()
  (require 'langtool)
  (setq langtool-default-language "en-US"
        langtool-disabled-rules '("WHITESPACE_RULE"
                                  "EN_UNPAIRED_BRACKETS"
                                  "COMMA_PARENTHESIS_WHITESPACE"
                                  "EN_QUOTES")
        langtool-language-tool-jar
        "/home/trungtq/Programs/LanguageTool-3.4/languagetool-commandline.jar"))

(defun tddsg/init-imenu-anywhere ()
  (require 'imenu-anywhere))

(defun tddsg/init-vline ()
  (require 'vline))

(defun tddsg/init-column-marker ()
  (require 'column-marker))

(defun tddsg/init-crux ()
  (require 'crux))

(defun tddsg/init-dired+ ()
  (require 'dired+))

(defun tddsg/init-elmacro ()
  (require 'elmacro))

(defun tddsg/init-helm-dired-history ()
  (require 'savehist)
  (add-to-list 'savehist-additional-variables 'helm-dired-history-variable)
  (savehist-mode 1)
  (require 'helm-dired-history))

(defun tddsg/init-pdf-tools ()
  (pdf-tools-install)
  (setq pdf-view-resize-factor 1.05)
  (custom-set-variables
   '(pdf-view-midnight-colors  (quote ("#D3D3D3" . "#292B2E"))))
  (defadvice pdf-sync-forward-search (after jump-to-pdf activate) (other-window 1))
  (defun my-pdf-view-hook ()
    (if (not  (bound-and-true-p pdf-view-midnight-minor-mode))
        (pdf-view-midnight-minor-mode)))
  (add-hook 'pdf-view-mode-hook 'my-pdf-view-hook))

(defun tddsg/init-god-mode ()
  (require 'god-mode)
  ;;; update cursor
  (defun update-cursor ()
    (if god-local-mode
        (set-cursor-color "purple")
      (set-cursor-color "lime green")))
  ;; (defadvice other-window (after update activate) (update-cursor))
  ;; (defadvice windmove-do-window-select (after update activate) (update-cursor))
  ;; (defadvice windmove-left (after update activate) (update-cursor))
  ;; (defadvice windmove-right (after update activate) (update-cursor))
  ;; (defadvice windmove-up (after update activate) (update-cursor))
  ;; (defadvice windmove-down (after update activate) (update-cursor))
  ;; (defadvice split-window (after update activate) (update-cursor))
  ;; (defadvice set-buffer (after update activate) (update-cursor))
  ;; (defadvice switch-to-buffer (after update activate) (update-cursor))
  ;; (defadvice save-buffer (after update activate) (update-cursor))
  ;; (defadvice pop-to-buffer (after update activate) (update-cursor))
  ;; (defadvice previous-buffer (after update activate) (update-cursor))
  ;; (defadvice next-buffer (after update activate) (update-cursor))
  ;; (defadvice keyboard-quit (after update activate) (update-cursor))
  (defun my-god-mode-hook () (interactive) (update-cursor))
  (add-hook 'god-mode-enabled-hook 'my-god-mode-hook)
  (add-hook 'god-mode-disabled-hook 'my-god-mode-hook))

(defun tddsg/post-init-org ()
  ;; unbind Shift + arrow
  (defun my-org-hook ()
    ;; disable keys
    (define-key org-mode-map (kbd "S-<left>") nil)
    (define-key org-mode-map (kbd "S-<right>") nil)
    (define-key org-mode-map (kbd "S-<up>") nil)
    (define-key org-mode-map (kbd "S-<down>") nil)
    ;; indentation
    (org-indent-mode t)
    (setq indent-line-function 'indent-relative))
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


;;; packages.el ends here

