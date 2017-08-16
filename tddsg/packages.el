;;; packages.el --- tddsg layer packages file for Spacemacs.
;;
;; Copyright (c) 2016-present TDDSG
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
  '(;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; languages
    tuareg
    merlin
    auctex
    latex-extra
    org
    cc-mode
    llvm-mode
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; utilities
    comment-dwim-2
    noflet
    sr-speedbar
    rtags
    company-rtags
    irony
    company-irony
    company-irony-c-headers
    god-mode
    ace-popup-menu
    smartparens
    paren
    helm-rtags
    helm-ispell
    helm-bibtex
    helm-dired-history
    goto-chg
    autorevert
    windmove
    framemove
    transpose-frame
    key-chord
    super-save
    whitespace
    anzu
    imenu-anywhere
    vline
    column-marker
    buffer-move
    dired+
    elmacro
    elpy
    company-math
    math-symbol-lists
    bash-completion
    swiper
    swiper-helm
    counsel
    monky
    zone-sl
    ;;; writing, spelling
    dictionary
    langtool
    writegood-mode
    ;;; local
    (songbird :location local)
    (proddag :location local)
    (buffer-clone :location local))
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
  (use-package comment-dwim-2))

(defun tddsg/init-noflet ()
  (use-package noflet))

(defun tddsg/init-transpose-frame ()
  (use-package transpose-frame))

(defun tddsg/init-ace-popup-menu ()
  (ace-popup-menu-mode 1))

(defun tddsg/init-super-save ()
  (super-save-mode 1))

(defun tddsg/post-init-cc-mode ()
  ;; coding style
  (defconst my-cc-style
    '("linux"
      (c-offsets-alist . ((innamespace . [0]))) ;; no indent in namespace
      ))
  (c-add-style "my-cc-style" my-cc-style)
  ;; hook
  (defun my-cc-mode-hook ()
    (setq company-backends (delete 'company-semantic company-backends))
    (add-to-list 'company-backends '(company-irony-c-headers company-irony))
    (c-set-style "my-cc-style")
    (setq c-basic-offset 4)
    (rtags-start-process-unless-running)  ;; using rtags
    (irony-mode)                          ;; using irony
    (semantic-mode -1)
    (local-set-key (kbd "C-c C-c") nil))
  (add-hook 'c-mode-hook 'my-cc-mode-hook 'append)
  (add-hook 'c++-mode-hook 'my-cc-mode-hook 'append)
  (add-hook 'objc-mode-hook 'my-cc-mode-hook 'append)
  (add-hook 'c-mode-common-hook 'my-cc-mode-hook 'append))

(defun tddsg/post-init-tuareg ()
  ;; fix syntax highlight for OCaml
  (font-lock-add-keywords
   'tuareg-mode
   '(("\\<\\(let\\|in\\|open\\|module\\|rec\\)\\>" . font-lock-keyword-face)
     ("\\<\\(with\\|and\\|type\\|include\\)\\>" . font-lock-keyword-face)
     ("\\<\\(struct\\|mutable\\|begin\\|end\\)\\>" . font-lock-keyword-face)
     ("\\<\\(sig\\|val\\|functor\\|raise\\)\\>" . font-lock-keyword-face)
     ("\\<\\(class\\|object\\|method\\|inherit\\)\\>" . font-lock-keyword-face)
     ("\\<\\(external\\|virtual\\)\\>" . font-lock-keyword-face)
     ("\\<[A-Z][A-Za-z0-9_']*\\>" . font-lock-constant-face)))
  (defun disable-ocp-indent ()
    (interactive)
    (setq indent-line-function 'indent-relative))
  (defun enable-ocp-indent ()
    (interactive)
    (setq indent-line-function 'ocp-indent-line))
  (defun my-tuareg-hook ()
    (merlin-mode)
    (eldoc-mode -1)
    (enable-ocp-indent)
    (setq indent-line-function 'ocp-indent-line)   ;; ocp-indent
    ;; customize syntax table for forward/backward slurping/barfing sexp
    (dolist (symbol (list ?, ?\; ?: ?+ ?- ?/ ?@ ?! ?> ?<))
      (modify-syntax-entry symbol "'" tuareg-mode-syntax-table))
    ;; unbind some keys
    (local-set-key (kbd "C-c C-i") nil)
    (local-set-key (kbd "C-c C-c") nil)
    (local-set-key (kbd "C-M-h") nil)
    (local-set-key (kbd "M-q") nil))
  (add-hook 'tuareg-mode-hook 'my-tuareg-hook 'append))

(defun tddsg/post-init-merlin ()
  (dolist (var (car (read-from-string
                     (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var)))
  (setq exec-path (split-string (getenv "PATH") path-separator)
        opam-share (substring (shell-command-to-string
                               "opam config var share 2> /dev/null") 0 -1))
  (defun merlin-locate-this-window ()
    (interactive)
    (setq merlin-locate-in-new-window 'never)
    (call-interactively 'merlin-locate))
  (defun merlin-locate-other-window ()
    (interactive)
    (setq merlin-locate-in-new-window 'always)
    (call-interactively 'merlin-locate))
  (defun my-merlin-hook ()
    (require 'merlin-imenu)
    (merlin-use-merlin-imenu)
    (setq merlin-locate-in-new-window 'diff)
    ;; unbind some keys
    (global-set-key (kbd "C-c l") nil)
    (define-key merlin-mode-map (kbd "C-c C-l") 'merlin-locate-this-window)
    (define-key merlin-mode-map (kbd "C-c l") 'merlin-locate-other-window)
    (define-key merlin-mode-map (kbd "M-.") 'merlin-locate-this-window)
    (define-key merlin-mode-map (kbd "C-M-.") 'merlin-locate-other-window)
    (define-key merlin-mode-map (kbd "M-,") 'merlin-error-next)
    (define-key merlin-mode-map (kbd "C-M-,") 'merlin-error-prev))
  (add-hook 'merlin-mode-hook 'my-merlin-hook 'append))

(defun tddsg/init-latex-extra ()
  (let ((byte-compile-warnings '(not free-vars)))
    (use-package latex-extra
      :ensure t
      :config
      (setq latex/view-after-compile nil)
      (add-hook 'LaTeX-mode-hook #'latex-extra-mode))))

(defun tddsg/init-helm-bibtex ()
  (use-package helm-bibtex
    :ensure t
    :config
    (add-hook 'LaTeX-mode-hook #'latex-extra-mode)))

(defun tddsg/init-helm-ispell ()
  (use-package helm-ispell
    :ensure t))

(defun tddsg/post-init-auctex ()
  (require 'tex)
  (require 'reftex)
  (require 'latex)
  (add-to-list 'TeX-command-list '("Make" "make" TeX-run-compile nil t))
  (custom-set-variables
   '(TeX-save-query nil)
   '(TeX-source-correlate-method 'synctex)
   '(TeX-source-correlate-mode t)
   '(TeX-source-correlate-start-server t)
   '(LaTeX-command "latex --synctex=1")
   '(TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
   '(TeX-view-program-selection '((output-pdf "pdf-tools"))))
  ;; add frametitle into outline of reftex and TeX
  (push '("frametitle" . 3) reftex-section-levels)
  (push '("frametitle" 3) TeX-outline-extra)
  (defun latex/font-underline () (interactive) (TeX-font nil ?\C-l))
  ;; remove the outer environment
  (defun LaTeX-delete-environment ()
    (interactive)
    (when (LaTeX-current-environment)
      (save-excursion
        (let* ((begin-start (save-excursion
                              (LaTeX-find-matching-begin)
                              (point)))
               (begin-end (save-excursion
                            (goto-char begin-start)
                            (search-forward-regexp "begin{.*?}")))
               (end-end (save-excursion
                          (LaTeX-find-matching-end)
                          (point)))
               (end-start (save-excursion
                            (goto-char end-end)
                            (1- (search-backward-regexp "\\end")))))
          ;; delete end first since if we delete begin first it shifts the
          ;; location of end
          (delete-region end-start end-end)
          (delete-region begin-start begin-end)))))
  ;; remove the outer macro
  (defun TeX-remove-macro ()
    "Remove current macro and return `t'.  If no macro at point, return `nil'."
    (interactive)
    (when (TeX-current-macro)
      (let ((bounds (TeX-find-macro-boundaries))
            (brace  (save-excursion
                      (goto-char (1- (TeX-find-macro-end)))
                      (TeX-find-opening-brace))))
        (delete-region (1- (cdr bounds)) (cdr bounds))
        (delete-region (car bounds) (1+ brace)))
      t))
  (defun my-latex-hook ()
    ;; set tex master file
    (let ((main-tex (concat (projectile-project-root) "main.tex")))
      (when (and (eq TeX-master t) (file-exists-p main-tex))
        (setq TeX-master main-tex)))
    (let ((main-bib (concat (projectile-project-root) "main.bib")))
      (when (file-exists-p main-bib)
        (setq bibtex-completion-bibliography (list main-bib))))
    ;; other setting
    (setq TeX-newline-function 'newline-and-indent
          paragraph-separate "[ \t\f]*$"
          paragraph-start "\f\\|[ \t]*$")
    (LaTeX-add-environments "small" "footnotesize" "scriptsize" "tiny")
    (latex-extra-mode)
    (turn-on-auto-fill)
    (latex/auto-fill-mode)
    (abbrev-mode +1)
    (set-fill-column 75))
  (add-hook 'LaTeX-mode-hook 'my-latex-hook 'append)
  (add-hook 'tex-mode-hook 'my-latex-hook 'append)
  (add-hook 'TeX-mode-hook 'my-latex-hook 'append))

(defun tddsg/post-init-smartparens ()
  ;; bindings
  (sp-use-paredit-bindings)
  (define-key smartparens-mode-map (kbd "C-<left>") nil)
  (define-key smartparens-mode-map (kbd "C-<right>") nil)
  (define-key smartparens-mode-map (kbd "M-s") nil)
  (define-key smartparens-mode-map (kbd "M-s s") 'sp-splice-sexp)
  ;; smartparens for ocaml
  (sp-with-modes '(tuareg-mode)
    (sp-local-pair "(*" "*)" ))
  ;; smartparens for org-mode
  (sp-with-modes 'org-mode
    (sp-local-pair "*" "*"
                   :unless '(sp-point-after-word-p sp-point-at-bol-p)
                   :wrap "C-*")
    (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
    (sp-local-pair "/" "/" :unless '(sp-point-after-word-p)
                   :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "~" "~" :unless '(sp-point-after-word-p)
                   :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "=" "=" :unless '(sp-point-after-word-p)
                   :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "«" "»"))
  ;; smartparens for latex
  (sp-with-modes '(tex-mode
                   plain-tex-mode
                   latex-mode
                   LaTeX-mode)
    ;; (sp-local-pair "{" nil :actions :rem)
    (sp-local-pair "`" "'"
                   :actions '(:rem autoskip)
                   :skip-match 'sp-latex-skip-match-apostrophe
                   :unless '(sp-latex-point-after-backslash))
    (sp-local-pair "$" "$")
    (sp-local-pair "``" "''" :trigger "\"" :actions :rem)
    (sp-local-pair "\\begin{frame}" "\\end{frame}")
    (sp-local-pair "\\begin" "\\end" :post-handlers
                   '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\If" "\\EndIf" :post-handlers
                   '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\While" "\\EndWhile" :post-handlers
                   '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\For" "\\EndFor" :post-handlers
                   '(sp-latex-insert-spaces-inside-pair)))
  ;; enable smartparens
  (smartparens-global-mode 1))

(defun tddsg/init-goto-chg ()
  (global-set-key (kbd "C-c C-\\") 'goto-last-change))

(defun tddsg/init-autorevert ()
  (global-auto-revert-mode t))

(defun tddsg/init-paren ()
  (show-paren-mode t))

(defun tddsg/init-windmove ()
  (require 'framemove)
  (windmove-default-keybindings)
  (setq framemove-hook-into-windmove t))

(defun tddsg/init-framemove ()
  (framemove-default-keybindings))

;; buffer-clone
(defun tddsg/init-buffer-move ()
  (use-package buffer-move))

;; buffer-clone
(defun tddsg/init-buffer-clone ()
  (use-package buffer-clone))

(defun tddsg/init-key-chord ()
  (require 'key-chord)
  (setq key-chord-one-key-delay 0.18
        key-chord-two-key-delay 0.1)
  (key-chord-mode 1))

(defun tddsg/post-init-whitespace ()
  (add-hook 'before-save-hook #'whitespace-cleanup)
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face tabs)))


(defun tddsg/init-anzu ()
  (global-set-key  (kbd "M-%") 'anzu-query-replace)
  (global-set-key  (kbd "C-M-%") 'anzu-query-replace-regexp)
  (defadvice anzu-query-replace (around wrap-query-replace activate)
    (save-excursion
      (goto-char (anzu--thing-begin t))
      ad-do-it
      (goto-char (point-min))
      ad-do-it))
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
        "/home/trungtq/Programs/LanguageTool/languagetool-commandline.jar"))

(defun tddsg/init-writegood-mode ()
  (require 'writegood-mode))

(defun tddsg/init-llvm-mode ()
  (use-package llvm-mode))

(defun tddsg/init-imenu-anywhere ()
  (use-package imenu-anywhere))

(defun tddsg/init-vline ()
  (use-package vline))

(defun tddsg/init-column-marker ()
  (use-package column-marker))

(defun tddsg/init-elmacro ()
  (use-package elmacro))

(defun tddsg/init-elpy ()
  (use-package elpy))

(defun tddsg/init-dired+ ()
  (use-package dired+))

(defun tddsg/init-helm-dired-history ()
  (use-package helm-dired-history))

(defun tddsg/init-company-math ()
  (use-package company-math))

(defun tddsg/init-math-symbol-lists ()
  (use-package math-symbol-lists))

(defun tddsg/init-bash-completion ()
  (use-package bash-completion))

(defun tddsg/init-monky ()
  (use-package monky))

(defun tddsg/init-zone-sl ()
  (use-package zone-sl))

(defun tddsg/init-counsel ()
  (use-package counsel))

(defun tddsg/init-swiper ()
  (use-package swiper))

(defun tddsg/init-swiper-helm ()
  (use-package swiper-helm))

(defun tddsg/init-sr-speedbar ()
  (use-package sr-speedbar
    :config
    (defun select-next-window ()
      (other-window 1))
    (defun my-sr-speedbar-open-hook ()
      (add-hook 'speedbar-visiting-file-hook 'select-next-window t)
      (add-hook 'speedbar-visiting-tag-hook 'select-next-window t))
    (advice-add 'sr-speedbar-open :after #'my-sr-speedbar-open-hook)))

(defun tddsg/init-rtags ()
  (use-package rtags
    :config
    (setq rtags-completions-enabled t)
    (eval-after-load 'company '(add-to-list 'company-backends 'company-rtags))
    (which-key-add-major-mode-key-based-replacements
      'c-mode "C-c t" "rtags-commands")
    (which-key-add-major-mode-key-based-replacements
      'c++-mode "C-c t" "rtags-commands")
    (rtags-enable-standard-keybindings c-mode-base-map "\C-c t")
    (setq rtags-autostart-diagnostics t)))

(defun tddsg/init-company-rtags ()
  (use-package company-rtags))

(defun tddsg/init-helm-rtags ()
  (use-package helm-rtags
    :config
    (setq rtags-display-result-backend 'helm)))

(defun tddsg/init-irony ()
  (use-package irony
    :config
    (defun my-irony-mode-hook ()
      (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
      (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async)
      (company-irony-setup-begin-commands))
    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

(defun tddsg/init-company-irony ()
  (use-package company-irony))

(defun tddsg/init-company-irony-c-headers ()
  (use-package company-irony-c-headers))

(defun tddsg/init-god-mode ()
  (require 'god-mode)
  ;;; update cursor
  (defun update-cursor ()
    (cond ((or (bound-and-true-p god-mode)
               (bound-and-true-p god-global-mode))
           (set-cursor-color "lime green"))
          ((eq spacemacs--cur-theme 'leuven)
           (set-cursor-color "dark orange"))
          ((eq spacemacs--cur-theme 'spacemacs-dark)
           (set-cursor-color "dark orange"))))
  (defun advice-update-cursor (orig-func &rest args)
    (apply orig-func args)
    (update-cursor))
  (dolist (func (list 'windmove-do-window-select
                      ;; 'select-window  ;; do not advise select-window
                      'god-mode-isearch-activate
                      'god-mode-isearch-disable
                      'god-mode-all))
    (advice-add func :around #'advice-update-cursor))
  (add-hook 'god-mode-enabled-hook 'update-cursor)
  (add-hook 'god-mode-disabled-hook 'update-cursor))

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
  ;; customize syntax table for forward/backward slurping/barfing sexp
  (defun my-songbird-hook ()
    ;; customize syntax table for slurping/barfing parentheses
    (dolist (symbol (list ?. ?, ?\; ?: ?+ ?- ?@ ?! ?> ?<))
      (modify-syntax-entry symbol "'" songbird-syntax-table))
    (dolist (symbol (list ?( ?) ?{ ?} ?[ ?]))
      (modify-syntax-entry symbol "_" songbird-syntax-table)))
  (add-hook 'songbird-hook 'my-songbird-hook 'append))

(defun tddsg/init-proddag ()
  (require 'proddag)
  (add-to-list 'auto-mode-alist '("\\.pdd\\'" . proddag))
 (defun my-proddag-hook ()
    ;; add hook here
    )
  (add-hook 'proddag-hook 'my-proddag-hook 'append))

;;; packages.el ends here
