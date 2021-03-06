;;; packages.el --- tddsg layer packages file for Spacemacs.
;;
;; Copyright (c) 2016-present TDDSG
;;
;; Author: tddsg
;; URL: https://tddsg.github.io
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst tddsg-packages
  '(;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    tuareg
    pdf-tools
    merlin
    auctex
    latex-extra
    cc-mode
    go-mode
    irony
    solidity-mode
    langtool
    helm-ag
    windmove
    popwin
    smartparens
    ;; set to local since spacemacs cannot install it from melpa
    (dired+ :location local)
    (framemove :location local)
    (llvm-mode :location local)
    (songbird :location local)
    (hummingbird :location local)
    (guess-style :location local))
  "The list of Lisp packages required by the tddsg layer.
   See: https://github.com/milkypostman/melpa#recipe-format")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INIT PACKAGES

(defun tddsg/post-init-helm-ag ()
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
          (error (error "`helm-ag' is not available. Is MELPA in your `package-archives'?")))))))

(defun tddsg/init-solidity-mode ()
  (use-package solidity-mode)
  (push 'solidity-mode irony-supported-major-modes))

(defun tddsg/init-windmove ()
  (require 'framemove)
  (windmove-default-keybindings)
  (setq framemove-hook-into-windmove t))

;;; PDF-TOOLS
(defun tddsg/post-init-pdf-tools ()
  (require 'pdf-sync)

  ;; pdf-view
  (defun update-pdf-view ()
    (when (derived-mode-p 'pdf-view-mode)
      ;; enable minor modes
      (pdf-tools-enable-minor-modes)
      (set (make-local-variable 'evil-emacs-state-cursor) (list nil))
      ;; update theme
      (cond ((eq spacemacs--cur-theme 'spacemacs-dark)
             (if (not (bound-and-true-p pdf-view-midnight-minor-mode))
                 (pdf-view-midnight-minor-mode)))
            ((eq spacemacs--cur-theme 'leuven)
             (if (bound-and-true-p pdf-view-midnight-minor-mode)
                 (pdf-view-midnight-minor-mode -1))))))
  (defadvice spacemacs/cycle-spacemacs-theme (after pdf-view activate)
    (mapc (lambda (window) (with-current-buffer (window-buffer window)
                             (update-pdf-view)))
          (window-list)))
  (add-hook 'pdf-view-mode-hook 'update-pdf-view)

  ;;;;; customize pdf-isearch for syncing backward
  (defun pdf-isearch-sync-backward ()
    "Sync backward to the LaTeX source of the current match."
    (interactive)
    (if pdf-isearch-current-match
        (let ((left (caar pdf-isearch-current-match))
              (top (cadar pdf-isearch-current-match)))
          (isearch-exit)
          (funcall 'pdf-sync-backward-search left top))))

  ;;;;; other settings
  (setq pdf-view-resize-factor 1.05)
  (add-hook 'pdf-view-mode-hook 'pdf-view-auto-slice-minor-mode)
  (custom-set-variables
   '(pdf-view-midnight-colors  (quote ("#D3D3D3" . "#292B2E")))))

;;; CC-MODE
(defun tddsg/post-init-cc-mode ()
  (require 'rtags)
  ;; hook
  (defun my-cc-mode-hook ()
    (setq company-backends (delete 'company-semantic company-backends))
    (add-to-list 'company-backends '(company-irony-c-headers company-irony))
    (c-set-offset 'innamespace 0)   ;; no indent in namespace
    (setq c-basic-offset 2)
    (setq tab-width 2)
    (rtags-start-process-unless-running)  ;; using rtags
    (irony-mode)                          ;; using irony
    (rtags-activate-imenu)
    (semantic-mode -1)
    (local-set-key (kbd "C-c C-c") nil))
  (add-hook 'c-mode-hook 'my-cc-mode-hook 'append)
  (add-hook 'c++-mode-hook 'my-cc-mode-hook 'append)
  (add-hook 'objc-mode-hook 'my-cc-mode-hook 'append)
  (add-hook 'c-mode-common-hook 'my-cc-mode-hook 'append))

(defun tddsg/post-init-go-mode ()
  (defun my-go-mode-hook ()
    (setq tab-width 4)
    (setq indent-tabs-mode 1))
  (add-hook 'go-mode-hook 'my-go-mode-hook 'append))

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
  (defun my-tuareg-hook ()
    (merlin-mode)
    (eldoc-mode -1)
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

(defun tddsg/post-init-popwin ()
  ;;; customize popwin to show it in a split window
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
                (let* ((new-height (- popwin:popup-window-height))
                       (orig-window (selected-window))
                       (new-window (split-window orig-window new-height 'below)))
                  (set-window-buffer new-window buffer)
                  (list orig-window new-window nil)))
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
                                             :width width :height height
                                             :position position :noselect noselect
                                             :dedicated dedicated :stick stick
                                             :tail tail)
              popwin:popup-window-dedicated-p dedicated
              popwin:popup-window-stuck-p stick)))
    (if noselect
        (setq popwin:focus-window popwin:selected-window)
      (setq popwin:focus-window popwin:popup-window)
      (select-window popwin:popup-window))
    (run-hooks 'popwin:after-popup-hook)
    popwin:popup-window))

(defun tddsg/init-latex-extra ()
  (let ((byte-compile-warnings '(not free-vars)))
    (use-package latex-extra
      :ensure t
      :config
      (setq latex/view-after-compile nil)
      (add-hook 'LaTeX-mode-hook #'latex-extra-mode))))

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
   '(TeX-command-extra-options "-shell-escape")
   '(TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
   '(TeX-view-program-selection '((output-pdf "pdf-tools"))))
  ;; add frametitle into outline of reftex and TeX
  (push '("frametitle" . 3) reftex-section-levels)
  (push '("frametitle" 3) TeX-outline-extra)
  (defun latex/font-underline () (interactive) (TeX-font nil ?\C-l))
  (defun latex/font-pack () (interactive) (TeX-font nil ?\C-p))
  ;; remove the outer environment
  (defun LaTeX-delete-environment ()
    (interactive)
    (when (LaTeX-current-environment)
      (save-excursion
        (let* ((begin-start (save-excursion (LaTeX-find-matching-begin)
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
  ;;; customize brace-count to allow indentation of square brackets
  (defun TeX-brace-count-line ()
    "Count number of open/closed braces."
    (save-excursion
      (let ((count 0) (limit (line-end-position)) char)
        (while (progn
                 (skip-chars-forward "^{}[]\\\\" limit)
                 (when (and (< (point) limit) (not (TeX-in-comment)))
                   (setq char (char-after))
                   (forward-char)
                   (cond ((eq char ?\{)
                          (setq count (+ count TeX-brace-indent-level)))
                         ((eq char ?\})
                          (setq count (- count TeX-brace-indent-level)))
                         ((eq char ?\[)
                          (setq count (+ count TeX-brace-indent-level)))
                         ((eq char ?\])
                          (setq count (- count TeX-brace-indent-level)))
                         ((eq char ?\\)
                          (when (< (point) limit) (forward-char) t))))))
        count)))
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
    ;; find bibtex files
    (setq bibtex-completion-bibliography
          (directory-files "." nil "\\.bib$"))
    ;; other setting
    (setq TeX-newline-function 'newline-and-indent)
    (LaTeX-add-environments "small" "footnotesize" "scriptsize" "tiny")
    (writegood-mode)
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
  (sp-with-modes 'tuareg-mode
    (sp-local-pair "struct" "end")
    (sp-local-pair "(*" "*)")
    (sp-local-pair "'" "'"))
  (sp-with-modes 'coq-mode
    (sp-local-pair "(*" "*)"))
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
    ;; (sp-local-pair "`" "'"
    ;;                :actions '(:rem autoskip)
    ;;                :skip-match 'sp-latex-skip-match-apostrophe
    ;;                :unless '(sp-latex-point-after-backslash))
    (sp-local-pair "`" "'" :actions nil)
    (sp-local-pair "``" "''" :trigger "\"" :actions :rem)
    (sp-local-pair "\\,{" "}\\,")
    (sp-local-pair "\\begin{frame}" "\\end{frame}")
    (sp-local-pair "\\begingroup" "\\endgroup")
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

(defun tddsg/init-langtool ()
  (require 'langtool)
  (defun langtool-autoshow-detail-popup (overlays)
    (when (require 'popup nil t)
      ;; Do not interrupt current popup
      (unless (or popup-instances
                  ;; suppress popup after type `C-g` .
                  (memq last-command '(keyboard-quit)))
        (let ((msg (langtool-details-error-message overlays)))
          (popup-tip msg)))))
  (setq langtool-default-language "en-US"
        langtool-disabled-rules '("WHITESPACE_RULE"
                                  "EN_UNPAIRED_BRACKETS"
                                  "COMMA_PARENTHESIS_WHITESPACE"
                                  "EN_QUOTES")
        langtool-autoshow-message-function 'langtool-autoshow-detail-popup)
  (when (eq system-type 'gnu/linux)
    (setq langtool-language-tool-jar
          "/home/trungtq/Programs/LanguageTool/languagetool-commandline.jar"
          langtool-user-arguments
          '("--languagemodel" "/home/trungtq/Programs/LanguageTool/")))
  (when (eq system-type 'darwin)
    (setq langtool-language-tool-jar
          "/Users/trungtq/Applications/LanguageTool/languagetool-commandline.jar"
          langtool-user-arguments
          '("--languagemodel" "/Users/trungtq/Applications/LanguageTool/"))))

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
    (setq rtags-autostart-diagnostics t
          rtags-use-helm t)))

(defun tddsg/init-helm-rtags ()
  (use-package helm-rtags
    :config (setq rtags-display-result-backend 'helm)))

(defun tddsg/init-irony ()
  (use-package irony
    :config
    (defun my-irony-mode-hook ()
      (define-key irony-mode-map [remap completion-at-point]
        'irony-completion-at-point-async)
      (define-key irony-mode-map [remap complete-symbol]
        'irony-completion-at-point-async)
      (company-irony-setup-begin-commands))
    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

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
    ;; (dolist (symbol (list ?( ?) ?{ ?} ?[ ?]))
    ;;   (modify-syntax-entry symbol "_" songbird-syntax-table))
    (dolist (symbol (list ?. ?, ?\; ?: ?+ ?- ?@ ?! ?> ?<))
      (modify-syntax-entry symbol "'" songbird-syntax-table)))
  (add-hook 'songbird-hook 'my-songbird-hook 'append))

(defun tddsg/init-hummingbird ()
  (require 'hummingbird)
  (add-to-list 'auto-mode-alist '("\\.hb\\'" . hummingbird))
  (add-to-list 'auto-mode-alist '("\\.sv\\'" . hummingbird))
  ;; customize syntax table for forward/backward slurping/barfing sexp
  (defun my-hummingbird-hook ()
    ;; customize syntax table for slurping/barfing parentheses
    (dolist (symbol (list ?. ?, ?\; ?: ?+ ?- ?@ ?! ?> ?<))
      (modify-syntax-entry symbol "'" hummingbird-syntax-table)))
  (add-hook 'hummingbird-hook 'my-hummingbird-hook 'append))

(defun tddsg/init-dired+ ()
  (use-package dired+))

(defun tddsg/init-guess-style ()
  (use-package guess-style)
  (autoload 'guess-style-set-variable "guess-style" nil t)
  (autoload 'guess-style-guess-variable "guess-style")
  (autoload 'guess-style-guess-all "guess-style" nil t))

(defun tddsg/init-buffer-clone ()
  (use-package buffer-clone))

(defun tddsg/init-framemove ()
  (use-package framemove)
  (framemove-default-keybindings))

(defun tddsg/init-llvm-mode ()
  (use-package llvm-mode))

;;; packages.el ends here
