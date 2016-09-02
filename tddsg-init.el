;;; package --- Summary

;;; Commentary:

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTILITIES FUNCTIONS

(defun tddsg/shell-other-window (&optional buffer)
  "Open a `shell' in a new window."
  (interactive)
  (let ((old-buf (current-buffer))
        (current-prefix-arg 4) ;; allow using C-u
        (shell-buf (call-interactively 'shell)))
    (switch-to-buffer-other-window shell-buf)
    (switch-to-buffer old-buf)
    (other-window 1)))

(defun tddsg/shell-current-window (&optional buffer)
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

(defun tddsg/save-file-as-and-open-file (filename &optional confirm)
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

(defun tddsg/mark-line ()
  "Select current line"
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(defun tddsg/smart-mark-sexp ()
  "Expand region or mark sexp"
  (interactive)
  (let ((current-char (char-after)))
    (cond ((= ?w (char-syntax current-char))
           (call-interactively 'er/expand-region))
          ((= ?_ (char-syntax current-char))
           (call-interactively 'er/expand-region))
          ((= ?\) (char-syntax current-char))
           (progn
             (forward-char)
             (backward-sexp)
             (call-interactively 'mark-sexp)))
          (t (call-interactively 'mark-sexp)))))

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

(defun tddsg/unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))

(defun tddsg-set-mark ()
  (interactive)
  (push-mark (point) t nil))

(defun tddsg/enable-company-auto-suggest ()
  (interactive)
  (require 'company)
  (setq company-idle-delay 0.3))

(defun tddsg/disable-company-auto-suggest ()
  (interactive)
  (require 'company)
  (setq company-idle-delay 300))

(defun tddsg-buffer-focus ()
  (if (derived-mode-p 'text-mode 'tuareg-mode)
      (tddsg/disable-company-auto-suggest)
    (tddsg/enable-company-auto-suggest)))

(defun tddsg-hook-prog-text-mode ()
  (linum-mode 1)
  (column-marker-1 80)
  (whitespace-mode 1))

(defun tddsg-hook-prog-mode ()
  (flycheck-mode 1))

(defun tddsg-hook-text-mode ()
  (flyspell-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INIT CONFIGS

(defun tddsg/init-configs ()
  ;; visual interface setting
  (blink-cursor-mode 1)             ;; turn on blinking
  (setq blink-cursor-blinks 15)     ;; blink 15 times
  (set-face-background hl-line-face "honeydew")
  (setq scroll-margin 5)            ;; top-bottom margin for scrolling
  (setq-default fill-column 80)
  (setq text-scale-mode-step 1.1)   ;; scale changing font size
  (setq frame-title-format          ;; frame title
        '("" invocation-name " - "
          (:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name)) "%b"))))

  ;; mode paragraph setting
  (setq paragraph-separate "[ \t\f]*$"
        paragraph-start "\f\\|[ \t]*$")

  ;; spell
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra")
        ispell-dictionary "english"
        prelude-flyspell nil)

  ;; automatically setting mark for certain commands
  (setq global-mark-ring-max 1000
        mark-ring-max 200)
  (defadvice find-file (before set-mark activate) (tddsg-set-mark))
  (defadvice isearch-update (before set-mark activate) (tddsg-set-mark))
  (defadvice beginning-of-buffer (before set-mark activate) (tddsg-set-mark))
  (defadvice end-of-buffer (before set-mark activate) (tddsg-set-mark))
  (defadvice merlin-locate (before set-mark activate) (tddsg-set-mark))

  ;; advice on buffer focusing
  (defadvice other-window (after update activate) (tddsg-buffer-focus))
  (defadvice windmove-do-window-select (after update activate) (tddsg-buffer-focus))
  (defadvice split-window (after update activate) (tddsg-buffer-focus))
  (defadvice set-buffer (after update activate) (tddsg-buffer-focus))
  (defadvice switch-to-buffer (after update activate) (tddsg-buffer-focus))
  (defadvice save-buffer (after update activate) (tddsg-buffer-focus))
  (defadvice pop-to-buffer (after update activate) (tddsg-buffer-focus))
  (defadvice previous-buffer (after update activate) (tddsg-buffer-focus))
  (defadvice next-buffer (after update activate) (tddsg-buffer-focus))
  (defadvice keyboard-quit (after update activate) (tddsg-buffer-focus))

  ;; spacemacs
  (setq shell-default-shell 'ansi-term)

  ;; mode editing setting
  (electric-pair-mode t)
  (delete-selection-mode t)                ;; delete selection by keypress
  (setq require-final-newline t)           ;; newline at end of file
  (defadvice newline                       ;; indent after new line
      (after newline-after activate)
    (indent-according-to-mode))
  ;; (setq company-idle-delay 200)         ;; set delay time by default
  (global-company-mode)

  ;; some Emacs threshold
  (setq max-lisp-eval-depth 10000)
  (setq max-specpdl-size 10000)

  ;; mode-line setting
  (require 'powerline)
  (setq powerline-default-separator 'wave)

  ;; compilation
  (setq compilation-ask-about-save nil)
  (setq compilation-window-height 10)

  ;; shell
  (setq comint-prompt-read-only nil)
  (defadvice shell (after linum activate) (linum-mode 1))

  ;; enable hl-todo
  (global-hl-todo-mode 1)

  ;; diminish
  (eval-after-load "abbrev" '(diminish 'abbrev-mode " ↹"))
  (eval-after-load "whitespace" '(diminish 'whitespace-mode " S"))
  (eval-after-load "whitespace-mode" '(diminish 'whitespace-mode " R"))
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
  (eval-after-load "projectile" '(diminish 'projectile-mode " π"))

  ;; hooks, finally hook
  (add-hook 'prog-mode-hook 'tddsg-hook-prog-text-mode)
  (add-hook 'text-mode-hook 'tddsg-hook-prog-text-mode)
  (add-hook 'prog-mode-hook 'tddsg-hook-prog-mode)
  (add-hook 'text-mode-hook 'tddsg-hook-text-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INIT KEYS

(defun tddsg/init-keys ()
  (global-set-key (kbd "<home>") 'crux-move-beginning-of-line)
  (global-set-key (kbd "<detete>") 'delete-forward-char)
  (global-set-key (kbd "<escape>") 'god-local-mode)
  (global-set-key (kbd "C-S-<backspace>") 'kill-whole-line)
  (global-set-key (kbd "C-<backspace>") 'crux-kill-line-backwards)
  (global-set-key (kbd "C-M-k") 'sp-kill-sexp)
  (global-set-key (kbd "C-M-SPC") 'tddsg/smart-mark-sexp)
  (global-set-key (kbd "C-<left>") 'left-word)
  (global-set-key (kbd "C-<right>") 'right-word)
  (global-set-key (kbd "C-+") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  (global-set-key (kbd "C-/") 'undo)
  (global-set-key (kbd "C-S-/") 'undo-tree-redo)
  (global-set-key (kbd "C-^") 'crux-top-join-line)
  (global-set-key (kbd "C-_") 'join-line)
  (global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
  (global-set-key (kbd "C-c d") 'crux-duplicate-current-line-or-region)

  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x C-b") 'helm-mini)
  (global-set-key (kbd "C-x _") 'shrink-window)
  (global-set-key (kbd "C-x m") 'monky-status)
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x G") 'magit-diff)

  (global-set-key (kbd "C-x w s") 'tddsg/save-file-as-and-open-file)
  (global-set-key (kbd "C-c C-SPC") 'tddsg/unpop-to-mark-command)
  (global-set-key (kbd "C-c m") 'tddsg/shell-other-window)
  (global-set-key (kbd "C-c M-m") 'tddsg/shell-current-window)

  (global-set-key (kbd "M-S-<up>") 'move-text-up)
  (global-set-key (kbd "M-S-<down>") 'move-text-down)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "M-s p") 'check-parens)
  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "M-;") 'comment-dwim-2)
  (global-set-key (kbd "M-?") 'company-complete)
  (global-set-key (kbd "M-H") 'tddsg/mark-line)
  (global-set-key (kbd "M-[") 'helm-company)
  (global-set-key (kbd "M-]") 'helm-dabbrev)

  (global-set-key (kbd "M-m h g") 'helm-do-grep-ag)
  (global-set-key (kbd "M-m h o") 'helm-occur)
  (global-set-key (kbd "M-m h s") 'helm-semantic-or-imenu)
  (global-set-key (kbd "M-m s d") 'dictionary-search)
  (global-set-key (kbd "M-m S s") 'flyspell-mode)
  (global-set-key (kbd "M-m S l") 'langtool-check)
  (global-set-key (kbd "M-m m s") 'shell)
  (global-set-key (kbd "M-m w t") 'transpose-frame)

  (require 'buffer-move)
  (global-set-key (kbd "C-s-S-<left>") 'buf-move-left)
  (global-set-key (kbd "C-s-S-<right>") 'buf-move-right)
  (global-set-key (kbd "C-s-S-<up>") 'buf-move-up)
  (global-set-key (kbd "C-s-S-<down>") 'buf-move-down)

  (global-set-key (kbd "C-M-1") 'spacemacs/workspaces-transient-state/eyebrowse-switch-to-window-config-1-and-exit)
  (global-set-key (kbd "C-M-2") 'spacemacs/workspaces-transient-state/eyebrowse-switch-to-window-config-2-and-exit)
  (global-set-key (kbd "C-M-3") 'spacemacs/workspaces-transient-state/eyebrowse-switch-to-window-config-3-and-exit)
  (global-set-key (kbd "C-M-4") 'spacemacs/workspaces-transient-state/eyebrowse-switch-to-window-config-4-and-exit)
  (global-set-key (kbd "C-M-5") 'spacemacs/workspaces-transient-state/eyebrowse-switch-to-window-config-5-and-exit)
  (global-set-key (kbd "C-M-6") 'spacemacs/workspaces-transient-state/eyebrowse-switch-to-window-config-6-and-exit)
  (global-set-key (kbd "C-M-7") 'spacemacs/workspaces-transient-state/eyebrowse-switch-to-window-config-7-and-exit)
  (global-set-key (kbd "C-M-8") 'spacemacs/workspaces-transient-state/eyebrowse-switch-to-window-config-8-and-exit)
  (global-set-key (kbd "C-M-9") 'spacemacs/workspaces-transient-state/eyebrowse-switch-to-window-config-9-and-exit)
  (global-set-key (kbd "C-M-0") 'spacemacs/workspaces-transient-state/eyebrowse-switch-to-window-config-0-and-exit)

  (global-set-key (kbd "C-M-+") 'spacemacs/workspaces-transient-state/eyebrowse-next-window-config)
  (global-set-key (kbd "C-M--") 'spacemacs/workspaces-transient-state/eyebrowse-prev-window-config)
  (global-set-key (kbd "C-x M-<right>") 'spacemacs/workspaces-transient-state/eyebrowse-next-window-config)
  (global-set-key (kbd "C-x M-<left>") 'spacemacs/workspaces-transient-state/eyebrowse-prev-window-config)


  (define-key isearch-mode-map (kbd "C-.") 'tddsg/yank-current-word-to-isearch-buffer)
  (define-key minibuffer-local-map (kbd "C-.") 'tddsg/yank-current-word-to-minibuffer)
  (define-key shell-mode-map (kbd "C-j") 'newline)
  (define-key undo-tree-map (kbd "C-_") nil)

  ;; god-mode
  (require 'god-mode)
  (require 'god-mode-isearch)
  (define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
  (define-key isearch-mode-map (kbd "C-z") 'god-mode-isearch-activate)
  (define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)
  (define-key god-mode-isearch-map (kbd "C-z") 'god-mode-isearch-disable)
  (define-key god-local-mode-map (kbd "<escape>") 'god-local-mode)

  ;; company mode
  (require 'company)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-.") 'company-show-location)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INIT THEMES

(defcustom tddsg-themes nil
  "Association list of override faces to set for different custom themes.")

(defun tddsg-read-custom-themes (alist-symbol key value)
  "Set VALUE of a KEY in ALIST-SYMBOL."
  (set alist-symbol
        (cons (list key value) (assq-delete-all key (eval alist-symbol)))))

;; override some settings of the leuven theme
(tddsg-read-custom-themes
 'tddsg-themes
 ;; update leuven
 'leuven
 '((cursor ((t (:background "lime green"))))
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
   ;; mode-line
   (powerline-active1 ((t (:inherit mode-line :background "#163365")))))
 ;; other themes
 )

(defun tddsg-override-theme ()
  (dolist (theme-settings tddsg-themes)
    (let ((theme (car theme-settings))
          (faces (cadr theme-settings)))
      (if (member theme custom-enabled-themes)
          (dolist (face faces)
            (custom-theme-set-faces theme face))))))

(defun tddsg/init-themes ()
  ;; load the theme
  (tddsg-override-theme)
  ;; and defadvice load-theme function
  (defadvice load-theme (after theme-set-overrides activate)
    "Set override faces for different custom themes."
    (tddsg-override-theme)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INIT SPACELINE

(require 'spaceline-segments)

;; reuse code from spaceline-config.el
(defun tddsg--create-spaceline-theme (left second-left &rest additional-segments)
  "Convenience function for the spacemacs and emacs themes."
  (spaceline-install 'tddsg
                     `(,left
                       anzu
                       auto-compile
                       ,second-left
                       major-mode
                       (process :when active)
                       ((flycheck-error flycheck-warning flycheck-info)
                        :when active)
                       (minor-modes :when active)
                       (mu4e-alert-segment :when active)
                       (erc-track :when active)
                       (version-control :when active)
                       (org-pomodoro :when active)
                       (org-clock :when active)
                       nyan-cat)
                     `(which-function
                       (python-pyvenv :fallback python-pyenv)
                       (battery :when active)
                       selection-info
                       input-method
                       ((buffer-encoding-abbrev))
                       (global :when active)
                       ,@additional-segments
                       buffer-position
                       hud))

  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-tddsg)))))

;;; used for setting pdf-view page
;;; FIXME: to be removed when spaceline is updated
(declare-function pdf-view-current-page 'pdf-view)
(declare-function pdf-cache-number-of-pages 'pdf-view)

(defun tddsg--pdfview-page-number ()
  (format "(%d/%d)"
          (eval (pdf-view-current-page))
          (pdf-cache-number-of-pages)))

(spaceline-define-segment line-column
  "The current line and column numbers, or `(current page/number of pages)`
in pdf-view mode (enabled by the `pdf-tools' package)."
  (if (eq 'pdf-view-mode major-mode)
      (tddsg--pdfview-page-number)
    "%l:%2c"))


(defun tddsg--create-spaceline-final (&rest additional-segments)
  "Install the modeline used by Spacemacs.

ADDITIONAL-SEGMENTS are inserted on the right, between `global' and
`buffer-position'."
  (require 'spaceline)
  (require 'pdf-view)
  (apply 'tddsg--create-spaceline-theme
         '((persp-name
            workspace-number
            window-number)
           :fallback evil-state
           :separator "|"
           :face highlight-face)
         '(buffer-modified
           point-position
           line-column
           ;; buffer-size
           buffer-id
           remote-host)
         additional-segments))

(dolist (s '((tddsg-face-unmodified "SteelBlue3" "Unmodified buffer face.")
             (tddsg-face-modified "DarkGoldenrod2" "Modified buffer face.")
             (tddsg-face-read-only "SteelBlue3" "Read-only buffer face.")))
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


(defun tddsg/init-spaceline ()
  (setq spaceline-highlight-face-func 'tddsg--spaceline-highlight-face)
  (tddsg--create-spaceline-final))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INIT CUSTOM

(defun tddsg/init-custom-vars ()
  (custom-set-variables
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
       ("HACK" . "red")
       ("FIXME" . "red")
       ("XXX" . "red")
       ("XXXX" . "red")
       ("???" . "red")
       ("BUG" . "red"))))))
