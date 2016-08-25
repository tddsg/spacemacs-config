;; visual interface setting
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

(use-package songbird-mode
  :load-path "private/tddsg/"
  :mode (("\\.slk\\'" . songbird-mode)
         ("\\.ss\\'" . songbird-mode)
         ("\\.sb\\'" . songbird-mode))
  :config
  (defun my-songbird-hook ()
    ;; customize syntax table for slurping/barfing parentheses
    (dolist (symbol (list ?. ?, ?\; ?: ?+ ?- ?@ ?! ?> ?<))
      (modify-syntax-entry symbol "'" songbird-syntax-table)))
  (add-hook 'songbird-mode-hook 'my-songbird-hook 'append))


(use-package buffer-clone  :load-path "personal/modules/")

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

