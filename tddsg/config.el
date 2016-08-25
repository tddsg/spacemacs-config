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



