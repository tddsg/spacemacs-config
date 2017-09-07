;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;; spacemacs
     better-defaults
     spacemacs-layouts
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;; languages
     org
     markdown
     emacs-lisp
     (c-c++ :variables c-c++-enable-clang-support t)
     latex
     ocaml
     haskell
     yaml
     perl5
     csv
     python
     html
     javascript
     ;; (reason-mode
     ;;  :location (recipe
     ;;             :repo "arichiardi/reason-mode"
     ;;             :fetcher github
     ;;             :files ("reason-mode.el" "refmt.el")))
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;; utilities
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     auto-completion
     git
     version-control
     spell-checking
     syntax-checking
     semantic
     pdf-tools
     semantic
     cscope
     search-engine
     (gtags :variables gtags-enable-by-default nil)
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;; personal
     tddsg)
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'emacs
   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         leuven
                         monokai
                         zenburn
                         material
                         spacemacs-light
                         default
                         sanityinc-tomorrow-eighties
                         sanityinc-tomorrow-night
                         sanityinc-tomorrow-day
                         cyberpunk
                         flatui
                         espresso
                         tango
                         whiteboard
                         adwaita
                         ample
                         afternoon
                         soothe
                         wombat)
   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("DejaVu Sans Mono"
                               :size 18
                               :weight normal
                               :width normal
                               :powerline-scale 1.25)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands.
   dotspacemacs-auto-generate-layout-names nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non-nil the paste micro-state is enabled. When enabled pressing `p'
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   dotspacemacs-frame-title-format "%I@%S"
   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil
   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
 )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (load-file "~/.emacs.d/private/tddsg-init.el")
  (tddsg/config-packages)
  (tddsg/config-keys)
  (tddsg/config-themes)
  (tddsg/config-spaceline)
  (tddsg/config-custom-vars))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-begin-regexp "begin\\b\\|\\[\\|If\\b\\|For\\b")
 '(LaTeX-command "latex --synctex=1")
 '(LaTeX-end-regexp "end\\b\\|\\]\\|\\EndIf\\b\\|EndFor\\b")
 '(LaTeX-indent-environment-list
   (quote
    (("verbatim" current-indentation)
     ("verbatim*" current-indentation)
     ("longtable" LaTeX-indent-tabular)
     ("Form" current-indentation)
     ("tabular")
     ("tabular*")
     ("align")
     ("align*")
     ("array")
     ("eqnarray")
     ("eqnarray*")
     ("displaymath")
     ("equation")
     ("equation*")
     ("picture")
     ("tabbing")
     ("figure")
     ("center")
     ("flushleft")
     ("flushright")
     ("small"))))
 '(TeX-save-query nil)
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-list (quote (("pdf-tools" "TeX-pdf-tools-sync-view"))))
 '(TeX-view-program-selection (quote ((output-pdf "pdf-tools"))))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#3F3F3F" "dark violet" "blue" "orange" "forest green" "firebrick" "dodger blue" "dim gray"])
 '(compilation-message-face (quote default))
 '(electric-pair-open-newline-between-pairs nil t)
 '(evil-want-Y-yank-to-eol nil)
 '(golden-ratio-exclude-buffer-names
   (quote
    ("*which-key*" "*LV*" "*NeoTree*" "*ace-popup-menu*" "*compilation*")))
 '(helm-ag-insert-at-point (quote symbol) t)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#20240E" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#20240E" . 100))))
 '(hl-sexp-background-color "#efebe9")
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
     ("TRUNG" . "red")
     ("HACK" . "red")
     ("FIXME" . "red")
     ("XXX" . "red")
     ("XXXX" . "red")
     ("???" . "red")
     ("BUG" . "red")
     ("OK" . "red"))))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (yaml-mode framemove llvm-mode yapfify xterm-color winum web-mode web-beautify vline uuidgen utop unfill tuareg caml transpose-frame toc-org tagedit swiper-helm super-save stickyfunc-enhance srefactor sr-speedbar smeargle slim-mode shell-pop scss-mode sass-mode pytest pyenv-mode py-isort pug-mode pip-requirements pdf-tools tablist paradox orgit org-projectile org-present org-pomodoro alert log4e gntp org-download org-bullets ocp-indent noflet mwim multi-term monky mmm-mode merlin markdown-toc markdown-mode magit-gitflow livid-mode skewer-mode simple-httpd live-py-mode link-hint less-css-mode latex-extra langtool key-chord json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc intero imenu-anywhere hydra hy-mode htmlize hlint-refactor hindent hide-comnt helm-rtags helm-pydoc helm-hoogle helm-gtags helm-gitignore request helm-dired-history helm-css-scss helm-cscope xcscope helm-company helm-c-yasnippet haskell-snippets haml-mode god-mode gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md ggtags fuzzy flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck-haskell flycheck eyebrowse evil-visual-mark-mode evil-unimpaired evil-magit magit magit-popup git-commit with-editor evil-ediff eshell-z eshell-prompt-extras esh-help emmet-mode elpy pyvenv find-file-in-project elmacro dumb-jump disaster dired+ diff-hl dictionary link connection cython-mode csv-mode crux counsel swiper ivy company-web web-completion-data company-tern dash-functional tern company-statistics company-rtags rtags company-math math-symbol-lists company-irony-c-headers company-irony irony company-ghci company-ghc ghc haskell-mode company-cabal company-c-headers company-auctex company-anaconda company comment-dwim-2 column-marker column-enforce-mode coffee-mode cmm-mode cmake-mode clang-format auto-yasnippet yasnippet auto-dictionary auctex-latexmk auctex anaconda-mode pythonic f adaptive-wrap ace-popup-menu avy-menu ac-ispell auto-complete org-plus-contrib ws-butler window-numbering volatile-highlights vi-tilde-fringe spaceline s powerline smooth-scrolling restart-emacs rainbow-delimiters popwin persp-mode pcre2el spinner page-break-lines open-junk-file neotree move-text macrostep lorem-ipsum linum-relative leuve-ntheme info+ indent-guide ido-vertical-mode hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-args evil-anzu anzu eval-sexp-fu highlight elisp-slime-nav define-word clean-aindent-mode buffer-move bracketed-paste auto-highlight-symbol auto-compile packed dash aggressive-indent ace-window ace-link ace-jump-helm-line helm avy helm-core popup async quelpa package-build use-package which-key bind-key bind-map evil spacemacs-theme)))
 '(paradox-github-token t)
 '(pdf-view-continuous nil)
 '(pdf-view-midnight-colors (quote ("#D3D3D3" . "#292B2E")))
 '(popwin:popup-window-height 15)
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(sp-highlight-wrap-overlay nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(compilation-error ((t (:foreground "red" :weight bold))))
 '(compilation-warning ((t (:inherit warning :weight bold))))
 '(header-line ((default :inherit mode-line) (((type tty)) :foreground "black" :background "yellow" :inverse-video nil) (((class color grayscale) (background light)) :background "grey90" :foreground "grey20" :box nil) (((class color grayscale) (background dark)) :background "#212026" :foreground "gainsboro" :box nil) (((class mono) (background light)) :background "white" :foreground "black" :inverse-video nil :box nil :underline t) (((class mono) (background dark)) :background "black" :foreground "white" :inverse-video nil :box nil :underline t)))
 '(hi-green ((t (:background "dark olive green" :foreground "white smoke"))))
 '(hi-yellow ((t (:background "dark goldenrod" :foreground "white smoke"))))
 '(isearch ((t (:background "dark orange" :foreground "#292b2e"))))
 '(lazy-highlight ((t (:background "LightGoldenrod3" :foreground "gray10" :weight normal))))
 '(sp-pair-overlay-face ((t nil)))
 '(sp-wrap-overlay-face ((t nil)))
 '(sp-wrap-tag-overlay-face ((t nil)))
 '(term-color-green ((t (:foreground "#67d11d")))))
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-begin-regexp "begin\\b\\|\\[\\|If\\b\\|For\\b")
 '(LaTeX-command "latex --synctex=1")
 '(LaTeX-end-regexp "end\\b\\|\\]\\|\\EndIf\\b\\|EndFor\\b")
 '(LaTeX-font-list
   (quote
    ((100 "" "" t)
     (117 "\\textup{" "}")
     (116 "\\texttt{" "}" "\\mathtt{" "}")
     (115 "\\textsl{" "}" "\\mathbb{" "}")
     (114 "\\textrm{" "}" "\\mathrm{" "}")
     (110 "\\textnormal{" "}" "\\mathnormal{" "}")
     (109 "\\textmd{" "}")
     (105 "\\textit{" "}" "\\mathit{" "}")
     (102 "\\textsf{" "}" "\\mathsf{" "}")
     (12 "\\underline{" "}")
     (101 "\\emph{" "}")
     (99 "\\textsc{" "}")
     (98 "\\textbf{" "}" "\\mathbf{" "}")
     (97 "" "" "\\mathcal{" "}")
     (100 "" "" t)
     (117 "\\textup{" "}")
     (116 "\\texttt{" "}" "\\mathtt{" "}")
     (115 "\\textsl{" "}" "\\mathbb{" "}")
     (114 "\\textrm{" "}" "\\mathrm{" "}")
     (110 "\\textnormal{" "}" "\\mathnormal{" "}")
     (109 "\\textmd{" "}")
     (105 "\\textit{" "}" "\\mathit{" "}")
     (102 "\\textsf{" "}" "\\mathsf{" "}")
     (101 "\\emph{" "}")
     (99 "\\textsc{" "}")
     (98 "\\textbf{" "}" "\\mathbf{" "}")
     (97 "" "" "\\mathcal{" "}")
     (100 "" "" t)
     (117 "\\textup{" "}")
     (116 "\\texttt{" "}" "\\mathtt{" "}")
     (115 "\\textsl{" "}" "\\mathbb{" "}")
     (114 "\\textrm{" "}" "\\mathrm{" "}")
     (110 "\\textnormal{" "}" "\\mathnormal{" "}")
     (109 "\\textmd{" "}")
     (105 "\\textit{" "}" "\\mathit{" "}")
     (102 "\\textsf{" "}" "\\mathsf{" "}")
     (101 "\\emph{" "}")
     (99 "\\textsc{" "}")
     (98 "\\textbf{" "}" "\\mathbf{" "}")
     (97 "" "" "\\mathcal{" "}")
     (100 "" "" t)
     (117 "\\textup{" "}")
     (116 "\\texttt{" "}" "\\mathtt{" "}")
     (115 "\\textsl{" "}" "\\mathbb{" "}")
     (114 "\\textrm{" "}" "\\mathrm{" "}")
     (110 "\\textnormal{" "}" "\\mathnormal{" "}")
     (109 "\\textmd{" "}")
     (105 "\\textit{" "}" "\\mathit{" "}")
     (102 "\\textsf{" "}" "\\mathsf{" "}")
     (101 "\\emph{" "}")
     (99 "\\textsc{" "}")
     (98 "\\textbf{" "}" "\\mathbf{" "}")
     (97 "" "" "\\mathcal{" "}")
     (100 "" "" t)
     (117 "\\textup{" "}")
     (116 "\\texttt{" "}" "\\mathtt{" "}")
     (115 "\\textsl{" "}" "\\mathbb{" "}")
     (114 "\\textrm{" "}" "\\mathrm{" "}")
     (110 "\\textnormal{" "}" "\\mathnormal{" "}")
     (109 "\\textmd{" "}")
     (105 "\\textit{" "}" "\\mathit{" "}")
     (102 "\\textsf{" "}" "\\mathsf{" "}")
     (101 "\\emph{" "}")
     (99 "\\textsc{" "}")
     (98 "\\textbf{" "}" "\\mathbf{" "}")
     (97 "" "" "\\mathcal{" "}")
     (100 "" "" t)
     (117 "\\textup{" "}")
     (116 "\\texttt{" "}" "\\mathtt{" "}")
     (115 "\\textsl{" "}" "\\mathbb{" "}")
     (114 "\\textrm{" "}" "\\mathrm{" "}")
     (110 "\\textnormal{" "}" "\\mathnormal{" "}")
     (109 "\\textmd{" "}")
     (105 "\\textit{" "}" "\\mathit{" "}")
     (102 "\\textsf{" "}" "\\mathsf{" "}")
     (101 "\\emph{" "}")
     (99 "\\textsc{" "}")
     (98 "\\textbf{" "}" "\\mathbf{" "}")
     (97 "" "" "\\mathcal{" "}")
     (1 "" "" "\\mathcal{" "}")
     (2 "\\textbf{" "}" "\\mathbf{" "}")
     (3 "\\textsc{" "}")
     (5 "\\emph{" "}")
     (6 "\\textsf{" "}" "\\mathsf{" "}")
     (9 "\\textit{" "}" "\\mathit{" "}")
     (13 "\\textmd{" "}")
     (14 "\\textnormal{" "}" "\\mathnormal{" "}")
     (18 "\\textrm{" "}" "\\mathrm{" "}")
     (19 "\\textsl{" "}" "\\mathbb{" "}")
     (20 "\\texttt{" "}" "\\mathtt{" "}")
     (21 "\\textup{" "}"))))
 '(LaTeX-indent-environment-list
   (quote
    (("verbatim" current-indentation)
     ("verbatim*" current-indentation)
     ("longtable" LaTeX-indent-tabular)
     ("Form" current-indentation)
     ("tabular")
     ("tabular*")
     ("align")
     ("align*")
     ("array")
     ("eqnarray")
     ("eqnarray*")
     ("displaymath")
     ("equation")
     ("equation*")
     ("picture")
     ("tabbing")
     ("figure")
     ("center")
     ("flushleft")
     ("flushright")
     ("small"))))
 '(TeX-font-list
   (quote
    ((2 "{\\bf " "}")
     (3 "{\\sc " "}")
     (5 "{\\em " "\\/}")
     (9 "{\\it " "\\/}")
     (18 "{\\rm " "}")
     (19 "{\\sl " "\\/}")
     (20 "{\\tt " "}")
     (4 "" "" t)
     (12 "\\underline{" "}"))))
 '(TeX-save-query nil)
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-list (quote (("pdf-tools" "TeX-pdf-tools-sync-view"))))
 '(TeX-view-program-selection (quote ((output-pdf "pdf-tools"))))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#3F3F3F" "dark violet" "blue" "orange" "forest green" "firebrick" "dodger blue" "dim gray"])
 '(auto-fill-inhibit-regexp "")
 '(compilation-message-face (quote default))
 '(electric-pair-open-newline-between-pairs nil t)
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "#14151E" t)
 '(golden-ratio-exclude-buffer-names
   (quote
    ("*which-key*" "*LV*" "*NeoTree*" "*ace-popup-menu*" "*compilation*")))
 '(google-translate-default-target-language "en" t)
 '(helm-ag-insert-at-point (quote symbol) t)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#20240E" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#20240E" . 100))))
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")) t)
 '(hl-sexp-background-color "#efebe9")
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
     ("TRUNG" . "red")
     ("HACK" . "red")
     ("FIXME" . "red")
     ("XXX" . "red")
     ("XXXX" . "red")
     ("???" . "red")
     ("BUG" . "red")
     ("OK" . "red"))))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (cmake-ide levenshtein material-theme zenburn-theme goto-last-change company-shell zone-sl writegood-mode bash-completion helm-ispell engine-mode popwin org-brain evil-org dedicated shx helm-ag dante helm-bibtex biblio parsebib biblio-core gandalf-theme monokai-theme symon string-inflection realgud test-simple loc-changes load-relative password-generator helm-purpose window-purpose imenu-list evil-lion editorconfig browse-at-remote yaml-mode framemove llvm-mode yapfify xterm-color winum web-mode web-beautify vline uuidgen utop unfill tuareg caml transpose-frame toc-org tagedit swiper-helm super-save stickyfunc-enhance srefactor sr-speedbar smeargle slim-mode shell-pop scss-mode sass-mode pytest pyenv-mode py-isort pug-mode pip-requirements pdf-tools tablist paradox orgit org-projectile org-present org-pomodoro alert log4e gntp org-download org-bullets ocp-indent noflet mwim multi-term monky mmm-mode merlin markdown-toc markdown-mode magit-gitflow livid-mode skewer-mode simple-httpd live-py-mode link-hint less-css-mode latex-extra langtool key-chord json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc intero imenu-anywhere hydra hy-mode htmlize hlint-refactor hindent hide-comnt helm-rtags helm-pydoc helm-hoogle helm-gtags helm-gitignore request helm-dired-history helm-css-scss helm-cscope xcscope helm-company helm-c-yasnippet haskell-snippets haml-mode god-mode gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md ggtags fuzzy flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck-haskell flycheck eyebrowse evil-visual-mark-mode evil-unimpaired evil-magit magit magit-popup git-commit with-editor evil-ediff eshell-z eshell-prompt-extras esh-help emmet-mode elpy pyvenv find-file-in-project elmacro dumb-jump disaster dired+ diff-hl dictionary link connection cython-mode csv-mode crux counsel swiper ivy company-web web-completion-data company-tern dash-functional tern company-statistics company-rtags rtags company-math math-symbol-lists company-irony-c-headers company-irony irony company-ghci company-ghc ghc haskell-mode company-cabal company-c-headers company-auctex company-anaconda company comment-dwim-2 column-marker column-enforce-mode coffee-mode cmm-mode cmake-mode clang-format auto-yasnippet yasnippet auto-dictionary auctex-latexmk auctex anaconda-mode pythonic f adaptive-wrap ace-popup-menu avy-menu ac-ispell auto-complete org-plus-contrib ws-butler window-numbering volatile-highlights vi-tilde-fringe spaceline s powerline smooth-scrolling restart-emacs rainbow-delimiters persp-mode pcre2el spinner page-break-lines open-junk-file neotree move-text macrostep lorem-ipsum linum-relative leuve-ntheme info+ indent-guide ido-vertical-mode hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-args evil-anzu anzu eval-sexp-fu highlight elisp-slime-nav define-word clean-aindent-mode buffer-move bracketed-paste auto-highlight-symbol auto-compile packed dash aggressive-indent ace-window ace-link ace-jump-helm-line helm avy helm-core popup async quelpa package-build use-package which-key bind-key bind-map evil spacemacs-theme)))
 '(paradox-github-token t)
 '(pdf-view-continuous nil)
 '(pdf-view-midnight-colors (quote ("#D3D3D3" . "#292B2E")))
 '(popwin:popup-window-height 15)
 '(popwin:special-display-config
   (quote
    (("*compilation*" :width 1.0 :height 0.3 :position bottom :noselect t)
     ("^\\*Flycheck.+\\*$" :regexp t :position bottom :noselect t :dedicated t :stick t)
     ("^*WoMan.+*$" :regexp t :position bottom)
     ("*nosetests*" :position bottom :noselect nil :dedicated t :stick t)
     ("*grep*" :position bottom :noselect nil :dedicated t :stick t)
     ("*ert*" :position bottom :noselect nil :dedicated t :stick t)
     (" *undo-tree*" :height 0.4 :position bottom :noselect nil :dedicated t :stick t)
     ("*Async Shell Command*" :position bottom :noselect nil :dedicated t :stick t)
     ("*Shell Command Output*" :position bottom :noselect nil :dedicated t :stick t)
     ("*Help*" :height 0.4 :position bottom :noselect t :dedicated t :stick t))))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(pupo-split-active-window t)
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(sp-highlight-wrap-overlay nil)
 '(super-save-mode t)
 '(super-save-triggers
   (quote
    ("switch-to-buffer" "other-window" "windmove-up" "windmove-down" "windmove-left" "windmove-right" "select-window-by-number" "previous-buffer" "next-buffer")))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(compilation-error ((t (:foreground "red" :weight bold))))
 '(compilation-warning ((t (:inherit warning :weight bold))))
 '(flycheck-error ((t (:underline (:color "#e0211d" :style wave) :weight normal))))
 '(flycheck-info ((t (:underline (:color "#4f97d7" :style wave) :weight normal))))
 '(flycheck-warning ((t (:underline (:color "#dc752f" :style wave) :weight normal))))
 '(font-latex-bold-face ((t (:foreground "tomato" :weight bold))))
 '(header-line ((default :inherit mode-line) (((type tty)) :foreground "black" :background "yellow" :inverse-video nil) (((class color grayscale) (background light)) :background "grey90" :foreground "grey20" :box nil) (((class color grayscale) (background dark)) :background "#212026" :foreground "gainsboro" :box nil) (((class mono) (background light)) :background "white" :foreground "black" :inverse-video nil :box nil :underline t) (((class mono) (background dark)) :background "black" :foreground "white" :inverse-video nil :box nil :underline t)))
 '(hi-green ((t (:background "dark olive green" :foreground "white smoke"))))
 '(hi-yellow ((t (:background "dark goldenrod" :foreground "white smoke"))))
 '(isearch ((t (:background "dark orange" :foreground "#292b2e"))))
 '(lazy-highlight ((t (:background "LightGoldenrod3" :foreground "gray10" :weight normal))))
 '(rtags-fixitline ((t (:underline (:color "red" :style wave) :slant normal :weight extra-bold))))
 '(rtags-warnline ((t (:underline (:color "dark orange" :style wave) :weight extra-bold))))
 '(sp-pair-overlay-face ((t nil)))
 '(sp-wrap-overlay-face ((t nil)))
 '(sp-wrap-tag-overlay-face ((t nil)))
 '(term-color-green ((t (:foreground "#67d11d")))))
)
