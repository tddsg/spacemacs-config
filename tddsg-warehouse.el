;;; package --- Summary

;;; Commentary:
;; This file contain a heap of unused code

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VARIOUS CONFIGURATION

;; isearch
(defun tddsg--isearch-show-case-fold (orig-func &rest args)
  (apply orig-func args)
  (if isearch-case-fold-search
      (spacemacs|diminish isearch-mode "⚡ISearch[ci]⚡")
    (spacemacs|diminish isearch-mode "⚡ISearch[CS]⚡")))
(advice-add 'isearch-mode :around #'tddsg--isearch-show-case-fold)
(advice-add 'isearch-repeat :around #'tddsg--isearch-show-case-fold)
(advice-add 'isearch-toggle-case-fold :around #'tddsg--isearch-show-case-fold)

;; scrolling
(spacemacs/toggle-smooth-scrolling-off)  ;; disable smooth-scrolling
(setq redisplay-dont-pause t
      scroll-conservatively 101
      scroll-margin 0                    ;; perfect setting for scrolling
      next-screen-context-lines 0        ;; perfect setting for scrolling
      scroll-preserve-screen-position 't)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INIT SPACELINE


(defvar tddsg--show-mode-line t)

;;; spaceline segments

(spaceline-define-segment tddsg/line-column
  "The current line and column numbers, or `(current page/number of pages)`
in pdf-view mode (enabled by the `pdf-tools' package)."
  (if (derived-mode-p 'pdf-view-mode)
      (format "(%d/%d)" (pdf-view-current-page) (pdf-cache-number-of-pages))
    "%l:%1c"))

(spaceline-define-segment tddsg/buffer-id
  "Name of buffer."
  (let ((buffer-id (s-trim (powerline-buffer-id 'mode-line-buffer-id))))
    (if (< (length buffer-id) 35) buffer-id
      (concat (substring buffer-id 0 30)
              (propertize "... " 'face 'mode-line-buffer-id)) )))

(spaceline-define-segment tddsg/current-time
  "Current time."
  (format-time-string "%I:%M%p"))

(spaceline-define-segment tddsg/hud
  "A HUD that shows which part of the buffer is currently visible."
  (when (string-match "\%" (format-mode-line "%p"))
    (powerline-hud highlight-face default-face 1))
  :tight t)

;; reuse code from spaceline-config.el
(defun tddsg--create-spaceline-theme (left second-left &rest additional-segments)
  "Convenience function for the spacemacs and emacs themes."
  (spaceline-install 'tddsg
    `(,left
      anzu
      auto-compile
      ,second-left
      major-mode
      ((version-control :when active)
       purpose )
      minor-modes
      (process :when active)
      ((flycheck-error flycheck-warning flycheck-info)
       :when active)
      (mu4e-alert-segment :when active)
      (erc-track :when active)
      (org-pomodoro :when active)
      (org-clock :when active)
      nyan-cat)
    `(which-function
      ;; (python-pyvenv :fallback python-pyenv)
      (battery :when active)
      selection-info
      ,@additional-segments
      ;; (input-method
      ;;  ;; (buffer-encoding-abbrev
      ;;  ;;  :when (and active (not (derived-mode-p 'pdf-view-mode))))
      ;;  (buffer-position :when active))
      tddsg/current-time ;; temporarily hide time
      ))
  (if (null mode-line-format)
      (setq mode-line-format '("%e" (:eval (spaceline-ml-tddsg))))
    (setq-default mode-line-format '("%e" (:eval (spaceline-ml-tddsg))))))

(defun tddsg--create-spaceline-final (&rest additional-segments)
  "Install the modeline used by Spacemacs.
ADDITIONAL-SEGMENTS are inserted on the right, between `global' and
`buffer-position'."
  (apply 'tddsg--create-spaceline-theme
         '((persp-name
            workspace-number
            window-number)
           :fallback evil-state
           :separator "|"
           :face highlight-face)
         '(buffer-modified
           tddsg/hud
           point-position
           tddsg/line-column
           ;; buffer-size
           tddsg/buffer-id
           remote-host)
         additional-segments))

(dolist (s '((tddsg-face-unmodified "SteelBlue3"
                                    "Unmodified buffer face.")
             (tddsg-face-modified "DarkGoldenrod2"
                                  "Modified buffer face.")
             (tddsg-face-read-only "SteelBlue3"
                                   "Read-only buffer face.")))
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

;; this function should be called from .spacemacs
(defun tddsg/config-spaceline ()
  (interactive)
  (if (not tddsg--show-mode-line) (setq mode-line-format nil)
    (progn
      (setq spaceline-highlight-face-func 'tddsg--spaceline-highlight-face)
      (tddsg--create-spaceline-final))))

(defun tddsg/toggle-mode-line()
  (interactive)
  (setq tddsg--show-mode-line (not tddsg--show-mode-line))
  (tddsg/config-spaceline)
  (force-mode-line-update))

(defun tddsg/toggle-presentation()
  (interactive)
  (if (or tddsg--show-header-line tddsg--show-mode-line)
      (setq tddsg--show-header-line nil
            tddsg--show-mode-line nil)
    (setq tddsg--show-header-line t
          tddsg--show-mode-line t))
  (tddsg--update-header-line)
  (tddsg/config-spaceline))

;; update mode line after every 60 seconds
(run-at-time 60 60 #'force-mode-line-update)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SHOW HEADER LINE

;; https://www.emacswiki.org/emacs/HeaderLine

(defvar tddsg--show-header-line t)

(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun tddsg--header-file-path ()
  "Create file path for the header line."
  (let* ((file-path (if buffer-file-name
                        (abbreviate-file-name buffer-file-name)
                      (buffer-name)))
         (dir-name  (if buffer-file-name
                        (file-name-directory file-path) ""))
         (file-name  (if buffer-file-name
                         (file-name-nondirectory buffer-file-name)
                       (buffer-name)))
         (path-len (length file-path))
         (name-len (length file-name))
         (dir-len (length dir-name))
         (drop-str "[...]")
         (path-display-len (- (window-body-width)
                              (length (projectile-project-name)) 3))
         (dir-display-len (- path-display-len (length drop-str) name-len 2)))
    (cond ((< path-len path-display-len)
           (concat "▷ "
                   (with-face dir-name :foreground "DeepSkyBlue3")
                   (with-face file-name :foreground "DarkOrange3")))
          ((and (> dir-len dir-display-len) (> dir-display-len 3))
           (concat "▷ "
                   (with-face (substring dir-name 0 (/ dir-display-len 2))
                              :foreground "DeepSkyBlue3")
                   (with-face drop-str :foreground "DeepSkyBlue3")
                   (with-face (substring dir-name
                                         (- dir-len (/ dir-display-len 2))
                                         (- dir-len 1))
                              :foreground "DeepSkyBlue3")
                   (with-face "/" :foreground "DeepSkyBlue3")
                   (with-face file-name :foreground "DarkOrange3")))
          (t (concat "▷ " (with-face file-name :foreground "DarkOrange3"))))))

(defun tddsg--header-project-path ()
  "Create project path for the header line."
  (if (tddsg--projectile-p)
      (concat "♖ "
              (with-face (projectile-project-name) :foreground "DarkOrange3")
              " ")
    ""))

;; set font of header line
(custom-set-faces
 '(header-line
   ((default :inherit mode-line)
    (((type tty))
     :foreground "black" :background "yellow" :inverse-video nil)
    (((class color grayscale) (background light))
     :background "grey90" :foreground "grey20" :box nil)
    (((class color grayscale) (background dark))
     :background "#212026" :foreground "gainsboro" :box nil)
    (((class mono) (background light))
     :background "white" :foreground "black"
     :inverse-video nil :box nil :underline t)
    (((class mono) (background dark))
     :background "black" :foreground "white"
     :inverse-video nil :box nil :underline t))))

(defun tddsg--create-header-line ()
  "Create the header line of a buffer."
  '("" ;; invocation-name
    (:eval
     (concat (tddsg--header-project-path)
             (tddsg--header-file-path)))))

(defun tddsg--update-header-line ()
  "Update header line of the active buffer and remove from all other."
  (defun exclude-buffer-p (buffer-name)
    (cl-loop for buffer-prefix in (list "*helm" "*spacemacs*")
             thereis (string-match-p (regexp-quote buffer-prefix) buffer-name)))
  (cl-loop for window in (window-list) do
           (with-current-buffer (window-buffer window)
             (when (not (exclude-buffer-p (buffer-name (window-buffer window))))
               (cond ((not tddsg--show-header-line)
                      (setq header-line-format nil))
                     ;; activate header-line of the active buffer
                     ((eq (window-buffer window) (window-buffer (selected-window)))
                      (setq header-line-format (tddsg--create-header-line)))
                     ;; dim header-line of inactive buffers
                     (t (setq header-line-format
                              `(:propertize ,(tddsg--create-header-line)
                                            face (:foreground "grey55")))))))))

(defun tddsg/toggle-header-line ()
  (interactive)
  (setq tddsg--show-header-line (not tddsg--show-header-line))
  (tddsg--update-header-line))

;; update header line of each buffer
(add-hook 'buffer-list-update-hook 'tddsg--update-header-line)
(add-hook 'window-configuration-change-hook 'tddsg--update-header-line)


;; show project in the mode line
(defun powerline-project-id (&optional face pad)
  (let ((location-name (if (tddsg--projectile-p) (projectile-project-name)
                         (file-name-nondirectory
                          (directory-file-name default-directory))))
        (location-message (directory-file-name default-directory)))
    (powerline-raw
     (format-mode-line
      (concat " "
              (propertize
		           (format-mode-line location-name)
		           'face face
		           'mouse-face 'mode-line-highlight
		           'help-echo location-message)))
     face pad)))
(spaceline-define-segment buffer-id
  "Name of buffer."
  (let ((face-format (if active 'mode-line-buffer-id
                       'mode-line-buffer-id-inactive)))
    (concat
     (s-trim (spaceline--string-trim-from-center
              (powerline-buffer-id face-format)
              spaceline-buffer-id-max-length))
     (if (buffer-file-name)
         (concat (if (tddsg--projectile-p) " @ " " ~ ")
                 (s-trim (spaceline--string-trim-from-center
                          (powerline-project-id face-format)
                          spaceline-buffer-id-max-length)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SHOW HEADER LINE

;; Reference: https://www.emacswiki.org/emacs/HeaderLine

(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun header-file-path ()
  "Create file path for the header line."
  (let* ((file-path (if buffer-file-name
                        (abbreviate-file-name buffer-file-name)
                      (buffer-name)))
         (dir-name  (if buffer-file-name
                        (file-name-directory file-path) ""))
         (file-name  (if buffer-file-name
                         (file-name-nondirectory buffer-file-name)
                       (buffer-name)))
         (path-len (length file-path))
         (name-len (length file-name))
         (dir-len (length dir-name))
         (drop-str "[...]")
         (path-display-len (- (window-body-width) 3))
         (dir-display-len (- path-display-len (length drop-str) name-len 2)))
    (cond ((< path-len path-display-len)
           (concat "▷ "
                   (with-face dir-name :foreground "DeepSkyBlue3")
                   (with-face file-name :foreground "DarkOrange3")))
          ((and (> dir-len dir-display-len) (> dir-display-len 3))
           (concat "▷ "
                   (with-face (substring dir-name 0 (/ dir-display-len 2))
                              :foreground "DeepSkyBlue3")
                   (with-face drop-str :foreground "DeepSkyBlue3")
                   (with-face (substring dir-name
                                         (- dir-len (/ dir-display-len 2))
                                         (- dir-len 1))
                              :foreground "DeepSkyBlue3")
                   (with-face "/" :foreground "DeepSkyBlue3")
                   (with-face file-name :foreground "DarkOrange3")))
          (t (concat "▷ " (with-face file-name :foreground "DarkOrange3"))))))

(defun update-header-line ()
  "Update header line of the active buffer and dim all others."
  (defun exclude-buffer-p (buffname)
    (or (string-prefix-p "*Helm" buffname)
        (string-prefix-p "*helm" buffname)
        (string-prefix-p "*Minibuf" buffname)
        (string-prefix-p "*spacemacs" buffname)))
  (when (not (exclude-buffer-p (buffer-name)))
    (setq header-line-format (header-file-path))))

;; update header line of each buffer
(add-hook 'window-configuration-change-hook 'update-header-line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CUSTOM NOTIFICATION

  ;; advice the message function
  (defvar notify-command nil)
  (defun notify-output (type command output)
    (when command
      (notifications-notify
       :title (format "%s" command)
       :urgency 'normal
       :body (format "%s%s" (if (equal type 'error) "ERROR OCCURS!!!\n" "")
                     output)))
    (when notify-command (setq notify-command nil)))
  (defun notify-message (orig-fun &rest args)
    (let ((output (apply orig-fun args)))
      (cond
       ;;; LaTeX
       ;; ((check-sub-string "LaTeX errors" output)
       ;;  (notify-output 'error "LaTeX" output))
       ;; ((or (check-sub-string "You should run LaTeX again" output)
       ;;      (check-sub-string "LaTeX: there were unresolved" output)
       ;;      (check-sub-string "LaTeX: successfully formatted" output))
       ;;  (notify-output 'success "LaTeX" output))
       ;; ((check-sub-string "BibTeX finished" output)
       ;;  (notify-output 'success "BibTeX" output))

       ;;; magit
       ((check-sub-string "Running git" output)
        (setq notify-command output))
       ((check-sub-string "Git finished" output)
        (notify-output 'success notify-command output))
       ((check-sub-string "Hit $ to see buffer magit-process" output)
        (notify-output 'error notify-command output))

       ;;; Compilation
       ((check-sub-string "Compiling: make" output)
        (setq notify-command output))
       ((check-sub-string "Compilation finished" output)
        (notify-output 'success notify-command output))
       ((check-sub-string "Compilation exited abnormally" output)
        (notify-output 'error notify-command output))
       
       )))
  (advice-add 'message :around #'notify-message)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VARIOUS FUNCTIONS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; show a message box:
(message-box "Compilation output: %s" status)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; show notifications using freedesktop.org

;; 1. Need to install the notification packages:
;;       sudo apt-get install notification-daemon
;;       sudo apt-get install xfce4-notifyd

;; 2. And run the notification service:
;;       systemctl --user enable xfce4-notifyd
;;       systemctl --user start xfce4-notifyd

(require 'notifications)

(notifications-notify :title (format "Title: %s" "this is the title!")
                      :urgency 'normal      ;; or 'critical
                      :timeout 3000
                      ;; :replaces-id nil
                      ;; :app-icon nil
                      :body (format "Message: %S" "This is the message!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add and remove advices

(advice-add 'magit-process-finish :around #'magit-notify)
(advice-remove 'magit-process-finish #'magit-notify)


