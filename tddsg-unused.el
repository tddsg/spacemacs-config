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
