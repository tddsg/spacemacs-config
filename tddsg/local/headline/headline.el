;;; headline.el

;; Author: Ta Quang Trung
;; Version: 0.0.1
;; Created: 22 July 2017
;; Keywords: header-line

;;; Commentary:

;;; This package provide a minor mode for header line of Emacs

;;; Acknowledgement:
;;    This package is originally inspired by the idea and the code
;;    from Emacswiki: https://www.emacswiki.org/emacs/HeaderLine
;;    and from Spaceline


;;; Code:

(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

;;; TODO;
(defmacro headline-define-segment (name value &rest props)
  )

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

(defun header-project-path ()
  "Create project path for the header line."
  (if (projectile-p)
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

(defun create-header-line ()
  "Create the header line of a buffer."
  '("" ;; invocation-name
    (:eval
     (concat (header-project-path)
             (header-file-path)))))

(defun update-header-line ()
  "Update header line of the active buffer and remove from all other."
  (defun exclude-buffer-p (buffer-name)
    (cl-loop for buffer-prefix in (list "*helm" "*spacemacs*")
             thereis (string-match-p (regexp-quote buffer-prefix) buffer-name)))
  (cl-loop for window in (window-list) do
           (with-current-buffer (window-buffer window)
             (when (not (exclude-buffer-p (buffer-name (window-buffer window))))
               (cond ((not show-header-line)
                      (setq header-line-format nil))
                     ;; activate header-line of the active buffer
                     ((eq (window-buffer window) (window-buffer (selected-window)))
                      (setq header-line-format (create-header-line)))
                     ;; dim header-line of inactive buffers
                     (t (setq header-line-format
                              `(:propertize ,(create-header-line)
                                            face (:foreground "grey55")))))))))

(defun toggle-header-line ()
  (interactive)
  (setq show-header-line (not show-header-line))
  (update-header-line))

;; update header line of each buffer
(add-hook 'buffer-list-update-hook 'update-header-line)
(add-hook 'window-configuration-change-hook 'update-header-line)

;; Local Variables:
;; coding: utf-8
;; End:

;;; headline.el ends here
