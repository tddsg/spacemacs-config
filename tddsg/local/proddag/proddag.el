;;; proddag.el

;; Author: Ta Quang Trung
;; Version: 0.0.1
;; Created: 13 June 2017
;; Keywords: proddag
;; Homepage:

;;; Commentary:

;;; This package provide a major mode to edit the source code file of protoddag
;;
;;  Current supported features:
;;    - Syntax highlighting
;;    - Outline viewing by Emacs Imenu
;;
;;  TODO:
;;    - Syntax parsing for a more structured outline view and syntax checking.
;;    - Auto-indentation

;;; Code:

;; define proddag keywors
(setq proddag-keywords
      '("abstract" "attribute" "entity" "enum"
        "extends" "field" "language"
        "typedef" "reference" "space" "struct"
        "vector"))

;; define proddag primitive types
(setq proddag-types
      '("float" "int" "uint32" "boolean" "void" "string"))

;; define proddag primitive constants
(setq proddag-constants
      '("true" "false" ))

;; generate regex string for each category of keywords
(setq proddag-keyword-regexp (regexp-opt proddag-keywords 'words))
(setq proddag-type-regexp (regexp-opt proddag-types 'words))
(setq proddag-constant-regexp (regexp-opt proddag-constants 'words))

;; create the list for font-lock.
;; each category of keyword is given a particular face
(setq proddag-font-lock-keywords
      `((,proddag-type-regexp . font-lock-type-face)
        (,proddag-constant-regexp . font-lock-constant-face)
        (,proddag-keyword-regexp . font-lock-keyword-face)))

(defvar proddag-syntax-table nil "Syntax table for `proddag'.")
(setq proddag-syntax-table
      (let ((syn-table (make-syntax-table)))
        ;; C++ style comment “//...”
        (modify-syntax-entry ?\/ ". 124" syn-table)
        (modify-syntax-entry ?* ". 23b" syn-table)
        (modify-syntax-entry ?\n ">" syn-table)
        syn-table))

;;;;;;;;;;;;;;;;
;;; generic imenu for viewing outline

(setq proddag-imenu-generic-expression
      '(("Entity"  "^\\s-*entity\\s-*\\([a-zA-Z0-9_']+\\)\\s-*extends\\s-*" 1)
        ("Abstract Entity"  "^\\s-*abstract\\s-*entity\\s-*\\([a-zA-Z0-9_']+\\)\\s-*{" 1)
        ("Space"  "^\\s-*space\\s-*\\([a-zA-Z0-9_']+\\)\\s-*{" 1)
        ("Type"  "^\\s-*typedef\\s-*\\([a-zA-Z0-9_']+\\)\\s-*{" 1)
        ("Type"  "^\\s-*struct\\s-*\\([a-zA-Z0-9_']+\\)\\s-*{" 1)
        ("Type"  "^\\s-*enum\\s-*\\([a-zA-Z0-9_']+\\)\\s-*{" 1)))

(defun proddag-imenu-create-index ()
  (save-excursion
    (imenu--generic-function proddag-imenu-generic-expression)))

;;;;;;;;;;;;;;;;
;;; Main mode

;;;###autoload
(define-derived-mode proddag prog-mode "proddag"
  "Major mode for editing protoddag"
  :syntax-table proddag-syntax-table

  ;; code for syntax highlighting
  (setq font-lock-defaults '((proddag-font-lock-keywords)))

  ;; highlight syntax function and predicate
  (font-lock-add-keywords 'proddag
                          '(("\\([a-zA-Z0-9_']+\\)\\s-*\("
                             (1 font-lock-function-name-face))) t)

  ;; highlight for type
  (font-lock-add-keywords 'proddag
                          '(("entity\\s-*\\([a-zA-Z0-9_']+\\)\\s-*"
                             (1 font-lock-type-face))) t)
  (font-lock-add-keywords 'proddag
                          '(("extends\\s-*\\([a-zA-Z0-9_']+\\)\\s-*{"
                             (1 font-lock-type-face))) t)
  (font-lock-add-keywords 'proddag
                          '(("typedef\\s-*\\([a-zA-Z0-9_']+\\)\\s-*{"
                             (1 font-lock-type-face))) t)
  (font-lock-add-keywords 'proddag
                          '(("struct\\s-*\\([a-zA-Z0-9_']+\\)\\s-*{"
                             (1 font-lock-type-face))) t)
  (font-lock-add-keywords 'proddag
                          '(("enum\\s-*\\([a-zA-Z0-9_']+\\)\\s-*{"
                             (1 font-lock-type-face))) t)
  (font-lock-add-keywords 'proddag
                          '(("reference\\s-*(\\s-*\\([a-zA-Z0-9_']+\\)\\s-*)"
                             (1 font-lock-type-face))) t)
  (font-lock-add-keywords 'proddag
                          '(("typedef\\s-*(\\s-*\\([a-zA-Z0-9_']+\\)\\s-*)"
                             (1 font-lock-type-face))) t)
  (font-lock-add-keywords 'proddag
                          '(("struct\\s-*(\\s-*\\([a-zA-Z0-9_']+\\)\\s-*)"
                             (1 font-lock-type-face))) t)
  (font-lock-add-keywords 'proddag
                          '(("enum\\s-*(\\s-*\\([a-zA-Z0-9_']+\\)\\s-*)"
                             (1 font-lock-type-face))) t)
  (font-lock-add-keywords 'proddag
                          '(("attribute\\s-*[a-zA-Z0-9_']+\\s-*=\\s-*\\([a-zA-Z0-9_']+\\)"
                             (1 font-lock-type-face))) t)

  ;; highlight for field, attribute, language, space
  (font-lock-add-keywords 'proddag
                          '(("field\\s-*\\([a-zA-Z0-9_']+\\)\\s-*:"
                             (1 font-lock-variable-name-face))) t)
  (font-lock-add-keywords 'proddag
                          '(("attribute\\s-*\\([a-zA-Z0-9_']+\\)\\s-*="
                             (1 font-lock-variable-name-face))) t)
  (font-lock-add-keywords 'proddag
                          '(("language\\s-*\\([a-zA-Z0-9_']+\\)\\s-*{"
                             (1 font-lock-warning-face))) t)
  (font-lock-add-keywords 'proddag
                          '(("space\\s-*\\([a-zA-Z0-9_']+\\)\\s-*{"
                             (1 font-lock-warning-face))) t)

  ;; constant
  (font-lock-add-keywords 'proddag
                          '(("\\s-*\\([a-zA-Z0-9_']+\\)\\s-*;"
                             (1 font-lock-constant-face))) t)

  ;; indentation
  (setq-local indent-tabs-mode nil)                       ;; using space
  (setq-local indent-line-function 'indent-relative)      ;; indent line relative
  (setq-local indent-region-function '(lambda (x y) ()))  ;; disable indent region

  ;; set comment command
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (setq-local comment-multi-line nil)
  (setq-local comment-use-syntax t)

  ;; imenu
  (setq-local imenu-create-index-function 'proddag-imenu-create-index)

  (run-hooks 'proddag-hook))

;; clear memory. no longer needed
(setq proddag-keywords nil)
(setq proddag-types nil)
(setq proddag-constants nil)

;; clear memory. no longer needed
(setq proddag-keyword-regexp nil)
(setq proddag-type-regexp nil)
(setq proddag-constant-regexp nil)


;; bind to file *.pdd
(or (assoc "\\.pdd$" auto-mode-alist)
    (setq auto-mode-alist (cons '("\\.pdd$" . proddag) auto-mode-alist)))

;; add the mode to the `features' list
(provide 'proddag)

;; Local Variables:
;; coding: utf-8
;; End:

;;; proddag.el ends here
