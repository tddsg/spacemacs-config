;;; proddag.el

;; Author: Ta Quang Trung
;; Version: 0.0.1
;; Created: 13 June 2017
;; Keywords: proddag
;; Homepage:

;;; Commentary:

;; short description here

;; full doc on how to use here

;;; Code:

(require 'smie)


;; define several category of keywords
(setq proddag-keywords
      '("abstract" "attribute" "entity" "enum"
        "extends" "field" "language"
        "typedef" "reference" "space" "struct"
        "vector"))

(setq proddag-types
      '("float" "int" "uint32" "boolean" "void" "string"))

(setq proddag-constants
      '("true" "false" ))

(setq proddag-predicates
      '("emp"))

;; generate regex string for each category of keywords
(setq proddag-keyword-regexp (regexp-opt proddag-keywords 'words))
(setq proddag-type-regexp (regexp-opt proddag-types 'words))
(setq proddag-constant-regexp (regexp-opt proddag-constants 'words))
(setq proddag-predicate-regexp (regexp-opt proddag-predicates 'words))
;; (setq proddag-functions-regexp proddag-functions)

;; create the list for font-lock.
;; each category of keyword is given a particular face
(setq proddag-font-lock-keywords
      `(
        (,proddag-type-regexp . font-lock-type-face)
        (,proddag-constant-regexp . font-lock-constant-face)
        ;; (,proddag-functions-regexp . font-lock-function-name-face)
        (,proddag-keyword-regexp . font-lock-keyword-face)
        (,proddag-predicate-regexp . font-lock-function-name-face)
        ;; note: order above matters, because once colored,
        ;; that part won't change.
        ;; in general, longer words first
        ))


(defvar proddag-syntax-table nil "Syntax table for `proddag'.")
(setq proddag-syntax-table
      (let ((syn-table (make-syntax-table)))
        ;; C++ style comment “// …”
        (modify-syntax-entry ?\/ ". 124" syn-table)
        (modify-syntax-entry ?* ". 23b" syn-table)
        (modify-syntax-entry ?\n ">" syn-table)
        ;; modify syntax table to use ' character as part of word
        (modify-syntax-entry ?' "w" syn-table)
        syn-table))

;; (defun proddag-smie-rules (kind token)
;;   (pcase (cons kind token)
;;     (`(:elem . basic) smie-indent-basic)
;;     ;; (`(,_ . ";") (smie-rule-separator kind))
;;     ;; (`(:after . ";") (smie-rule-bolp))
;;     (`(:before . ";") (- smie-indent-basic))
;;     ;; (`(:after . ";") (smie-rule-separator kind))
;;     (`(:before . "{")
;;      (if (smie-rule-hanging-p) (smie-rule-parent)))
;;     (`(:after . "}")
;;      (if (smie-rule-hanging-p) (smie-rule-parent)))
;;     ))

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
  ;; ;; higlight syntax for data structure
  ;; (font-lock-add-keywords 'proddag
  ;;                         '(("data\\s-*\\([a-zA-Z0-9_']+\\)\\s-*\{"
  ;;                            (1 font-lock-type-face))) t)

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
  (make-local-variable 'indent-tabs-mode)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'indent-region-function)
  (setq indent-tabs-mode nil)                      ;; insert spaces instead of tabs
  (setq indent-line-function 'indent-relative)     ;; indent line relative
  (setq indent-region-function (quote (lambda (begin end))))  ;; disable indent region
  ;; (setq indent-line-function (quote (lambda ())))  ;; disable indent line

  ;; set comment command
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-multi-line) nil)
  (set (make-local-variable 'comment-use-syntax) t)

  ;; indentation
  ;; (smie-setup nil #'proddag-smie-rules)

  (run-hooks 'proddag-hook))

;; clear memory. no longer needed
(setq proddag-keywords nil)
(setq proddag-types nil)
(setq proddag-constants nil)
;; (setq proddag-functions nil)

;; clear memory. no longer needed
(setq proddag-keyword-regexp nil)
(setq proddag-predicate-regexp nil)
(setq proddag-type-regexp nil)
(setq proddag-constant-regexp nil)
;; (setq proddag-function-regexp nil)


(or (assoc "\\.pdd$" auto-mode-alist)
    (setq auto-mode-alist (cons '("\\.sb$" . proddag) auto-mode-alist)))

;; add the mode to the `features' list
(provide 'proddag)

;; Local Variables:
;; coding: utf-8
;; End:

;;; proddag.el ends here
