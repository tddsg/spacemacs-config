;;; hummingbird.el

;; Copyright © December 2018, by Ta Quang Trung

;; Author: Ta Quang Trung
;; Version: 0.0.1
;; Created: 22 December 2018
;; Keywords: hummingbird
;; Homepage:

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; short description here

;; full doc on how to use here

;;; Code:

;; define several category of keywords
(setq hummingbird-keywords
      '("data" "pred" "view" "rel" "checkentails"))

(setq hummingbird-types
      '("float" "int" "bool" "void" "string"))

(setq hummingbird-constants
      '("null" "nil" "true" "false" "unknown" "valid" "invalid" "sat" "unsat"))

(setq hummingbird-predicates
      '("emp"))

;; generate regex string for each category of keywords
(setq hummingbird-keyword-regexp (regexp-opt hummingbird-keywords 'words))
(setq hummingbird-type-regexp (regexp-opt hummingbird-types 'words))
(setq hummingbird-constant-regexp (regexp-opt hummingbird-constants 'words))
(setq hummingbird-predicate-regexp (regexp-opt hummingbird-predicates 'words))
;; (setq hummingbird-functions-regexp hummingbird-functions)

;; create the list for font-lock.
;; each category of keyword is given a particular face
(setq hummingbird-font-lock-keywords
      `(
        (,hummingbird-type-regexp . font-lock-type-face)
        (,hummingbird-constant-regexp . font-lock-constant-face)
        ;; (,hummingbird-functions-regexp . font-lock-function-name-face)
        (,hummingbird-keyword-regexp . font-lock-keyword-face)
        (,hummingbird-predicate-regexp . font-lock-function-name-face)
        ;; note: order above matters, because once colored,
        ;; that part won't change.
        ;; in general, longer words first
        ))


(defvar hummingbird-syntax-table nil "Syntax table for `hummingbird'.")
(setq hummingbird-syntax-table
      (let ((syn-table (make-syntax-table)))
        ;; C++ style comment “// …”
        (modify-syntax-entry ?\/ ". 124" syn-table)
        (modify-syntax-entry ?* ". 23b" syn-table)
        (modify-syntax-entry ?\n ">" syn-table)
        ;; modify syntax table to use ' character as part of word
        (modify-syntax-entry ?' "w" syn-table)
        syn-table))

;;;;;;;;;;;;;;;;
;;; generic imenu for viewing outline

(setq hummingbird-imenu-generic-expression
      '(("Entailment"
         "^\\s-*checkentail\\s-+\\([a-zA-Z0-9_']+\\)\\s-*;"
         1)
        ("Entailment (exact)"
         "^\\s-*checkentail_exact\\s-+\\(.+\\)\\s-*\."
         1)
        ("Entailment"
         "^\\s-*checkentail\\s-+\\(.+\\)\\s-*\."
         1)
        ("Predicate"
         "^\\s-*pred\\s-+\\([a-zA-Z0-9_']+\\)\\s-*:"
         1)
        ("Predicate"
         "^\\s-*pred\\s-+\\([a-zA-Z0-9_']+\\)\\s-*<"
         1)
        ("Predicate (primitive)"
         "^\\s-*pred_prim\\s-+\\([a-zA-Z0-9_']+\\)\\s-*<"
         1)
        ("Data"
         "^\\s-*data\\s-+\\([a-zA-Z0-9_']+\\)\\s-*\{"
         1)))

(defun hummingbird-imenu-create-index ()
  (save-excursion
    (imenu--generic-function hummingbird-imenu-generic-expression)))

;;;###autoload
(define-derived-mode hummingbird prog-mode
  "hummingbird"
  "Major mode for editing Hummingbird/Hip/Sleek files"
  :syntax-table hummingbird-syntax-table

  ;; function to defined keywords in hummingbird-mode
  (defun new-keywords (params)
    (font-lock-add-keywords 'hummingbird params t))

  ;; code for syntax highlighting
  (setq font-lock-defaults '((hummingbird-font-lock-keywords)))

  ;; highlight syntax function and predicate
  (new-keywords '(("\\([a-zA-Z0-9_']+\\)\\s-*\("
                   (1 font-lock-function-name-face))))
  (new-keywords '(("::\\([a-zA-Z0-9_']+\\)\\s-*<"
                   (1 font-lock-function-name-face))))
  (new-keywords '(("pred\\s-*\\([a-zA-Z0-9_']+\\)\\s-*<"
                   (1 font-lock-function-name-face))))
  (new-keywords '(("pred_prim\\s-*\\([a-zA-Z0-9_']+\\)\\s-*<"
                   (1 font-lock-function-name-face))))
  ;; higlight syntax for data structure
  (new-keywords '(("data\\s-*\\([a-zA-Z0-9_']+\\)\\s-*\{"
                   (1 font-lock-type-face))))
  (new-keywords '(("\\([a-zA-Z0-9_']+\\)\\s-*\\([a-zA-Z0-9_']+\\);"
                   (1 font-lock-type-face))))
  (new-keywords '(("\\([a-zA-Z0-9_']+\\)\\s-*\\([a-zA-Z0-9_']+\\)\\s-*\}"
                   (1 font-lock-type-face))))
  ;; higlight syntax for data structure formula
  (new-keywords '(("->\\([a-zA-Z0-9_']+\\)\\s-*{"
                   (1 font-lock-type-face))))

  ;; indentation
  ;; indentation
  (setq-local indent-tabs-mode nil)                       ;; using space
  (setq-local indent-line-function 'indent-relative)      ;; indent line relative
  (setq-local indent-region-function '(lambda (x y) ()))  ;; disable indent region

  ;; set comment command
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-multi-line) nil)
  (set (make-local-variable 'comment-use-syntax) t)

  ;; imenu
  (setq-local imenu-create-index-function 'hummingbird-imenu-create-index)

  (run-hooks 'hummingbird-hook))

;; clear memory. no longer needed
(setq hummingbird-keywords nil)
(setq hummingbird-types nil)
(setq hummingbird-constants nil)
;; (setq hummingbird-functions nil)

;; clear memory. no longer needed
(setq hummingbird-keyword-regexp nil)
(setq hummingbird-predicate-regexp nil)
(setq hummingbird-type-regexp nil)
(setq hummingbird-constant-regexp nil)
;; (setq hummingbird-function-regexp nil)


(or (assoc "\\.sb$" auto-mode-alist)
    (setq auto-mode-alist (cons '("\\.sb$" . hummingbird) auto-mode-alist)))

(or (assoc "\\.slk$" auto-mode-alist)
    (setq auto-mode-alist (cons '("\\.slk$" . hummingbird) auto-mode-alist)))

(or (assoc "\\.ss$" auto-mode-alist)
    (setq auto-mode-alist (cons '("\\.ss$" . hummingbird) auto-mode-alist)))

;; add the mode to the `features' list
(provide 'hummingbird)

;; Local Variables:
;; coding: utf-8
;; End:

;;; hummingbird.el ends here
