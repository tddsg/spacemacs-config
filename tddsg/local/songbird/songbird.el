;;; songbird.el

;; Copyright © July 2016, by Ta Quang Trung

;; Author: Ta Quang Trung
;; Version: 0.0.1
;; Created: 13 July 2016
;; Keywords: songbird
;; Homepage:

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; short description here

;; full doc on how to use here

;;; Code:

;; define several category of keywords
(setq songbird-keywords
      '("if" "else" "while" "return" "break" "catch" "try" "with" "static"
        "global" "or" "and" "exists" "forall" "ref" "class" "extends"
        "this" "throws" "raise"
        "data" "axiom" "relation"
        "pred" "pred_prim" "pred_prop" "inv"
        "lemma" "lemma_prop" "lemma_split" "lemma_test" "lemma_unsafe"
        "lemma_infer" "lemma_safe"
        "checkentail" "checkentail_exact" "checkentail_inexact"
        "check_nondet" "checksat" "checkeq" ))
(setq songbird-types
      '("float" "int" "bool" "void" "string"))
(setq songbird-constants
      '("null" "nil" "true" "false" "emp"))
;;; TODO: somehow, defining function syntax doesn't work here.
;;; Currently, it is directly defined inside the syntax table.
;; (setq songbird-functions
;;       '("\\([a-zA-Z0-9_']+\\)\\s-*\("))

;; generate regex string for each category of keywords
(setq songbird-keywords-regexp (regexp-opt songbird-keywords 'words))
(setq songbird-type-regexp (regexp-opt songbird-types 'words))
(setq songbird-constant-regexp (regexp-opt songbird-constants 'words))
;; (setq songbird-functions-regexp songbird-functions)

;; create the list for font-lock.
;; each category of keyword is given a particular face
(setq songbird-font-lock-keywords
      `(
        (,songbird-type-regexp . font-lock-type-face)
        (,songbird-constant-regexp . font-lock-constant-face)
        ;; (,songbird-event-regexp . font-lock-builtin-face)
        ;; (,songbird-functions-regexp . font-lock-function-name-face)
        (,songbird-keywords-regexp . font-lock-keyword-face)
        ;; note: order above matters, because once colored, that part won't change.
        ;; in general, longer words first
        ))


(defvar songbird-syntax-table nil "Syntax table for `songbird'.")
(setq songbird-syntax-table
      (let ((syn-table (make-syntax-table)))
        ;; C++ style comment “// …”
        (modify-syntax-entry ?\/ ". 124" syn-table)
        (modify-syntax-entry ?* ". 23b" syn-table)
        (modify-syntax-entry ?\n ">" syn-table)
        ;; modify syntax table to use ' character as part of word
        (modify-syntax-entry ?' "w" syn-table)
        syn-table))

;;;###autoload
(define-derived-mode songbird prog-mode
  "songbird"
  "Major mode for editing Songbird/Hip/Sleek files"
  :syntax-table songbird-syntax-table

  ;; code for syntax highlighting
  (setq font-lock-defaults '((songbird-font-lock-keywords)))

  ;; highlight syntax function and predicate
  (font-lock-add-keywords 'songbird
                          '(("\\([a-zA-Z0-9_']+\\)\\s-*\("
                             (1 font-lock-function-name-face)))
                          t)
  ;; higlight syntax for data structure declaration
  (font-lock-add-keywords 'songbird
                          '(("data\\s-*\\([a-zA-Z0-9_']+\\)\\s-*\{"
                             (1 font-lock-type-face)))
                          t)
  ;; higlight syntax for data structure formula
  (font-lock-add-keywords 'songbird
                          '(("::\\([a-zA-Z0-9_']+\\)\\s-*\<"
                             (1 font-lock-type-face)))
                          t)
  ;; highlight syntax variable declaration
  (font-lock-add-keywords 'songbird ;
                          '(("\\([a-zA-Z0-9_']+\\)\\s-+\\(\\([a-zA-Z0-9_']+\\)\\s-*\\)+;"
                             (1 font-lock-type-face)))
                          t)

  (setq indent-tabs-mode nil)               ;; insert spaces instead of tabs

  ;; set comment command
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-multi-line) nil)
  (set (make-local-variable 'comment-use-syntax) t)

  (run-hooks 'songbird-hook))

;; clear memory. no longer needed
(setq songbird-keywords nil)
(setq songbird-types nil)
(setq songbird-constants nil)
(setq songbird-events nil)
(setq songbird-functions nil)

;; clear memory. no longer needed
(setq songbird-keywords-regexp nil)
(setq songbird-types-regexp nil)
(setq songbird-constants-regexp nil)
(setq songbird-events-regexp nil)
(setq songbird-functions-regexp nil)


(or (assoc "\\.sb$" auto-mode-alist)
    (setq auto-mode-alist (cons '("\\.sb$" . songbird) auto-mode-alist)))

(or (assoc "\\.slk$" auto-mode-alist)
    (setq auto-mode-alist (cons '("\\.slk$" . songbird) auto-mode-alist)))

(or (assoc "\\.ss$" auto-mode-alist)
    (setq auto-mode-alist (cons '("\\.ss$" . songbird) auto-mode-alist)))

;; add the mode to the `features' list
(provide 'songbird)

;; Local Variables:
;; coding: utf-8
;; End:

;;; songbird.el ends here
