


(global-set-key (kbd "<detete>") 'delete-forward-char)
(global-set-key (kbd "C-<left>") 'left-word)
(global-set-key (kbd "C-<right>") 'right-word)
(global-set-key (kbd "M-<down>") '(lambda () (interactive) (next-line 5)))
(global-set-key (kbd "M-<up>") '(lambda () (interactive) (previous-line 5)))
(global-set-key (kbd "M-<left>") '(lambda () (interactive) (left-char 5)))
(global-set-key (kbd "M-<right>") '(lambda () (interactive) (right-char 5)))

(global-set-key (kbd "C-_") 'join-line)
(global-set-key (kbd "M-;") 'comment-dwim-2)
(global-set-key (kbd "M-k") 'sp-kill-sexp)
(global-set-key (kbd "C-M-k") 'tddsg-kill-line-backwards)
(global-set-key (kbd "S-<backspace>") 'crux-kill-whole-line)
(global-set-key (kbd "C-S-<backspace>") 'crux-kill-whole-line)
(global-set-key (kbd "M-s") 'sp-splice-sexp)
(global-set-key (kbd "M-S") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "C-s") 'isearch-forward)
(global-set-key (kbd "M-H") 'tddsg-select-current-line)
(global-set-key (kbd "M-h") 'mark-paragraph)
(global-set-key (kbd "C-x _") 'shrink-window)
(global-set-key (kbd "C-x m") 'monky-status)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x G") 'magit-diff)
(global-set-key (kbd "C-x f") 'helm-find)
(global-set-key (kbd "<escape>") 'god-local-mode)
(global-set-key (kbd "M-?") 'company-complete)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-[") 'helm-company)
(global-set-key (kbd "M-]") 'helm-dabbrev)
(global-set-key (kbd "M-m") 'tddsg-set-default-mode-line)

(global-set-key (kbd "C-c C-\\") 'goto-last-change)
(global-set-key (kbd "C-c C-\\") 'goto-last-change)
(global-set-key (kbd "C-c C-w") 'tddsg-save-file-as-and-open-file)
(global-set-key (kbd "C-c C-r") 'eval-region)
(global-set-key (kbd "C-c C-f") 'projectile-find-file)
(global-set-key (kbd "C-c C-g") 'helm-do-grep-ag)
(global-set-key (kbd "C-c C-i") 'helm-imenu-anywhere)
(global-set-key (kbd "C-c C-s") 'dictionary-search)
(global-set-key (kbd "C-c M-m") 'tddsg-shell-current-window)

(global-set-key (kbd "C-c f") 'helm-recentf)
(global-set-key (kbd "C-c m") 'tddsg-shell-other-window)
(global-set-key (kbd "C-c e") 'flyspell-mode)
(global-set-key (kbd "C-c o") 'helm-occur)
(global-set-key (kbd "C-c i") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-c u") 'crux-view-url)
(global-set-key (kbd "C-c k") 'crux-kill-other-buffers)
(global-set-key (kbd "C-c g") 'helm-projectile-grep)
(global-set-key (kbd "C-c s") 'flyspell-mode)
(global-set-key (kbd "C-c r") 'projectile-replace)
(global-set-key (kbd "C-c w") 'ace-window)
(global-set-key (kbd "C-c q") 'ace-delete-window)
(global-set-key (kbd "C-c |") 'vline-mode)
(global-set-key (kbd "C-c _") 'global-hl-line-mode)
(global-set-key (kbd "C-c t") 'transpose-frame)
(global-set-key (kbd "C-c v") 'visual-line-mode)
(global-set-key (kbd "C-c )") 'check-parens)
(global-set-key (kbd "C-c y") 'yafolding-mode)
(global-set-key (kbd "C-c C-SPC") 'tddsg-unpop-to-mark-command)
(global-set-key (kbd "C-c SPC") 'helm-all-mark-rings)

(global-set-key (kbd "C-c R") 'crux-rename-file-and-buffer)
(global-set-key (kbd "C-c D") 'crux-delete-file-and-buffer)

;; moving the cursor around windows (requiring windmove package)
(global-set-key (kbd "S-<left>") 'windmove-left)
(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<up>") 'windmove-up)
(global-set-key (kbd "S-<down>") 'windmove-down)

;; moving buffer around (requiring buffer-move package)
(global-set-key (kbd "C-s-S-<left>") 'buf-move-left)
(global-set-key (kbd "C-s-S-<right>") 'buf-move-right)
(global-set-key (kbd "C-s-S-<up>") 'buf-move-up)
(global-set-key (kbd "C-s-S-<down>") 'buf-move-down)
(global-set-key (kbd "C-c c J") 'buf-move-left)
(global-set-key (kbd "C-c c L") 'buf-move-right)
(global-set-key (kbd "C-c c I") 'buf-move-up)
(global-set-key (kbd "C-c c K") 'buf-move-down)

;; moving buffer around (requiring buffer-clone package)
(global-set-key (kbd "s-S-<left>") 'buf-clone-left)
(global-set-key (kbd "s-S-<right>") 'buf-clone-right)
(global-set-key (kbd "s-S-<up>") 'buf-clone-up)
(global-set-key (kbd "s-S-<down>") 'buf-clone-down)
(global-set-key (kbd "C-c c j") 'buf-clone-left)
(global-set-key (kbd "C-c c l") 'buf-clone-right)
(global-set-key (kbd "C-c c i") 'buf-clone-up)
(global-set-key (kbd "C-c c k") 'buf-clone-down)

;; define key for other minor mode
(define-key isearch-mode-map (kbd "C-.") 'tddsg-yank-current-word-to-isearch-buffer)
