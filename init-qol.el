
;;; Quality of life changes in emacs

;; Turn off bell
(setq visible-bell nil
      ring-bell-function #'ignore)

;; Write backup files to backup directory instead of editing directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Replace "yes or no" prompts with "y or n" prompts
(defalias 'yes-or-no-p #'y-or-n-p
  "Use `y-or-n-p' instead of a yes/no prompt.")

;; Set custom file so init.el does not get poluted
(setq custom-file "~/.emacs.d/custom.el")

;; Never convert spaces to tabs when indenting
(setq-default indent-tabs-mode nil)

;; Minibuffer completion quality of life
(defun kill-dir-or-char ()
  "Kill backward by word for directories else by char"
  (interactive)
  (if (looking-back "/")
      (backward-kill-sexp 1)
    (backward-delete-char 1)))

(define-key minibuffer-local-completion-map
            (kbd "DEL") #'kill-dir-or-char)

;; Position and format of completions window
(add-to-list 'display-buffer-alist
             '("\\*Completions\\*"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (window-parameters . ((mode-line-format . none)))))

;; Mode line colors
(set-face-attribute 'mode-line nil
                    :foreground "black"
                    :background "#99dd99"
                    :box '(:line-width 3 :color "#99dd99" :style nil)
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
                    :foreground "black"
                    :background "gray90"
                    :box '(:line-width 3 :color "gray90" :style nil)
                    :overline nil
                    :underline nil)

;; NOTE need to be careful with :eval in modeline
(setq-default
 mode-line-format
 (list
  " "
  '(:eval (char-to-string (upcase (string-to-char (symbol-name evil-state)))))
  " "
  '(:eval (if (buffer-modified-p)
              (propertize "%b" 'face 'font-lock-warning-face)
            "%b"))))

;; Select only "real buffers" when toggling between buffers
;; (set-frame-parameter (selected-frame) 'buffer-predicate
;;                      (lambda (buf)
;;                        (let ((name (buffer-name buf)))
;;                          (not (or (string-prefix-p "*" name)
;;                                (eq 'dired-mode (buffer-local-value 'major-mode buf)))))))

(global-set-key (kbd "<f1>") #'mode-line-other-buffer)
(global-set-key (kbd "C-<f1>") #'mode-line-other-buffer)
(global-set-key (kbd "M-<f1>") #'mode-line-other-buffer)
(global-set-key (kbd "C-M-<f1>") #'mode-line-other-buffer)
(global-set-key (kbd "S-<f1>") #'mode-line-other-buffer)
(global-set-key (kbd "C-S-<f1>") #'mode-line-other-buffer)
(global-set-key (kbd "M-S-<f1>") #'mode-line-other-buffer)
(global-set-key (kbd "C-M-S-<f1>") #'mode-line-other-buffer)

;; Suppress async-shell-command popup
(add-to-list 'display-buffer-alist
             '("\\*Async Shell Command\\*.*" display-buffer-no-window))
