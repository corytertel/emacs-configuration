
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
;; (setq-default
 ;; mode-line-format
 ;; '("%e" mode-line-front-space
   ;; (:propertize
    ;; ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
    ;; display
    ;; (min-width
     ;; (5.0)))
   ;; mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position evil-mode-line-tag
   ;; (vc-mode vc-mode)
   ;; "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))


(defface modeline-narrow-face
  `((t (:foreground "#141404" :background "#ed8f23")))
  "Todo/fixme highlighting."
  :group 'faces)

(defface modeline-read-only-face
  `((t (:foreground "#141404" :background "#9feaae")))
  "Read-only buffer highlighting."
  :group 'faces)

(defface modeline-modified-face
  `((t (:foreground "#d8d8d8" :background "#e60909")))
  "Modified buffer highlighting."
  :group 'faces)

;; (setq-default
;;  mode-line-modified
;;  '(list (propertize
;; 	 "%1*"
;; 	 'help-echo 'mode-line-read-only-help-echo
;; 	 'local-map (purecopy (make-mode-line-mouse-map
;; 			       'mouse-1
;; 			       #'mode-line-toggle-read-only))
;; 	 'mouse-face 'mode-line-highlight)
;; 	(propertize
;; 	 "%1+"
;;          'face 'modeline-modified-face
;; 	 'help-echo 'mode-line-modified-help-echo
;; 	 'local-map (purecopy (make-mode-line-mouse-map
;; 			       'mouse-1 #'mode-line-toggle-modified))
;; 	 'mouse-face 'mode-line-highlight)))

;; Select only "real buffers" when toggling between buffers
(set-frame-parameter (selected-frame) 'buffer-predicate
		     (lambda (buf)
		       (let ((name (buffer-name buf)))
			 (not (or (string-prefix-p "*" name)
			       (eq 'dired-mode (buffer-local-value 'major-mode buf)))))))

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
