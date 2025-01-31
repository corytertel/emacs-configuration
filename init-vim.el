
;;; Importing this file will turn emacs into a vim-like editor

(pkg 'evil)
(require 'evil)
(evil-mode 1)

;;; Evil M-x

(load "~/.emacs.d/evil-M-x.el")

;;; Evil Settings

;; (setq evil-want-C-u-scroll t)
(custom-set-variables
 '(evil-want-C-u-scroll t)
 '(evil-undo-system 'undo-redo))

;; Relative line numbers
(global-display-line-numbers-mode 1)
;; (setq display-line-numbers 'relative)
;; (setq display-line-numbers-type 'relative)

;; Cursor
(setq evil-default-cursor '(box "black"))
(setq evil-normal-state-cursor '(box "black"))
(setq evil-visual-state-cursor '(box "black"))
(setq evil-motion-state-cursor '(box "black"))
(setq evil-insert-state-cursor '((bar . 2) "green"))

;; Enable C-[
(define-key evil-insert-state-map (kbd "C-[") #'evil-normal-state)
(define-key evil-replace-state-map (kbd "C-[") #'evil-normal-state)

;; Make TAB have the same behavior as in emacs
(define-key evil-normal-state-map (kbd "TAB") #'indent-for-tab-command)
(define-key evil-visual-state-map (kbd "TAB") #'indent-for-tab-command)

;; Make A also indent
(defvar evil-indent-and-append-modes
  '(emacs-lisp-mode
    lisp-mode
    scheme-proc
    c-mode
    csharp-mode
    typescript-ts-mode
    tsx-ts-mode
    javascript-mode
    js-jsx-mode
    js-ts-mode))

(defun evil-append-line (count &optional vcount)
  "Switch to Insert state at the end of the current line.
The insertion will be repeated COUNT times.  If VCOUNT is non nil
it should be number > 0. The insertion will be repeated in the
next VCOUNT - 1 lines below the current one."
  (interactive "p")
  (when (memq major-mode evil-indent-and-append-modes)
    (indent-for-tab-command))
  (if (and visual-line-mode
           evil-respect-visual-line-mode)
      (evil-end-of-visual-line)
    (evil-move-end-of-line))
  (setq evil-insert-count count
        evil-insert-lines nil
        evil-insert-vcount
        (and vcount
             (> vcount 1)
             (list (line-number-at-pos)
                   #'end-of-line
                   vcount)))
  (evil-insert-state 1))

  ;; Make :q not quit emacs
  (global-set-key [remap evil-quit] #'kill-buffer-and-window)
  (global-set-key [remap evil-save-and-close] #'cory/kill-buffer-and-window-and-save)

  ;; Make : trigger M-x instead
  (define-key evil-motion-state-map (kbd ":") #'evil-execute-extended-command)
  ;; (define-key evil-motion-state-map (kbd ":")
  ;;   (lambda ()
  ;;     (interactive)
  ;;     ;; You will want vertico-preselect to be 'prompt for this command, or else you it
  ;;     ;; will be unintutive to submit evil-commands
  ;;     (let ((vertico-preselect 'prompt))
  ;;       (call-interactively #'evil-execute-extended-command))))
  (define-key evil-motion-state-map (kbd "C-w :") #'execute-extended-command) 

;;; Use devil as "leader key"

;; Using devil instead of any other "roll your own" leader key allows you
;; to reuse emacs binds. Now you have integration with all emacs packages
;; out of the box, no setup required. RYO leader keys require you to manually
;; rebind everything (a lot of meaningless work) and the result is esoteric.

;; TLDR devil makes evil compatible with the vanilla Emacs keymap (and packages which expect it)
;; No need for "specialized" packages for evil anymore

(pkg 'devil)
(require 'devil)

(define-key evil-motion-state-map (kbd "SPC") #'devil)
(global-set-key (kbd "C-x SPC") nil)
(global-set-key (kbd "C-x C-SPC") nil)

(setq devil-key " "
      devil-repeatable-keys nil
      devil-global-sets-buffer-default t
      ;; Use C-x binds as the leader map
      devil-translations '(("%k %k" . "C-x")
			   ("%k" . "C-x C-"))
      devil-special-keys '(("%k %k %k" . ignore) 
			   ("%k %k <escape>" . ignore)
			   ("%k <escape>" . ignore)))

;;; Escape as keyboard-quit

(define-key global-map [escape] #'keyboard-quit)
(define-key minibuffer-mode-map [escape] #'abort-minibuffers)
;; (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
