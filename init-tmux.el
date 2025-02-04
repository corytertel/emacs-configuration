
;;; Tmux-like Navigation
;;; Make Emacs window-management identical to Tmux window-management for consistency
;;; Importing this file will make emacs have tmux-like window management

(require 'window)
(require 'windmove)
(require 'keymap)
(require 'evil-states)

(load "~/.emacs.d/init-tmux-utils.el")

(setq windmove-wrap-around t)

;; Define our new tmux inspired keymap
(defvar-keymap window-map
  :doc "Keymap for window-management related commands."
  ;; TODO next layout
  "%" #'split-and-follow-right
  "\"" #'split-and-follow-below
  "C-o" #'rotate-windows
  "M-o" #'rotate-windows-reverse ; for no reason, this doesn't bind
  "C-x" #'delete-other-windows-prompt ; not in tmux, but it's too useful to me
  "x" #'delete-window-prompt
  "o" #'other-window
  "t" #'world-clock
  "E" #'balance-windows
  "<left>" #'windmove-left
  "<right>" #'windmove-right
  "<up>" #'windmove-up
  "<down>" #'windmove-down
  "C-<left>" #'move-border-left
  "C-<right>" #'move-border-right
  "C-<up>" #'move-border-up
  "C-<down>" #'move-border-down
  "M-<left>" (lambda () (interactive) (move-border-left 5))
  "M-<right>" (lambda () (interactive) (move-border-right 5))
  "M-<up>" (lambda () (interactive) (move-border-up 5))
  "M-<down>" (lambda () (interactive) (move-border-down 5))
  "{" #'windmove-swap-states-up
  "}" #'windmove-swap-states-down
  "?" (lambda () (interactive) (describe-variable #'window-map))
  ;; ESC and C-g should get you out of everything
  "ESC" #'ignore
  "C-g" #'ignore)

;; Use new window-map in evil states
(define-key evil-normal-state-map (kbd "C-b") window-map)
(define-key evil-emacs-state-map (kbd "C-b") window-map)
(define-key evil-motion-state-map (kbd "C-b") window-map)
(define-key evil-insert-state-map (kbd "C-b") window-map)
(define-key evil-visual-state-map (kbd "C-b") window-map)
(define-key evil-replace-state-map (kbd "C-b") window-map)
(define-key evil-operator-state-map (kbd "C-b") window-map)

;; Unbind default emacs window management binds
(global-set-key (kbd "C-x o") nil)
(global-set-key (kbd "C-x 0") nil)
(global-set-key (kbd "C-x 1") nil)
(global-set-key (kbd "C-x 2") nil)
(global-set-key (kbd "C-x 3") nil)
(global-set-key (kbd "C-x 4") nil)
(global-set-key (kbd "C-x 5") nil)
