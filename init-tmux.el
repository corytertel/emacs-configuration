
;;; Importing this file will make emacs have tmux-like window management

(defun split-and-follow-below ()
  "Open a new window vertically."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun split-and-follow-right ()
  "Open a new window horizontally."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun kill-all-buffers-and-windows ()
  "Kill all buffers and windows."
  (interactive)
  (when (yes-or-no-p "Really kill all buffers and windows? ")
    (save-some-buffers)
    (mapc 'kill-buffer (buffer-list))
    (delete-other-windows)))

(defun delete-window-prompt ()
  "Prompt the user to delete currently focused window."
  (interactive)
  (when (yes-or-no-p "Delete window?")
    (delete-window nil)))

(defun delete-other-windows-prompt ()
  "Prompt the user to make the currently focused window fill its frame."
  (interactive)
  (cond ((not (> (count-windows) 1))
         (message "No other windows exist on the current frame."))
        (t
         (when (yes-or-no-p "Delete other windows?")
           (delete-other-windows nil nil)))))

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

;; FIXME
(defun rotate-windows-reverse ()
  "Rotate your windows in the reverse direction"
  (interactive)
  (cond ((not (> (count-windows) 1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (- (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
				    (car next-win-edges))
                                 (<= (cadr this-win-edges)
				    (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; Tmux-like Navigation
;; Make Emacs window-management identical to Tmux window-management for consistency

(require 'window)
(require 'windmove)
(setq windmove-wrap-around t)

;;; Keymap

(require 'keymap)

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
