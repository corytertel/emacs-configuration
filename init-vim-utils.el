
(evil-define-command evil-kill-this-buffer (&optional force)
  "Kill the current buffer. If the bang is given, quit even with unsaved changes."
  :repeat nil
  (interactive "<!>")
  (if force
      (set-buffer-modified-p nil))
  (if (buffer-modified-p)
      (message (propertize
                "No write since last change (add ! to override)"
                'face
                'font-lock-warning-face))
    (kill-buffer (current-buffer))))

(evil-define-command evil-save-and-kill-this-buffer (file &optional bang)
  "Save the current buffer and kill it."
  :repeat nil
  (interactive "<f><!>")
  (evil-write nil nil nil file bang)
  (evil-kill-this-buffer))

(evil-define-command evil-save-modified-and-kill-this-buffer (file &optional bang)
  "Save the current buffer if modified and kill it."
  :repeat nil
  (interactive "<f><!>")
  (when (buffer-modified-p)
    (evil-write nil nil nil file bang))
  (evil-kill-this-buffer))
