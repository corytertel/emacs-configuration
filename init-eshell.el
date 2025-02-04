
;; Make eshell dark mode
(require 'eshell)

(add-hook 'eshell-mode-hook
          (lambda ()
            (require 'face-remap)
            (setq-local buffer-face-mode-face '(:background "black" :foreground "white"))
            (buffer-face-mode 1)
            ;; Turn off line numbers for eshell
            (setq-local display-line-numbers nil)))

;; Eshell faces
(require 'em-ls)
(set-face-attribute 'eshell-ls-directory nil
                    :foreground "cyan"
                    :inherit nil)

(set-face-attribute 'eshell-ls-symlink nil
                    :foreground "magenta"
                    :inherit nil)
