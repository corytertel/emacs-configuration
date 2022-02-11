
;; Disables the startup message
(setq inhibit-startup-message t)

(scroll-bar-mode -1) ; Disables the visible scrollbar
(tool-bar-mode -1)   ; Disables the toolbar
(menu-bar-mode -1)   ; Disables the menubar
(tooltip-mode -1)    ; Disables tooltips
(set-fringe-mode 10) ; Gives some breathing room

(setq visible-bell nil) ; Disables the visible bell

;; Sets the font. Height is a percentage
;(set-face-attribute 'default nil :font "Hack" :height 100)
;; (set-face-attribute 'default nil
;; 		    :family "Roboto Mono" :weight 'light :height 100)
;; (set-face-attribute 'bold nil
;; 		    :family "Roboto Mono" :weight 'regular)
;; (set-face-attribute 'italic nil
;; 		    :family "Victor Mono" :weight 'semilight :slant 'italic :height 90)
;; (set-fontset-font t 'unicode
;; 		  (font-spec :name "RobotoMono Nerd Font" :size 16) nil)
;; (set-fontset-font t '(#xe000 . #xffdd)
;; 		  (font-spec :name "RobotoMono Nerd Font" :size 12) nil)
(set-face-attribute 'default nil
		    :family "JetBrains Mono" :weight 'light :height 100)
(set-face-attribute 'bold nil
		    :family "JetBrains Mono" :weight 'regular)
(set-face-attribute 'italic nil
		    :family "Victor Mono" :weight 'semilight :slant 'italic :height 90)
(set-fontset-font t 'unicode
		  (font-spec :name "JetBrainsMono NF" :size 16) nil)
(set-fontset-font t '(#xe000 . #xffdd)
		  (font-spec :name "JetBrainsMono NF" :size 12) nil)

(load-theme 'tango)
;;(load-theme 'nord t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package command-log-mode)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package swiper)

;; Basic Keybind
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

;; Define a binding just for a certain mode
;;(define-key emacs-lisp-mode-map (kbd "C-x M-t") 'counsel-load-theme)

;; Declare keybinds in a more consise way
(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer cory/leader-keys
			  :keymaps '(normal insert visual emacs)
			  :prefix "SPC"
			  :global-prefix "C-SPC")
  (cory/leader-keys
    ;; Search (M-x alternative)
    "SPC" '(counsel-M-x :which-key "M-x")

    ;; Buffers
    "b"  '(:ignore t :which-key "buffers")
    "bb" '(counsel-switch-buffer :which-key "switch buffer")
    "bd" '(kill-buffer :which-key "kill-buffer")
    "be" '(eval-buffer :which-key "eval buffer")
    "bn" '(centaur-tabs-forward :which-key "next buffer")
    "bp" '(centaur-tabs-backward :which-key "previous buffer")
    "bN" '(centaur-tabs-counsel-switch-group :which-key "next group")

    ;; Files
    "f"  '(:ignore t :which-key "files")
    "ff" '(counsel-find-file :which-key "find-file")

    ;; Git
    "g"  '(:ignore t :which-key "git")

    ;; Toggles
   "t"  '(:ignore t :which-key "toggles")
   "ts" '(hydra-text-scale/body :which-key ":scale-text")
   "tt" '(counsel-load-theme :which-key "choose theme")
   ))

(defun cory/evil-hook ()
  (dolist (mode '(custom-mode
		  eshell-mode
		  special-mode
		  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  ;(setq evil-undo-system 'undo-tree)
  :hook (evil-mode . cory/evil-hook)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; (use-package evil-collection
;;   :after evil
;;   :ensure t
;;   :config
;;   (evil-collection-init))
(when (require 'evil-collection nil t)
  (evil-collection-init))

(evil-mode)

;(use-package undo-tree)
;(global-undo-tree-mode)

(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 25)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.0))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package helpful
  :ensure
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-style "rounded")
  (setq centaur-tabs-height 40)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-plain-icons nil)
  (setq centaur-tabs-gray-out-icons 'buffer)
  (setq centaur-tabs-set-bar 'under)
  (setq x-underline-at-descent-line t)
  (setq centaur-tabs-set-close-button t)
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-adjust-buffer-order t)
  (setq centaur-tabs-label-fixed-length 8) ; 0 is dynamic
  (centaur-tabs-headline-match)
  (centaur-tabs-change-fonts "JetBrains Mono" 100)
  (centaur-tabs-enable-buffer-reordering)
  (centaur-tabs-mode t)
  :init
  (setq centaur-tabs-enable-key-bindings t)
  :bind
  ;; Vim-like tab changing
  (:map evil-normal-state-map
	("g t" . centaur-tabs-forward)
	("g T" . centaur-tabs-backward))
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))


;;; Clojure
(use-package clojure-mode)
(use-package lsp-mode)
(use-package cider)
(use-package lsp-treemacs)
(use-package company)
(use-package flycheck
  :init
  (progn
    (define-fringe-bitmap 'my-flycheck-fringe-indicator
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00011100
              #b00111110
              #b00111110
              #b00111110
              #b00011100
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000))

    (flycheck-define-error-level 'error
      :severity 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-error)

    (flycheck-define-error-level 'warning
      :severity 1
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-warning)

    (flycheck-define-error-level 'info
      :severity 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-info)))

(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-minimum-prefix-length 1
      lsp-lens-enable t
      lsp-signature-auto-activate nil
      ;; lsp-enable-indentation nil ; uncomment to use cider identation instead of lsp
      ;; lsp-enable-completion-at-point-nil ; uncomment to use cider completion instead of lsp
      )

;; Define keybindings just for clojure-mode
;;(define-key clojure-mode-map (kbd "SPC cc") 'cider-jack-in)

;;; C++
;(use-package modern-cpp-font-lock
;  :ensure t)
;(modern-c++-font-lock-global-mode t)
;(use-package cpp-auto-include)

;;; Other Modes
(use-package haskell-mode)
(use-package nix-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(package-selected-packages
   '(nix-mode haskell-mode company flycheck lsp-treemacs cider lsp-mode clojure-mode undo-tree evil-collection which-key use-package rainbow-delimiters ivy-rich hydra helpful general evil doom-modeline counsel command-log-mode centaur-tabs annalist)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
