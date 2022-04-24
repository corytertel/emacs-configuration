
;; Disables the startup message
(setq inhibit-startup-message t)

(scroll-bar-mode -1) ; Disables the visible scrollbar
(tool-bar-mode -1)   ; Disables the toolbar
(menu-bar-mode -1)   ; Disables the menubar
(tooltip-mode -1)    ; Disables tooltips
(set-fringe-mode 10) ; Gives some breathing room

(setq visible-bell nil) ; Disables the visible bell

;; ;; Byte compile on first run
;; (defun cory-init/compile-user-emacs-directory ()
;;   "Recompile all files in `user-emacs-directory'."
;;   (byte-recompile-directory user-emacs-directory 0))

;; (unless (file-exists-p (locate-user-emacs-file "init.elc"))
;;   (add-hook 'after-init-hook #'cory-init/compile-user-emacs-directory))

;; ;; Prefer the newest files
;; (setq load-prefer-newer t)

;; ;; File name handling setup (for some reason makes emacs start faster)
;; (defvar startup/file-name-handler-alist file-name-handler-alist
;;   "Temporary storage for `file-name-handler-alist' during startup.")

;; (defun startup/revert-file-name-handler-alist ()
;;   "Revert `file-name-handler-alist' to its default value after startup."
;;   (setq file-name-handler-alist startup/file-name-handler-alist))

;; (setq file-name-handler-alist nil)
;; (add-hook 'emacs-startup-hook #'startup/revert-file-name-handler-alist)

;; ;; Garbage collection setup
;; (defun garbage-collect-defer ()
;;   "Defer garbage collection."
;;   (setq gc-cons-threshold most-positive-fixnum
;; 	gc-cons-percentage 0.6))

;; (defun garbage-collect-restore ()
;;   "Return garbage collection to normal parameters."
;;   (setq gc-cons-threshold 16777216
;; 	gc-cons-percentage 0.1))

;; (garbage-collect-defer)
;; (add-hook 'emacs-startup-hook #'garbage-collect-restore)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Asynchronous execution
;; (use-package async
;;   :ensure t
;;   :defer t
;;   :init
;;   (dired-async-mode 1)
;;   (async-bytecomp-package-mode 1)
;;   :custom (async-bytecomp-allowed-packages '(all)))

;; Automatic Package Updates (every 2 days)
;; (use-package auto-package-update
;;   :ensure t
;;   :defer t
;;   :custom ((auto-package-update-interval 2)
;; 	   (auto-package-update-hide-results t)
;; 	   (auto-package-update-delete-old-versions t))
;;   :hook (after-init . auto-package-update-maybe))

;; Use UTF-8 Encoding
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Setting the font
(set-face-attribute 'default nil
		    :family "VictorMono Nerd Font" :weight 'regular :height 100)
(set-face-attribute 'bold nil
		    :family "VictorMono Nerd Font" :weight 'bold)
(set-face-attribute 'italic nil
		    :family "VictorMono Nerd Font" :weight 'regular :slant 'italic)
(set-fontset-font t 'unicode
		  (font-spec :name "VictorMono Nerd Font" :size 16) nil)
(set-fontset-font t '(#xe000 . #xffdd)
		  (font-spec :name "VictorMono Nerd Font" :size 12) nil)

;; Don't unload fonts when not in use
(setq inhibit-compacting-font-caches t)

;; Theme
;; (use-package modus-themes
;;   :ensure
;;   :init
;;   ;; Add all your customizations prior to loading the themes
;;   (setq modus-themes-italic-constructs t
;;         modus-themes-bold-constructs nil
;;         modus-themes-region '(bg-only no-extend))

;;   ;; Load the theme files before enabling a theme
;;   (modus-themes-load-themes)
;;   :config
;;   ;; Load the theme of your choice:
;;   (modus-themes-load-operandi) ;; OR (modus-themes-load-vivendi)
;;   :bind ("<f11>" . modus-themes-toggle))

(setq custom-safe-themes t) ; Treat all themes as safe
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
(load-theme 'plain-light t)

;; Display Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook
		cider-repl-mode-hook
		racket-repl-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Window dividers
;; (setq window-divider-default-right-width 3)
;; (let ((color (face-background 'mode-line)))
;;   (dolist (face '(window-divider-first-pixel
;; 		  window-divider-last-pixel
;; 		  window-divider))
;;     (set-face-foreground face color)))
;; (window-divider-mode 1)

;; Transparent frames
;; (dolist (frame (frame-list))
;;   (set-frame-parameter frame 'alpha 90))
;; (add-to-list 'default-frame-alist '(alpha . 90))

;; Better Org-mode headers
;; (set-face-attribute 'org-document-title nil
;; 		    :weight 'extra-bold
;; 		    :height 1.8)
;; (set-face-attribute 'org-level-1 nil
;; 		    :height 1.3)
;; (set-face-attribute 'org-level-2 nil
;; 		    :height 1.1)
;; (set-face-attribute 'org-level-3 nil
;; 		    :height 1.0)
;; (set-face-attribute 'org-code nil
;; 		    :inherit 'font-lock-string-face)

;; Doom Modeline
(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 40)))

;; Show line/column numbers on the mode line
;; (line-number-mode 1)
;; (column-number-mode 1)

;; Show clock and battery level on the mode line
;; (display-time-mode 1)
;; (display-battery-mode 1)
;; :custom ((display-time-format "%a %m/%d %H:%M")
;; 	 (display-time-day-and-date t)
;; 	 (display-time-24hr-format t))

;; Start screen
(use-package dashboard
  :ensure t
  :defer t
  :init
  (dashboard-setup-startup-hook)
  :custom ((inhibit-start-screen t)
	   (dashboard-set-footer nil)
	   (dashboard-startup-banner (locate-user-emacs-file "logo.png"))
	   (dashboard-items '((recents . 10)))
	   (initial-buffer-choice #'dashboard-or-scratch)
	   (dashboard-banner-logo-title
	    "Welcome to GNU Emacs!")
	   (dashboard-center-content t)
	   (dashboard-show-shortcuts nil))
  :hook (dashboard-mode . dashboard-immortal))

;; Show dashboard or scratch initially
(defun dashboard-or-scratch ()
  "Open either dashboard or the scratch buffer."
  (or (get-buffer "*dashboard*")
      (get-buffer "*scratch*")))

;; Make the dashboard buffer immortal
(defun dashboard-immortal ()
  "Make the dashboard buffer immortal."
  (emacs-lock-mode 'kill))

;; Add padding to the sides
(require 'frame)
(setq-default default-frame-alist
	      (append (list
		       '(internal-border-width . 40)
		       ;; '(left-fringe . 0)
		       ;; '(right-fringe . 0)
		       '(tool-bar-lines . 0)
		       '(menu-bar-lines . 0)
		       '(line-spacing . 0.075)
		       '(vertical-scroll-bars . nil))))
(setq-default window-resize-pixelwise t)
(setq-default frame-resize-pixelwise t)
(add-hook 'before-make-frame-hook 'window-divider-mode)

;; Scroll conservatively
;; (setq scroll-conservatively 101)

;; Make scrolling a little less crazy
;; (setq scroll-margin 0
;;       auto-window-vscroll nil
;;       scroll-preserve-screen-position 1
;;       scroll-conservatively most-positive-fixnum
;;       mouse-wheel-scroll-amount '(1 ((shift) . 1))
;;       mouse-wheel-progressive-speed nil
;;       mouse-wheel-follow-mouse t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Always confirm closing Emacs
(setq confirm-kill-emacs #'yes-or-no-p)

;; Replace "yes or no" prompts with "y or n" prompts
(defalias 'yes-or-no-p #'y-or-n-p
  "Use `y-or-n-p' instead of a yes/no prompt.")

;;; Packages

(use-package command-log-mode)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("M-l" . ivy-alt-done)
	 ("M-j" . ivy-next-line)
	 ("M-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("M-k" . ivy-previous-line)
	 ("M-l" . ivy-done)
	 ("M-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("M-k" . ivy-previous-line)
	 ("M-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package swiper)

;; Basic Keybind
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "S-C-c") 'kill-ring-save)
(global-set-key (kbd "S-C-v") 'yank)

;; Define a binding just for a certain mode
;;(define-key emacs-lisp-mode-map (kbd "C-x M-t") 'counsel-load-theme)

;; Declare keybinds in a more consise way
(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer cory/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "S-SPC")
  (cory/leader-keys
    ;; Search (M-x alternative)
    "SPC" '(counsel-M-x :which-key "M-x")

    ;; Buffers
    "b"  '(:ignore t :which-key "buffers")
    "bb" '(counsel-switch-buffer :which-key "switch buffer")
    "bd" '(kill-this-buffer :which-key "kill buffer")
    "bD" '(kill-all-buffers-and-windows :which-key "kill all buffers")
    "be" '(eval-last-sexp :which-key "eval last expression")
    "bE" '(eval-buffer :which-key "eval buffer")
    ;; "bn" '(centaur-tabs-forward :which-key "next buffer")
    ;; "bp" '(centaur-tabs-backward :which-key "previous buffer")
    "bn" '(next-buffer :which-key "next buffer")
    "bp" '(previous-buffer :which-key "previous buffer")
    ;; "bg" '(centaur-tabs-counsel-switch-group :which-key "switch group")

    ;; Copy
    "c"  '(:ignore t :which-key "copy")
    "cc" '(copy-as-format :which-key "copy (default)")
    "cg" '(copy-as-format-github :which-key "copy (github)")
    "ct" '(copy-as-format-markdown-table :which-key "copy (markdown table)")
    "cm" '(copy-as-format-markdown :which-key "copy (markdown)")
    "co" '(copy-as-format-org-mode :which-key "copy (orgmode)")
    "cd" '(copy-as-format-slack :which-key "copy (discord)")
    "cv" '(org-copy-visible :which-key "org copy (visible)")

    ;; Files
    "f"  '(:ignore t :which-key "files")
    "ff" '(counsel-find-file :which-key "find file")
    "fr" '(rename-file :which-key "rename file")

    ;; Git
    "g"  '(:ignore t :which-key "git")
    "gs" '(magit-status :which-key "git status")

    ;; Operations
    "o"  '(:ignore t :which-key "operations")
    "oc" '(comment-or-uncomment-region :which-key "comment region")
    "or" '(replace-string :which-key "replace string")
    "os" '(swiper :which-key "search")

    ;; Projects
    "p" '(projectile-command-map :which-key "projects")

    ;; Saving
    "s" '(save-buffer :which-key "save buffer")

    ;; Toggles
    "t"  '(:ignore t :which-key "toggles")
    "tf" '(treemacs :which-key "file viewer")
    "ts" '(hydra-text-scale/body :which-key "scale text")
    "tt" '(counsel-load-theme :which-key "choose theme")

    ;; Windows
    "w"  '(:ignore t :which-key "windows")
    "wd" '(delete-window :which-key "delete window")
    "wD" '(kill-buffer-and-window :which-key "delete buffer and window")
    "wf" '(delete-other-windows :which-key "focus selected window")
    "ws" '(split-and-follow-right :which-key "split right")
    "wS" '(split-and-follow-below :which-key "split below")
    "wb" '(balance-windows :which-key "balance windows")
    "wr" '(hydra-window-resize/body :which-key "resize window")

    "wk" '(windmove-up :which-key "move focus up")
    "wj" '(windmove-down :which-key "move focus down")
    "wh" '(windmove-left :which-key "move focus left")
    "wl" '(windmove-right :which-key "move focus right")
    "wK" '(buf-move-up :which-key "move window up")
    "wJ" '(buf-move-down :which-key "move window down")
    "wH" '(buf-move-left :which-key "move window left")
    "wL" '(buf-move-right :which-key "move window right")

    ;; Window Management
    ;; Dwm-style movement
    ;; "j" '(next-window-any-frame :which-key "next window")
    ;; "k" '(previous-window-any-frame :which-key "previous window")
    ;; Directional movement
    "k" '(windmove-up :which-key "move focus up")
    "j" '(windmove-down :which-key "move focus down")
    "h" '(windmove-left :which-key "move focus left")
    "l" '(windmove-right :which-key "move focus right")
    "K" '(buf-move-up :which-key "move window up")
    "J" '(buf-move-down :which-key "move window down")
    "H" '(buf-move-left :which-key "move window left")
    "L" '(buf-move-right :which-key "move window right")

    "u" '(evil-scroll-page-up :which-key "page up")
    "d" '(evil-scroll-page-down :which-key "page down"))

  ;; (general-create-definer cory/racket-keys
  ;;   :keymaps '(normal override insert visual emacs racket-mode-map)
  ;;   :prefix "SPC"
  ;;   :global-prefix "S-SPC")

  ;; (cory/racket-keys
  ;;  ";e" '(racket-send-last-sexp :which-key "racket send expression")
  ;;  )
  )

;; Window management
(use-package buffer-move
  :ensure t
  :defer t
  :bind (("C-x o" . nil)
	 ("C-x o k" . windmove-up)
	 ("C-x o j" . windmove-down)
	 ("C-x o h" . windmove-left)
	 ("C-x o l" . windmove-right)
	 ("C-x o C-k" . buf-move-up)
	 ("C-x o C-j" . buf-move-down)
	 ("C-x o C-h" . buf-move-left)
	 ("C-x o C-l" . buf-move-right))
  :custom ((focus-follows-mouse t)
	   (mouse-autoselect-window t)))

(defun split-and-follow-below ()
  "Open a new window vertically."
  (interactive)
  (split-window-below)
  (other-window 1)
  (counsel-ibuffer))

(defun split-and-follow-right ()
  "Open a new window horizontally."
  (interactive)
  (split-window-right)
  (other-window 1)
  (counsel-ibuffer))

(defun kill-all-buffers-and-windows ()
  "Kill all buffers and windows."
  (interactive)
  (when (yes-or-no-p "Really kill all buffers and windows? ")
    (save-some-buffers)
    (mapc 'kill-buffer (buffer-list))
    (delete-other-windows)))

(global-set-key (kbd "C-x 2") 'split-and-follow-below)
(global-set-key (kbd "C-x 3") 'split-and-follow-right)
(global-set-key (kbd "C-x 4 q") 'kill-all-buffers-and-windows)
(global-set-key (kbd "C-c b") 'balance-windows)

;; Emacs run launcher
(defun emacs-run-launcher ()
  "A frame to launch desktop applications."
  (interactive)
  (with-selected-frame
      (make-frame '((name . "emacs-run-launcher")
		    (minibuffer . only)
		    (width . 240)
		    (height . 22)))
    (unwind-protect
	(counsel-linux-app)
      (delete-frame))))

(defun cory/evil-hook ()
  (dolist (mode '(custom-mode
		  eshell-mode
		  special-mode
		  term-mode
		  cider-repl-mode
		  racket-repl-mode
		  dashboard-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-tree)
  ;; (setq evil-search-module 'swiper)
  :hook (evil-mode . cory/evil-hook)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-normal-state-map (kbd "/") 'swiper)

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

(use-package undo-tree)
(global-undo-tree-mode)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.00000001))

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
  ("q" nil "finished" :exit t))

(defhydra hydra-window-resize (:timeout 4)
  "resize window"
  ("k" shrink-window 5 "shrink vertically")
  ("j" enlarge-window 5 "enlarge vertically")
  ("h" shrink-window-horizontally 5 "shrink horizontally")
  ("l" enlarge-window-horizontally 5 "enlarge horizontally")
  ("q" nil "finished" :exit t))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Code")
    (setq projectile-project-search-path '("~/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; (use-package evil-magit
;;   :after magit)

;; (use-package forge)

;; (use-package centaur-tabs
;;   :demand
;;   :config
;;   (centaur-tabs-mode t)
;;   (setq centaur-tabs-style "bar")
;;   (setq centaur-tabs-height 72)
;;   (setq centaur-tabs-set-icons t)
;;   (setq centaur-tabs-plain-icons nil)
;;   (setq centaur-tabs-gray-out-icons 'buffer)
;;   (setq centaur-tabs-set-bar 'under)
;;   (setq x-underline-at-descent-line t)
;;   (setq centaur-tabs-set-close-button t)
;;   (setq centaur-tabs-set-modified-marker t)
;;   (setq centaur-tabs-adjust-buffer-order t)
;;   (setq centaur-tabs-label-fixed-length 10) ; 0 is dynamic
;;   (setq centaur-tabs-cycle-scope 'tabs)
;;   (centaur-tabs-headline-match)
;;   (centaur-tabs-change-fonts "VictorMono Nerd Font" 100)
;;   (centaur-tabs-enable-buffer-reordering)
;;   (centaur-tabs-mode t)
;;   :init
;;   (setq centaur-tabs-enable-key-bindings t)
;;   :bind
;;   ;; Vim-like tab changing
;;   (:map evil-normal-state-map
;; 	("g t" . centaur-tabs-forward)
;; 	("g T" . centaur-tabs-backward))
;;   ("C-<prior>" . centaur-tabs-backward)
;;   ("C-<next>" . centaur-tabs-forward))

;;; IDE Features

;; Pretty mode (ligatures and indicators)
(use-package pretty-mode
  :config
  (add-hook 'prog-mode-hook 'pretty-mode))

;; Ligatures
;; (use-package ligature
;;   :hook (prog-mode . ligature-mode)
;;   :config
;;   ;; Some ligatures supported by most fonts. E.g. Fira Code, Victor Mono
;;   (ligature-set-ligatures 'prog-mode
;; 			  '("~~>" "##" "|-" "-|" "|->" "|=" ">-" "<-" "<--" "->"
;;                             "-->" "-<" ">->" ">>-" "<<-" "<->" "->>" "-<<" "<-<"
;;                             "==>" "=>" "=/=" "!==" "!=" "<==" ">>=" "=>>" ">=>"
;;                             "<=>" "<=<" "=<=" "=>=" "<<=" "=<<"
;;                             "=:=" "=!=" "==" "===" "::" ":=" ":>" ":<" ">:"
;;                             ";;" "/=" "__" "&&" "++")))

;; Automatically remove trailing whitespace if user put it there
(use-package ws-butler
  :hook ((text-mode prog-mode) . ws-butler-mode)
  :config (setq ws-butler-keep-whitespace-before-point nil))

;; Word wrapping
(global-visual-line-mode 1)
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'turn-on-auto-fill)

;; Make the cursor a bar
(setq-default cursor-type 'bar)

;; Turn ^L into pretty lines
(use-package page-break-lines
  :ensure t
  :defer t
  :hook (after-init . global-page-break-lines-mode))

;; Highlight matching parentheses
;; (use-package paren
;;   :defer t
;;   :init
;;   (show-paren-mode 1)
;;   :custom-face (show-paren-match
;; 		((t (:weight extra-bold
;; 			     :underline t))))
;;   :custom ((show-paren-style 'parentheses)
;; 	   (show-paren-delay 0.00000001)))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Auto pairs
;; (electric-pair-mode)

;; Show empty whitespace
(global-whitespace-mode)
(setq whitespace-style '(face trailing tabs lines empty big-indent))

;; Use hex mode for binary files
(add-to-list 'auto-mode-alist '("\\.bin\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.dat\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.exe\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.o\\'" . hexl-mode))

;; Highlight and navigate TODO keywords
(use-package hl-todo
  :defer 2
  :config (global-hl-todo-mode))

;; Visual feedback on some operations like yank, kill, undo
(use-package goggles
  :defer 10
  :config (goggles-mode))

;; Show the name of the current function definition in the modeline
(use-package which-func
  :defer 5
  :config (which-function-mode 1))

;;; Smartparens
(use-package smartparens
  :defer 1
  :hook ((
          emacs-lisp-mode lisp-mode lisp-data-mode clojure-mode cider-repl-mode
	  racket-mode racket-repl-mode hy-mode prolog-mode go-mode cc-mode
	  python-mode typescript-mode json-mode javascript-mode ;java-mode
          ) . smartparens-strict-mode)
  ;; :hook (prog-mode . smartparens-strict-mode)
  :bind (:map smartparens-mode-map
         ;; This is the paredit mode map minus a few key bindings
         ;; that I use in other modes (e.g. M-?)
         ("C-M-f" . sp-forward-sexp) ;; navigation
         ("C-M-b" . sp-backward-sexp)
         ("C-M-u" . sp-backward-up-sexp)
         ("C-M-d" . sp-down-sexp)
         ("C-M-p" . sp-backward-down-sexp)
         ("C-M-n" . sp-up-sexp)
         ("C-w" . whole-line-or-region-sp-kill-region)
         ("M-s" . sp-splice-sexp) ;; depth-changing commands
         ("M-r" . sp-splice-sexp-killing-around)
         ("M-(" . sp-wrap-round)
         ("C-)" . sp-forward-slurp-sexp) ;; barf/slurp
         ("C-<right>" . sp-forward-slurp-sexp)
         ("M-0" . sp-forward-slurp-sexp)
         ("C-}" . sp-forward-barf-sexp)
         ("C-<left>" . sp-forward-barf-sexp)
         ("M-9" . sp-forward-barf-sexp)
         ("C-(" . sp-backward-slurp-sexp)
         ("C-M-<left>" . sp-backward-slurp-sexp)
         ("C-{" . sp-backward-barf-sexp)
         ("C-M-<right>" . sp-backward-barf-sexp)
         ("M-S" . sp-split-sexp) ;; misc
         ("M-j" . sp-join-sexp))
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)

  ;; Don't insert annoying colon after Python def
  (setq sp-python-insert-colon-in-function-definitions nil)

  ;; Always highlight matching parens
  (show-smartparens-global-mode +1)
  (setq blink-matching-paren nil)  ;; Don't blink matching parens

  (defun whole-line-or-region-sp-kill-region (prefix)
    "Call `sp-kill-region' on region or PREFIX whole lines."
    (interactive "*p")
    (whole-line-or-region-wrap-beg-end 'sp-kill-region prefix))

  ;; Create keybindings to wrap symbol/region in pairs
  (defun prelude-wrap-with (s)
    "Create a wrapper function for smartparens using S."
    `(lambda (&optional arg)
       (interactive "P")
       (sp-wrap-with-pair ,s)))
  (define-key prog-mode-map (kbd "M-(") (prelude-wrap-with "("))
  (define-key prog-mode-map (kbd "M-[") (prelude-wrap-with "["))
  (define-key prog-mode-map (kbd "M-{") (prelude-wrap-with "{"))
  (define-key prog-mode-map (kbd "M-\"") (prelude-wrap-with "\""))
  (define-key prog-mode-map (kbd "M-'") (prelude-wrap-with "'"))
  (define-key prog-mode-map (kbd "M-`") (prelude-wrap-with "`"))

  ;; smart curly braces
  (sp-pair "{" nil :post-handlers
           '(((lambda (&rest _ignored)
                (crux-smart-open-line-above)) "RET")))
  (sp-pair "[" nil :post-handlers
           '(((lambda (&rest _ignored)
                (crux-smart-open-line-above)) "RET")))
  (sp-pair "(" nil :post-handlers
           '(((lambda (&rest _ignored)
                (crux-smart-open-line-above)) "RET")))

  ;; Don't include semicolon ; when slurping
  (add-to-list 'sp-sexp-suffix '(java-mode regexp ""))

  ;; use smartparens-mode everywhere
  (smartparens-global-mode))

;;; end smartparens

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-c m" . mc/mark-all-dwim)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         :map mc/keymap
         ("C-x v" . mc/vertical-align-with-space)
         ("C-x n" . mc-hide-unmatched-lines-mode))
  :config
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

  (with-eval-after-load 'multiple-cursors-core
    ;; Immediately load mc list, otherwise it will show as
    ;; changed as empty in my git repo
    (mc/load-lists)

    (define-key mc/keymap (kbd "M-T") 'mc/reverse-regions)
    (define-key mc/keymap (kbd "C-,") 'mc/unmark-next-like-this)
    (define-key mc/keymap (kbd "C-.") 'mc/skip-to-next-like-this)))

;; Copy text as Discord/GitHub/etc formatted code
(use-package copy-as-format
  :config
  (setq copy-as-format-default "slack")
  (defun copy-as-format--markdown-table (text _multiline)
    (s-replace "--+--" "--|--" text))
  (add-to-list 'copy-as-format-format-alist '("markdown-table" copy-as-format--markdown-table)))

(use-package lsp-mode
  :bind (:map lsp-mode-map
         ("C-c C-a" . lsp-execute-code-action)
         ("M-." . lsp-find-definition-other)
         ("M-," . lsp-find-references-other))
  :init (setq lsp-keymap-prefix nil)  ; Don't map the lsp keymap to any key
  :config
  ;; Increase lsp file watch threshold when lsp shows a warning
  (setq lsp-file-watch-threshold 1500)

  (defun lsp-find-definition-other (other?)
    "Like `lsp-find-definition' but open in other window when called with prefix arg."
    (interactive "P")
    (back-button-push-mark-local-and-global)
    (if other?
        (lsp-find-definition :display-action 'window)
      (lsp-find-definition)))
  (defun lsp-find-references-other (other?)
    "Like `lsp-find-references' but open in other window when called with prefix arg."
    (interactive "P")
    (back-button-push-mark-local-and-global)
    (if other?
        (lsp-find-references :display-action 'window)
      (lsp-find-references)))

  ;; Don't watch `build' and `.gradle' directories for file changes
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]build$")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.gradle$")

  (require 'yasnippet)) ;; Use yasnippet for lsp snippet support

(use-package lsp-ui
  :bind (:map lsp-mode-map
         ("M-?" . lsp-ui-doc-toggle))
  :config
  (defun lsp-ui-doc-toggle ()
    "Shows or hides lsp-ui-doc popup."
    (interactive)
    (if lsp-ui-doc--bounds
        (lsp-ui-doc-hide)
      (lsp-ui-doc-show)))

  ;; Deactivate most of the annoying "fancy features"
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-use-childframe t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-symbol nil))

(use-package lsp-treemacs
  :after lsp-mode
  :config
  ;; Enable bidirectional synchronization of lsp workspace folders and treemacs
  (lsp-treemacs-sync-mode))

(use-package dap-mode
  :after lsp-mode
  :bind (:map dap-server-log-mode-map
         ("g" . recompile)
         :map dap-mode-map
         ([f9] . dap-continue)
         ([S-f9] . dap-disconnect)
         ([f10] . dap-next)
         ([f11] . dap-step-in)
         ([S-f11] . dap-step-out)
         ([f12] . dap-hide/show-ui))
  :config
  ;; FIXME: Create nice solution instead of a hack
  (defvar dap-hide/show-ui-hidden? t)
  (defun dap-hide/show-ui ()
    "Hide/show dap ui. FIXME"
    (interactive)
    (if dap-hide/show-ui-hidden?
        (progn
          (setq dap-hide/show-ui-hidden? nil)
          (dap-ui-locals)
          (dap-ui-repl))
      (dolist (buf '("*dap-ui-inspect*" "*dap-ui-locals*" "*dap-ui-repl*" "*dap-ui-sessions*"))
        (when (get-buffer buf)
          (kill-buffer buf)))
      (setq dap-hide/show-ui-hidden? t)))

  (dap-mode)
  ;; displays floating panel with debug buttons
  (dap-ui-controls-mode)
  ;; Displaying DAP visuals
  (dap-ui-mode))

;; (use-package company
;;   :ensure t
;;   :defer t
;;   :custom ((company-idle-delay 0.75)
;; 	   (company-minimum-prefix-length 3))
;;   :hook (after-init . global-company-mode)
;;   :bind (:map company-active-map
;; 	      ("M-n" . nil)
;; 	      ("M-p" . nil)
;; 	      ("C-n" . company-select-next)
;; 	      ("C-p" . company-select-previous)))

;; Autocompletion
(use-package company
  :defer 1
  :bind (:map company-active-map
         ([return] . nil)
         ("RET" . nil)

         ("TAB" . company-complete-selection)
         ([tab] . company-complete-selection)
         ("S-TAB" . company-select-previous)
         ([backtab] . company-select-previous)
         ("C-j" . company-complete-selection))
  :config
  (setq company-idle-delay 0.1)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 1)
  ;; Don't display icons
  (setq company-format-margin-function nil)
  ;; Aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  ;;(setq company-dabbrev-downcase nil)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  ;;(setq company-tooltip-flip-when-above t)
  ;; start autocompletion only after typing
  (setq company-begin-commands '(self-insert-command))
  (global-company-mode 1)

  (use-package company-emoji
    :disabled t
    :config (add-to-list 'company-backends 'company-emoji))

  (use-package company-quickhelp
    :disabled t
    :config (company-quickhelp-mode 1))

  ;; Add yasnippet support for all company backends
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

;; Enhances TAB select
(use-package company-tng
  :disabled t
  :after company
  :bind (:map company-active-map
         ([return] . nil)
         ("RET" . nil)
         ("TAB" . company-select-next)
         ([tab] . company-select-next)
         ("S-TAB" . company-select-previous)
         ([backtab] . company-select-previous)
         ("C-j" . company-complete-selection))
  :config
  (company-tng-mode))

;; Autocompletion for shell
(use-package company-shell
  :hook ((sh-mode shell-mode) . sh-mode-init)
  :config
  (defun sh-mode-init ()
    (setq-local company-backends
		'((company-shell
                   company-shell-env
                   company-files
                   company-dabbrev-code
                   company-capf
                   company-yasnippet)))))

;; Tramp
(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "ssh")

  ;; Only for debugging slow tramp connections
  ;;(setq tramp-verbose 7)

  ;; Skip version control for tramp files
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))

  ;; Use ControlPath from .ssh/config
  (setq tramp-ssh-controlmaster-options "")

  ;; Backup tramp files like local files and don't litter the remote
  ;; file system with my emacs backup files
  ;;(setq tramp-backup-directory-alist backup-directory-alist)
  ;; (add-to-list 'backup-directory-alist
  ;;              (cons tramp-file-name-regexp
  ;;                    (no-littering-expand-var-file-name "backup/")))

  ;; See https://www.gnu.org/software/tramp/#Ad_002dhoc-multi_002dhops
  ;; For all hosts, except my local one, first connect via ssh, and then apply sudo -u root:
  (dolist (tramp-proxies '((nil "\\`root\\'" "/ssh:%h:")
                           ((regexp-quote (system-name)) nil nil)
                           ("localhost" nil nil)
                           ("blif\\.vpn" nil nil)
                           ("skor-pi" nil nil)
                           ;; Add tramp proxy for atomx user
                           (nil "atomx" "/ssh:%h:")))
    (add-to-list 'tramp-default-proxies-alist tramp-proxies)))

(use-package flycheck
  :ensure
  :init
  (global-flycheck-mode t)
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

;;; Lisps

;; Nicer elisp regex syntax highlighting
(use-package easy-escape
  :hook ((emacs-lisp-mode lisp-mode) . easy-escape-minor-mode))

;; From: https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L20-L94
;; redefines the silly indent of keyword lists
;; before
;;   (:foo bar
;;         :baz qux)
;; after
;;   (:foo bar
;;    :baz qux)
(eval-after-load "lisp-mode"
  '(defun lisp-indent-function (indent-point state)
     "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:
- `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
- an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
- a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
     (let ((normal-indent (current-column))
           (orig-point (point)))
       (goto-char (1+ (elt state 1)))
       (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
       (cond
        ;; car of form doesn't seem to be a symbol, or is a keyword
        ((and (elt state 2)
              (or (not (looking-at "\\sw\\|\\s_"))
                  (looking-at ":")))
         (if (not (> (save-excursion (forward-line 1) (point))
                     calculate-lisp-indent-last-sexp))
             (progn (goto-char calculate-lisp-indent-last-sexp)
                    (beginning-of-line)
                    (parse-partial-sexp (point)
                                        calculate-lisp-indent-last-sexp 0 t)))
         ;; Indent under the list or under the first sexp on the same
         ;; line as calculate-lisp-indent-last-sexp.  Note that first
         ;; thing on that line has to be complete sexp since we are
         ;; inside the innermost containing sexp.
         (backward-prefix-chars)
         (current-column))
        ((and (save-excursion
                (goto-char indent-point)
                (skip-syntax-forward " ")
                (not (looking-at ":")))
              (save-excursion
                (goto-char orig-point)
                (looking-at ":")))
         (save-excursion
           (goto-char (+ 2 (elt state 1)))
           (current-column)))
        (t
         (let ((function (buffer-substring (point)
                                           (progn (forward-sexp 1) (point))))
               method)
           (setq method (or (function-get (intern-soft function)
                                          'lisp-indent-function)
                            (get (intern-soft function) 'lisp-indent-hook)))
           (cond ((or (eq method 'defun)
                      (and (null method)
                           (> (length function) 3)
                           (string-match "\\`def" function)))
                  (lisp-indent-defform state indent-point))
                 ((integerp method)
                  (lisp-indent-specform method state
                                        indent-point normal-indent))
                 (method
                  (funcall method indent-point state)))))))))

;;; Elisp
;; (use-package subr-x
;;   :defer t
;;   :config
;;   (put 'if-let   'byte-obsolete-info nil)
;;   (put 'when-let 'byte-obsolete-info nil))

;; (use-package elisp-mode
;;   :bind (:map emacs-lisp-mode-map
;;          ("C-c C-c" . eval-defun)
;;          ("C-c C-b" . eval-buffer)
;;          ("C-c C-k" . eval-buffer)
;;          ("C-c ;"   . eval-print-as-comment))
;;   :config
;;   (defvar eval-print-as-comment-prefix ";;=> ")

;;   (defun eval-print-as-comment (&optional arg)
;;     (interactive "P")
;;     (let ((start (point)))
;;       (eval-print-last-sexp arg)
;;       (save-excursion
;;         (goto-char start)
;;         (save-match-data
;;           (re-search-forward "[[:space:]\n]*" nil t)
;;           (insert eval-print-as-comment-prefix)))))

;;   (add-hook 'emacs-lisp-mode-hook (lambda ()
;;                                     (setq mode-name "EL"))))

;;; Clojure
(use-package clojure-mode
  :defer t
  :interpreter ("bb" . clojure-mode)
  :config
  ;; Eval top level forms inside comment forms instead of the comment form itself
  (setq clojure-toplevel-inside-comment-form t)
  ;; Indent fn-traced and defn-traced the same as a regular defn.
  ;; The macros are for re-frame-10x tracing.
  (put-clojure-indent 'fn-traced :defn)
  (put-clojure-indent 'defn-traced :defn))

(use-package cider
  :bind (:map cider-mode-map
         ("M-?" . cider-maybe-clojuredocs)
         :map cider-repl-mode-map
         ("M-?" . cider-doc))
  :hook (((cider-mode cider-repl-mode) . cider-company-enable-fuzzy-completion)
         (cider-mode . eldoc-mode))
  :config
  (defun cider-maybe-clojuredocs (&optional arg)
    "Like `cider-doc' but call `cider-clojuredocs' when invoked with prefix arg in `clojure-mode'."
    (interactive "P")
    (if (and arg (eq major-mode 'clojure-mode))
        (cider-clojuredocs arg)
      (cider-doc)))

  ;; Location of the jdk sources. In Arch Linux package `openjdk-src'
  (setq cider-jdk-src-paths "/usr/lib/jvm/java-11-openjdk/lib/src.zip")

  (require 's)

  ;; Inject reveal middleware in cider-jack-in when the `:reveal' alias is set
  (defun cider-cli-global-options-contains-reveal? (&rest _)
    (and cider-clojure-cli-global-options
         (s-contains? ":reveal" cider-clojure-cli-global-options)))
  (add-to-list 'cider-jack-in-nrepl-middlewares
               '("vlaaad.reveal.nrepl/middleware" :predicate cider-cli-global-options-contains-reveal?))

  ;; Inject shadowcljs nrepl middleware in cider-jack-in when the `:cljs' alias is set
  (defun cider-cli-global-options-contains-cljs? (&rest _)
    (and cider-clojure-cli-global-options
         (s-contains? ":cljs" cider-clojure-cli-global-options)))
  (add-to-list 'cider-jack-in-nrepl-middlewares
               '("shadow.cljs.devtools.server.nrepl/middleware" :predicate cider-cli-global-options-contains-cljs?))


  ;; jack-in for babashka
  (defun cider-jack-in-babashka ()
    "Start an babashka nREPL server for the current project and connect to it."
    (interactive)
    (let* ((default-directory (project-root (project-current t)))
           (process-filter (lambda (proc string)
                             "Run cider-connect once babashka nrepl server is ready."
                             (when (string-match "Started nREPL server at .+:\\([0-9]+\\)" string)
                               (cider-connect-clj (list :host "localhost"
                                                        :port (match-string 1 string)
                                                        :project-dir default-directory)))
                             ;; Default behavior: write to process buffer
                             (internal-default-process-filter proc string))))
      (set-process-filter
       (start-file-process "babashka" "*babashka*" "bb" "--nrepl-server" "0")
       process-filter)))

  ;; Store more items in repl history (default 500)
  (setq cider-repl-history-size 2000)
  ;; When loading the buffer (C-c C-k) save first without asking
  (setq cider-save-file-on-load t)
  ;; Don't show cider help text in repl after jack-in
  (setq cider-repl-display-help-banner nil)
  ;; Don't focus repl after sending somehint to there from another buffer
  (setq cider-switch-to-repl-on-insert nil)
  ;; Eval automatically when insreting in the repl (e..g. C-c C-j d/e) (unless called with prefix)
  (setq cider-invert-insert-eval-p t)
  ;; Don't focus error buffer when error is thrown
  (setq cider-auto-select-error-buffer nil)
  ;; Don't focus inspector after evaluating something
  (setq cider-inspector-auto-select-buffer nil)
  ;; Display context dependent info in the eldoc where possible.
  (setq cider-eldoc-display-context-dependent-info t)
  ;; Don't pop to the REPL buffer on connect
  ;; Create and display the buffer, but don't focus it.
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)
  ;; Just use symbol under point and don't prompt for symbol in e.g. cider-doc.
  (setq cider-prompt-for-symbol nil))

(use-package clj-refactor
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (dolist (magit-require '(("csv" . "clojure.data.csv")
                           ("edn" . "clojure.edn")
                           ;; ("http" . "clj-http.client")
                           ("reagent" . "reagent.core")
                           ("re-frame" . "re-frame.core")))
    (add-to-list 'cljr-magic-require-namespaces magit-require)))

(use-package flycheck-clj-kondo
  :after (flycheck clojure-mode))

;; (use-package ob-clojure
;;   :after ob
;;   :config
;;   (setq org-babel-clojure-backend 'cider))

(use-package clojure-mode-extra-font-locking)
(use-package paredit)

;; Enable paredit for Clojure
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

;; syntax hilighting for midje
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq inferior-lisp-program "lein repl")
            (font-lock-add-keywords
             nil
             '(("(\\(facts?\\)"
                (1 font-lock-keyword-face))
               ("(\\(background?\\)"
                (1 font-lock-keyword-face))))
            (define-clojure-indent (fact 1))
            (define-clojure-indent (facts 1))
            (rainbow-delimiters-mode)))

;; provides minibuffer documentation for the code you're typing into the repl
(add-hook 'cider-mode-hook 'eldoc-mode)

;; go right to the REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect t)

;; When there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; Wrap when navigating history.
(setq cider-repl-wrap-history t)

;; enable paredit in your REPL
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

;; Define keybindings just for clojure-mode
;; (define-key clojure-mode-map (kbd "SPC l c") 'cider-jack-in)

;; Clojure-mode specific keybindings
(add-hook 'clojure-mode-hook
	  '(cory/leader-keys
	     ","  '(:ignore t :which-key "clojure")
	     ",c" '(cider-jack-in-clj :which-key "cider jack in")
	     ",k" '(cider-load-buffer :which-key "load buffer")))

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

;;; C++
(use-package yasnippet)
(yas-global-mode 1)

;; (use-package auto-complete
;;   :ensure t
;;   :init
;;   (progn
;;     (ac-config-default)
;;     (global-auto-complete-mode t)))

(use-package modern-cpp-font-lock
  :ensure t)
(modern-c++-font-lock-global-mode t)

(use-package cpp-auto-include)

(defun code-compile ()
  (interactive)
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
     (let ((file (file-name-nondirectory buffer-file-name)))
       (format "%s -o %s %s"
           (if  (equal (file-name-extension file) "cpp") "clang++" "clang" )
           (file-name-sans-extension file)
           file)))
    (compile compile-command)))

(use-package cc-mode
  :bind (:map c-mode-base-map
         ("C-c C-a" . nil)
         ;; I want to use smartparens for () and {} instead of c-electric
         ("(" . nil)
         (")" . nil)
         ("{" . nil)
         ("}" . nil)
         (";" . nil)
         ("," . nil)))

(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package cmake-mode
  :mode ("CMakeLists.txt" "\\.cmake\\'"))

(use-package irony
  :disabled t
  :hook (((c++-mode c-mode objc-mode) . irony-mode-on-maybe)
         (irony-mode . irony-cdb-autosetup-compile-options))
  :config
  (defun irony-mode-on-maybe ()
    ;; avoid enabling irony-mode in modes that inherits c-mode, e.g: solidity-mode
    (when (member major-mode irony-supported-major-modes)
      (irony-mode 1))))

(use-package company-irony
  :after irony
  :config (add-to-list 'company-backends 'company-irony))

(use-package irony-eldoc
  :hook (irony-mode))

;;(global-set-key [f9] 'code-compile)

;;; Racket
(use-package racket-mode)
(use-package flymake-racket)
(use-package dr-racket-like-unicode)
(use-package bracketed-paste)

(add-hook 'racket-mode-hook
	  (lambda ()
	    (define-key racket-mode-map (kbd "<f5>") 'racket-run)))

(add-hook 'racket-repl-mode-hook
	  (lambda ()
	    (define-key racket-repl-mode-map (kbd "<f5>") 'racket-run)))

;;; Java
(use-package lsp-java
  :hook (java-mode . java-lsp-init)
  :config
  ;; Use Google style formatting by default
  (setq lsp-java-format-settings-url
        "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
  (setq lsp-java-format-settings-profile "GoogleStyle")

  ;; Use 3rd party decompiler
  (setq lsp-java-content-provider-preferred "fernflower")
  (defun java-lsp-init ()
    "We need to require java-lsp before loading lsp in a Java buffer.
use-package will load java-lsp for us simply by calling this function."
    (setq electric-indent-inhibit nil)  ; Auto-indent code after e.g. {}
    (setq company-lsp-cache-candidates nil)  ; Company cache should be disabled for lsp-java
    (lsp-deferred)))

;; (use-package dap-java
;;   :after lsp-java)

;; For groovy and gradle support
(use-package groovy-mode :defer t)

;; Viewing Java Class files
(defun javap-handler-real (operation args)
  "Run the real handler without the javap handler installed."
  (let ((inhibit-file-name-handlers
         (cons 'javap-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

(defun javap-handler (op &rest args)
  "Handle .class files by putting the output of javap in the buffer."
  (cond
   ((eq op 'get-file-buffer)
    (let ((file (car args)))
      (with-current-buffer (create-file-buffer file)
        (call-process "javap" nil (current-buffer) nil "-verbose"
                      "-classpath" (file-name-directory file)
                      (file-name-sans-extension (file-name-nondirectory file)))
        (setq buffer-file-name file)
        (setq buffer-read-only t)
        (set-buffer-modified-p nil)
        (goto-char (point-min))
        (java-mode)
        (current-buffer))))
   ((javap-handler-real op args))))

(add-to-list 'file-name-handler-alist '("\\.class$" . javap-handler))

;;; Latex
;; (use-package latex-preview-pane)

;;; Other Modes
(use-package haskell-mode
  :hook (haskell-mode . haskell-indentation-mode))
(use-package nix-mode
  :mode "\\.nix\\'")

;;; Terminal

(use-package term
  :config
  (setq explicit-shell-file-name "zsh")
  ;;(setq explicit-zsh-args '())
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

;;; Eshell

(defun cory/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size 10000
	eshell-buffer-maximum-lines 10000
	eshell-hist-ignoredups t
	eshell-scroll-to-bottom-on-input t))

(use-package eshell
  :hook (eshell-first-time-mode . cory/configure-shell))

(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

;; Running programs in a term-mode buffer
(with-eval-after-load 'esh-opt
  (setq eshell-destroy-buffer-when-process-dies t)
  (setq eshell-visual-commands '("htop" "zsh" "vim")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(eshell-git-prompt eterm-256color dap-java ob-clojure elisp-mode subr-x forge evil-magit magit paredit clojure-mode-extra-font-locking counsel-projectile projectile yasnippet which-key use-package undo-tree rainbow-delimiters parchment-theme nix-mode modern-cpp-font-lock lsp-treemacs ivy-rich helpful haskell-mode general flycheck evil doom-modeline cpp-auto-include counsel company command-log-mode cider centaur-tabs auto-complete))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((t (:weight extra-bold :underline t)))))

;;; init.el ends here
