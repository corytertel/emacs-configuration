
;;; A goal of this config is to keep emacs as snappy and fast as possible.
;;; In order to accomplish this, we will minimize as many things from
;;; running at the same time as possible. This means minimizing minor modes and hooks.
;;; Importing packages is fine, as long as they have no side effects.
;;; Defining new functions are fine, as long as they are not continuously running.

;;; Another goal is to not provide unneccesary configuration. In the past I have
;;; added configuration because I have found it in other configs, but I did not
;;; need it. Examples include tweaking the garbage collector, tramp, and lsp.
;;; Other times I've installed packages which seemed harmless to performance and
;;; a massive quality of life boost, but in reality they did nothing but harm
;;; performance and did not really boost quality of life beyond what exists
;;; out of the box with vanilla emacs. In this config, I will not install a package
;;; unless I find a need for it. In other words, packages will be descriptive,
;;; not perscriptive. I won't install a package because it looks cool of efficient.
;;; I will only add a package if I am currently struggling with the way things
;;; currently are.
;;; I will not add something until I need it.
;;; I will not add something unless the built-in functionality is not enough.

;;; The last goal is to provide "compatibility" with other tools. I'm not the biggest
;;; fan of vim-style editing, but it is without a doubt the most popular "efficient"
;;; style of editing. If I need to move to another machine, it will almost certainly
;;; have vim. If I need to use another editor, there will be a vim mode. Almost all
;;; curses applications have vim binds out-of-the-box. I don't want to change contexts.
;;; I want my experience to be the same everywhere. As a result, I use vim binds.
;;; I also use tmux binds for window management because these days tmux seems to be
;;; the de-facto window-manager/terminal-multiplexer.

;;; P.S. One method of crafting this config is to learn the current config in it's entirety before I
;;; add another package. Don't add a bunch of packages at once and try to learn them
;;; all at once. Just add one at a time, and learn it thoroughly. Even if you have a
;;; bunch of packages which you have in mind which you want to use, just go one at a time.

;;; P.P.S. The idea is that this config is as close being a superset
;;; of vim as possible.  If vim supports an action, this should also
;;; support that action. And if both evil and vanilla emacs provide
;;; that action, it should prefered to do it the vim way. An example
;;; would be killing a buffer. One could kill a buffer the emacs way,
;;; C-x k, or the vim way, :q. The vim way of :q should be
;;; used. Because C-x k should not be used, it may be unbound or
;;; rebound to a different action.

;;; Packages to add in the future (try to prioritize built-in packages):
;;; - use-package w/ lazy loading
;;; - Magit
;;; - in emacs terminal
;;; - tramp
;;; - use dired
;;; - embark + wgrep (and rg.el)
;;; - modeline
;;; - theme
;;; - use bookmarks, registers, and marks
;;; - learn the undo ring
;;; - use the emacs editing features
;;; - avy
;;; - workspaces
;;; - undo (either built-in undo ring or undo tree)
;;; - sudo-edit
;;; - info mode

;; Set up package.el to work with MELPA
;; Use a MELPA mirror because some places block melpa.org
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://www.mirrorservice.org/sites/melpa.org/packages/"))
(package-initialize)
;; (package-refresh-contents)

;; Utils
(load "~/.emacs.d/init-utils.el")

;; Quality of life
(load "~/.emacs.d/init-qol.el")

;; Vim-style editing
(load "~/.emacs.d/init-vim.el")

;; Tmux-style window management
(load "~/.emacs.d/init-tmux.el")

;; Tree sitter
(load "~/.emacs.d/init-treesit.el")

;; Lsp
(load "~/.emacs.d/init-lsp.el")

;; Git
(load "~/.emacs.d/init-git.el")

;; Eshell
(load "~/.emacs.d/init-eshell.el")

;; Web dev
(load "~/.emacs.d/init-web-dev.el")

;; Misc langs
(load "~/.emacs.d/init-misc-langs.el")
