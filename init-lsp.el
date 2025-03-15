
;; Use lsp-bridge for it's speed.
;; Unfortunately, lsp-bridge is neither built-in, nor is it pure elisp.

;; lsp-bridge requires python dependenices:
;; pip3 install epc orjson sexpdata six setuptools paramiko rapidfuzz watchdog packaging

;; lsp-bridge requires markdown-mode and yasnippet as dependencies
(pkg 'markdown-mode)
(pkg 'yasnippet)
(require 'yasnippet)
(yas-global-mode 1)

(pkg-git 'lsp-bridge "https://github.com/manateelazycat/lsp-bridge")
(require 'lsp-bridge)
(global-lsp-bridge-mode)

;; Enable tooltip diagnostics on hover
(setq lsp-bridge-enable-hover-diagnostic t)
