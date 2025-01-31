
;; Requires emacs compiled with tree-sitter support
;; In order for ts modes to successfully import, the
;; grammars must be found.

(require 'treesit)

(setq treesit-language-source-alist
      '((elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(python "https://github.com/tree-sitter/tree-sitter-python")))

;; Check if the language is available, if not install and compile
(mapc (lambda (lang)
	(unless (treesit-language-available-p lang)
	  (treesit-install-language-grammar lang)))
      (mapcar #'car treesit-language-source-alist))
