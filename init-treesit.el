
;; Requires emacs compiled with tree-sitter support
;; In order for ts modes to successfully import, the
;; grammars must be found.

(require 'treesit)

(setq treesit-language-source-alist
      '((elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.23.2"))
        (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.23.4"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.23.2"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1" "src"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.24.8"))
        (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.23.6"))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.23.2"))
        (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
        ;; (java "https://github.com/tree-sitter/java-tree-sitter" "v0.24.1" "src/main/java/io/github/treesitter/jtreesitter")
        ))

;; Check if the language is available, if not install and compile
(mapc (lambda (lang)
	(unless (treesit-language-available-p lang)
	  (treesit-install-language-grammar lang)))
      (mapcar #'car treesit-language-source-alist))
