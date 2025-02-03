
;; Utility functions and macros for the init files

(defmacro pkg (name)
  `(unless (package-installed-p ,name)
     (package-install ,name)))

;; Used for packages not in package.el repos 
;; Requires git
(defmacro pkg-git (name url)
  `(let* ((str-name (symbol-name ,name))
          (dir (concat user-emacs-directory str-name)))
     (unless (file-exists-p dir)
       (shell-command (concat "git clone " ,url " --depth 1 --quiet " dir)))
     (add-to-list 'load-path dir)))
