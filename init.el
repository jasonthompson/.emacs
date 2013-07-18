(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(setq url-http-attempt-keepalives nil)

(defvar my-packages
  '(ac-nrepl auto-complete clojure-mode fuzzy geiser highlight idomenu markdown-mode mic-paren
	     nrepl paredit popup rainbow-delimiters rainbow-mode undo-tree)
  "A list of packages to ensure are installed at launch.")

(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package)) 
           (package-install package))))
 my-packages)

(provide 'my-packages)


;; Files to load
(load-file "~/.emacs.d/elisp/displat-windows.el")

;; General Settings
(set-face-attribute 'default nil :font "SourceCodePro 12")
(add-to-list 'load-path "~/.emacs.d/elisp/")
(setq linum-format "%3d")
(global-linum-mode t)
(setq inhibit-splash-screen t)
(tool-bar-mode -1)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; Save all tempfiles in $TMPDIR/emacs$UID/                                                        
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

;; Clojure Stuff
(add-hook 'nrepl-interaction-mode-hook
  'nrepl-turn-on-eldoc-mode)
(require 'mic-paren)
(paren-activate)

;; Chuck stuff
(require 'chuck-mode)

;; Highlight expression on eval
(require 'easymenu)
(require 'nrepl-eval-sexp-fu)
(require 'highlight)
(turn-on-nrepl-eval-sexp-fu-flash-mode)
(setq nrepl-eval-sexp-fu-flash-duration 0.5)

(require 'rainbow-delimiters)

(load-file "~/.emacs.d/conf/auto-complete-conf.el")
(load-file "~/.emacs.d/conf/nrepl-conf.el")
(load-file "~/.emacs.d/conf/paredit-conf.el")
(load-file "~/.emacs.d/conf/clojure-conf.el")

;; Theme Stuff
(add-to-list 'custom-theme-load-path "/home/jason/.emacs.d/themes/")
(load-theme 'displat-zenburn t)


;;bindings
(load-file "~/.emacs.d/lib/bindings.el")






































