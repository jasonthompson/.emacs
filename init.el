(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(setq url-http-attempt-keepalives nil)

(defvar my-packages
  '(auto-complete clojure-mode fuzzy geiser highlight idomenu markdown-mode mic-paren
	     nrepl paredit popup rainbow-delimiters rainbow-mode undo-tree magit ac-nrepl)
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
(set-default-font "SourceCodePro 12")
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






































(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#3A3A3A" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(custom-safe-themes (quote ("541b58826d3d7abe525a8a4c88046e0a4c81a753b1b94ce4a1020e5f2eef15a2" default)))
 '(fci-rule-color "#383838")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#7F9F7F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
