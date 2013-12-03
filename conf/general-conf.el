;; General Settings
(setq-default indent-tabs-mode nil)
(load-file "~/.emacs.d/elisp/displat-windows.el")
(add-to-list 'load-path "~/.emacs.d/elisp/")
(setq linum-format "%3d")
(global-linum-mode t)
(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(setq visible-bell t)

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

;;Functions
(defun whack-whitespace (arg)
      "Delete all white space from point to the next word.  With prefix ARG
    delete across newlines as well.  The only danger in this is that you
    don't have to actually be at the end of a word to make it work.  It
    skips over to the next whitespace and then whacks it all to the next
    word."
      (interactive "P")
      (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
                (re-search-forward regexp nil t)
                        (replace-match "" nil nil)))
