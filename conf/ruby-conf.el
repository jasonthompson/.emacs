;;ruby  stuff
(defun set-newline-and-indent ()
    (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'lisp-mode-hook 'set-newline-and-indent)


(setq enh-ruby-program "~/.rubies/ruby-2.0.0-p247/bin/ruby")
(add-to-list 'load-path "~/.emacs.d/elisp/enhanced-ruby-mode") ; must be added after any path containing old ruby-mode
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(setq enh-ruby-bounce-deep-indent t)
(setq enh-ruby-hanging-brace-indent-level 2)

;;use pry for inf-ruby shell
;;https://gist.github.com/jsvnm/1390890
(add-to-list 'inf-ruby-implementations '("pry" . "pry"))
(setq inf-ruby-default-implementation "pry")
(setq inf-ruby-first-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)> *")
(setq inf-ruby-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)[>*\"'] *")

(require 'smartparens-ruby)

;;Projectile
(projectile-global-mode)
(setq projectile-completion-system 'grizzl)

;;Robe Mode
;(push 'company-robe company-backends)
(push 'ac-source-robe ac-sources)
