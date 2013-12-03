;;ruby  stuff
(defun set-newline-and-indent ()
    (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'lisp-mode-hook 'set-newline-and-indent)

(require 'rvm)
(rvm-use-default)

(add-to-list 'load-path "~/.emacs.d/elisp/ruby-dev.el")
(autoload 'turn-on-ruby-dev "ruby-dev" nil t)
(add-hook 'ruby-mode-hook 'turn-on-ruby-dev)
(add-hook 'ruby-mode-hook 'robe-mode)

;;Robe Mode
(push 'company-robe company-backends)
(push 'ac-source-robe ac-sources)

(setq enh-ruby-program "/home/jason/.rubies/ruby-2.0.0-p247/bin/ruby")
(add-to-list 'load-path "~/.emacs.d/elisp/enhanced-ruby-mode") ; must be added after any path containing old ruby-mode
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(setq enh-ruby-bounce-deep-indent t)
(setq enh-ruby-hanging-brace-indent-level 2)

(require 'cl)
;;use pry for inf-ruby
(load-file "~/.emacs.d/elisp/inf-ruby-settings.el")
;;resense
(setq rsense-home "/opt/rsense-0.3")
(add-to-list 'load-path (concat rsense-home "/etc"))
(require 'rsense)
(require 'smartparens-ruby)
(push 'ac-source-robe ac-sources)

;;Projectile
(projectile-global-mode)
(projectile-enable-caching t)
(setq projectile-completion-system 'grizzl)
