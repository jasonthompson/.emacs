(require 'rvm)
(require 'inf-ruby)
(add-to-list 'load-path "~/.emacs.d/elisp/ruby-dev.el")
(autoload 'turn-on-ruby-dev "ruby-dev" nil t)
(add-hook 'enh-ruby-mode-hook 'turn-on-ruby-dev)

(setq enh-ruby-program "~/.rubies/ruby-2.0.0-p247/bin/ruby")
(add-to-list 'load-path "~/.emacs.d/elisp/enhanced-ruby-mode") ; must be added after any path containing old ruby-mode
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(require 'smartparens-ruby)

;;Robe Mode
;(push 'company-robe company-backends)
;(push 'ac-source-robe ac-sources)

