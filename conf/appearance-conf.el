;; Theme Stuff
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'displat-zenburn t)
(if windows-p
    (set-default-font "Source Code Pro-11")
    (set-default-font "SourceCodePro 12"))
