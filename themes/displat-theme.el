;;; zenburn-theme.el --- A low contrast color theme for Emacs.

;; Copyright (C) 2011-2013 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://github.com/bbatsov/zenburn-emacs
;; Version: 2.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A port of the popular Vim theme Zenburn for Emacs 24, built on top
;; of the new built-in theme support in Emacs 24.

;;; Credits:

;; Jani Nurminen created the original theme for vim on such this port
;; is based.

;;; Code:

(deftheme displat "The displat color theme")

;;; Color Palette
(defvar displat-colours-alist
  (("displat-light-grey" . "#FAFAFA")
   ("displat-fg"         . "#DDDDDD")
   ("displat-fg-2"       . "#BBBBBB")
   ("displat-bg"         . "#2B2B2B")
   ("displat-bg-dark"    . "#1A1A1A")
   ("displat-red"        . "#CC4242")
   ("displat-orange"     . "#FFBB45")
   ("displat-yellow"     . "#FAFA02")
   ("displat-dark-green" . "#66F73E")
   ("displat-cyan"       . "#93E0E3")
   ("displat-blue"       . "#3C9093")
   ("displat-magenta"    . "#DC8CC3")))

(defmacro zenburn-with-color-variables (&rest body)
  "`let' bind all colors defined in `zenburn-colors-alist'.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   zenburn-colors-alist))
     ,@body))

  (custom-theme-set-faces
   'displat
 ;; Frame
 `(default ((t (:foreground ,displat-fg :background ,displat-bg))))
 `(cursor ((t (:foreground ,displat-magenta))))
 `(hl-line ((t (:background ,displat-bg-dark))))
 `(minibuffer-prompt ((t (:foreground ,displat-fg-2))))
 `(modeline ((t (:background ,displat-fg :foreground ,displat-fg))))
 `(region ((t (:background ,displat-blue))))
 `(show-paren-match-face ((t (:background ,displat-light-grey))))
 ;; Main
 `(font-lock-builtin-face ((t (:foreground ,displat-green))))
 `(font-lock-comment-face ((t (:foreground ,displat-orange))))
 `(font-lock-constant-face ((t (:foreground ,displat-magenta))))
 `(font-lock-doc-string-face ((t (:foreground ,displat-yellow))))
 `(font-lock-function-name-face ((t (:foreground ,displat-green))))
 `(font-lock-keyword-face ((t (:foreground ,displat-magenta))))
 `(font-lock-string-face ((t (:foreground ,displat-yellow))))
 `(font-lock-type-face ((t (:foreground ,displat-cyan))))
 `(font-lock-variable-name-face ((t (:foreground ,displat-magenta))))
 `(font-lock-warning-face ((t (:bold t :foreground ,dispalt-magenta))))))

