;;; displat-zenburn-theme.el --- Zenburn with more contrast

;; Copyright (C) 2013 Jason Thompson
;; Author: Jason Thompson <jason@jthompson.ca>
;; Url: http://github.com/jasonthompson/displat-zenburn
;;
;; Based on zenburn-theme by:
;; Copyright (C) 2011-2013 Bozhidar Batsov
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://github.com/bbatsov/zenburn-emacs
;; 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a copy of the GNU General Public License,
;;  see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The original zenburn-theme does not have enough contrast for my
;; laptop for working outside, so I am slowly tweaking it. So far I've
;; darkened the background and made the comment colour lighter.

;;; Original Commentary
;; A port of the popular Vim theme Zenburn for Emacs 24, built on top
;; of the new built-in theme support in Emacs 24.

;;; Credits:
;; Bozhidar Batsov created the original emacs port.
;; Jani Nurminen created the original theme for vim on which
;; Batsov's port is based.

;;; Code:

(deftheme displat-zenburn "The displat-zenburn color theme")

;;; Color Palette

(defvar displat-zenburn-colors-alist
  '(("displat-zenburn-fg"       . "#DCDCCC")
    ("displat-zenburn-fg-1"     . "#656555")
    ("displat-zenburn-bg-2"     . "#252525")
    ("displat-zenburn-bg-1"     . "#2B2B2B")
    ("displat-zenburn-bg-05"    . "#383838")
    ("displat-zenburn-bg"       . "#3A3A3A")
    ("displat-zenburn-bg+1"     . "#4F4F4F")
    ("displat-zenburn-bg+2"     . "#5F5F5F")
    ("displat-zenburn-bg+3"     . "#6F6F6F")
    ("displat-zenburn-red+1"    . "#DCA3A3")
    ("displat-zenburn-red"      . "#CC9393")
    ("displat-zenburn-red-1"    . "#BC8383")
    ("displat-zenburn-red-2"    . "#AC7373")
    ("displat-zenburn-red-3"    . "#9C6363")
    ("displat-zenburn-red-4"    . "#8C5353")
    ("displat-zenburn-orange"   . "#DFAF8F")
    ("displat-zenburn-yellow"   . "#F0DFAF")
    ("displat-zenburn-yellow-1" . "#E0CF9F")
    ("displat-zenburn-yellow-2" . "#D0BF8F")
    ("displat-zenburn-green-1"  . "#7F9F7F")
    ("displat-zenburn-green"    . "#7F9F7F")
    ("displat-zenburn-green+1"  . "#8FB28F")
    ("displat-zenburn-green+2"  . "#9FC59F")
    ("displat-zenburn-green+3"  . "#AFD8AF")
    ("displat-zenburn-green+4"  . "#BFEBBF")
    ("displat-zenburn-cyan"     . "#93E0E3")
    ("displat-zenburn-blue+1"   . "#94BFF3")
    ("displat-zenburn-blue"     . "#8CD0D3")
    ("displat-zenburn-blue-1"   . "#7CB8BB")
    ("displat-zenburn-blue-2"   . "#6CA0A3")
    ("displat-zenburn-blue-3"   . "#5C888B")
    ("displat-zenburn-blue-4"   . "#4C7073")
    ("displat-zenburn-blue-5"   . "#366060")
    ("displat-zenburn-magenta"  . "#DC8CC3"))
  "List of Displat-Zenburn colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro displat-zenburn-with-color-variables (&rest body)
  "`let' bind all colors defined in `displat-zenburn-colors-alist'.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   displat-zenburn-colors-alist))
     ,@body))

;;; Theme Faces
(displat-zenburn-with-color-variables
  (custom-theme-set-faces
   'displat-zenburn
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,displat-zenburn-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,displat-zenburn-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,displat-zenburn-fg :background ,displat-zenburn-bg-2))))
   `(cursor ((t (:foreground ,displat-zenburn-fg :background "white"))))
   `(escape-glyph ((t (:foreground ,displat-zenburn-yellow :bold t))))
   `(fringe ((t (:foreground ,displat-zenburn-fg :background ,displat-zenburn-bg+1))))
   `(header-line ((t (:foreground ,displat-zenburn-yellow
                                  :background ,displat-zenburn-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,displat-zenburn-bg-05))))
   `(success ((t (:foreground ,displat-zenburn-green :weight bold))))
   `(warning ((t (:foreground ,displat-zenburn-orange :weight bold))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,displat-zenburn-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,displat-zenburn-green))))
   `(compilation-error-face ((t (:foreground ,displat-zenburn-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,displat-zenburn-fg))))
   `(compilation-info-face ((t (:foreground ,displat-zenburn-blue))))
   `(compilation-info ((t (:foreground ,displat-zenburn-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,displat-zenburn-green))))
   `(compilation-line-face ((t (:foreground ,displat-zenburn-yellow))))
   `(compilation-line-number ((t (:foreground ,displat-zenburn-yellow))))
   `(compilation-message-face ((t (:foreground ,displat-zenburn-blue))))
   `(compilation-warning-face ((t (:foreground ,displat-zenburn-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,displat-zenburn-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,displat-zenburn-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,displat-zenburn-yellow :weight bold))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,displat-zenburn-fg))))
   `(grep-error-face ((t (:foreground ,displat-zenburn-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,displat-zenburn-blue))))
   `(grep-match-face ((t (:foreground ,displat-zenburn-orange :weight bold))))
   `(match ((t (:background ,displat-zenburn-bg-1 :foreground ,displat-zenburn-orange :weight bold))))
;;;;; isearch
   `(isearch ((t (:foreground ,displat-zenburn-yellow-2 :weight bold :background ,displat-zenburn-bg-1))))
   `(isearch-fail ((t (:foreground ,displat-zenburn-fg :background ,displat-zenburn-red-4))))
   `(lazy-highlight ((t (:foreground ,displat-zenburn-yellow-2 :weight bold :background ,displat-zenburn-bg-05))))

   `(menu ((t (:foreground ,displat-zenburn-fg :background ,displat-zenburn-bg))))
   `(minibuffer-prompt ((t (:foreground ,displat-zenburn-yellow))))
   `(mode-line
     ((,class (:foreground ,displat-zenburn-green+1
                           :background ,displat-zenburn-bg-1
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,displat-zenburn-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,displat-zenburn-green-1
                      :background ,displat-zenburn-bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,displat-zenburn-bg-1))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,displat-zenburn-bg+2))))
   `(trailing-whitespace ((t (:background ,displat-zenburn-red))))
   `(vertical-border ((t (:foreground ,displat-zenburn-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,displat-zenburn-cyan))))
   `(font-lock-comment-face ((t (:foreground ,displat-zenburn-green+2))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,displat-zenburn-green+2))))
   `(font-lock-constant-face ((t (:foreground ,displat-zenburn-green+4))))
   `(font-lock-doc-face ((t (:foreground ,displat-zenburn-green+1))))
   `(font-lock-doc-string-face ((t (:foreground ,displat-zenburn-blue-2))))
   `(font-lock-function-name-face ((t (:foreground ,displat-zenburn-blue))))
   `(font-lock-keyword-face ((t (:foreground ,displat-zenburn-yellow :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,displat-zenburn-fg))))
   `(font-lock-preprocessor-face ((t (:foreground ,displat-zenburn-blue+1))))
   `(font-lock-string-face ((t (:foreground ,displat-zenburn-red))))
   `(font-lock-type-face ((t (:foreground ,displat-zenburn-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,displat-zenburn-orange))))
   `(font-lock-warning-face ((t (:foreground ,displat-zenburn-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,displat-zenburn-fg))))
   `(newsticker-default-face ((t (:foreground ,displat-zenburn-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,displat-zenburn-green+3))))
   `(newsticker-extra-face ((t (:foreground ,displat-zenburn-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,displat-zenburn-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,displat-zenburn-green))))
   `(newsticker-new-item-face ((t (:foreground ,displat-zenburn-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,displat-zenburn-red))))
   `(newsticker-old-item-face ((t (:foreground ,displat-zenburn-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,displat-zenburn-fg))))
   `(newsticker-treeview-face ((t (:foreground ,displat-zenburn-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,displat-zenburn-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,displat-zenburn-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,displat-zenburn-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,displat-zenburn-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,displat-zenburn-bg+3))))
   `(newsticker-treeview-selection-face ((t (:foreground ,displat-zenburn-yellow))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,displat-zenburn-fg-1 :background ,displat-zenburn-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,displat-zenburn-green+2 :background ,displat-zenburn-bg :inverse-video nil))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,displat-zenburn-fg))))
   `(ack-file ((t (:foreground ,displat-zenburn-blue))))
   `(ack-line ((t (:foreground ,displat-zenburn-yellow))))
   `(ack-match ((t (:foreground ,displat-zenburn-orange :background ,displat-zenburn-bg-1 :weight bold))))
;;;;; auctex
   `(font-latex-bold ((t (:inherit bold))))
   `(font-latex-warning ((t (:inherit font-lock-warning))))
   `(font-latex-sedate ((t (:foreground ,displat-zenburn-yellow :weight bold ))))
   `(font-latex-title-4 ((t (:inherit variable-pitch :weight bold))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,displat-zenburn-bg+3 :foreground "black"))))
   `(ac-selection-face ((t (:background ,displat-zenburn-blue-4 :foreground ,displat-zenburn-fg))))
   `(popup-tip-face ((t (:background ,displat-zenburn-yellow-2 :foreground "black"))))
   `(popup-scroll-bar-foreground-face ((t (:background ,displat-zenburn-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,displat-zenburn-bg-1))))
   `(popup-isearch-match ((t (:background ,displat-zenburn-bg :foreground ,displat-zenburn-fg))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,displat-zenburn-green+1))))
   `(android-mode-error-face ((t (:foreground ,displat-zenburn-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,displat-zenburn-fg))))
   `(android-mode-verbose-face ((t (:foreground ,displat-zenburn-green))))
   `(android-mode-warning-face ((t (:foreground ,displat-zenburn-yellow))))
;;;;; bm
   `(bm-face ((t (:background ,displat-zenburn-yellow-1 :foreground ,displat-zenburn-bg))))
   `(bm-fringe-face ((t (:background ,displat-zenburn-yellow-1 :foreground ,displat-zenburn-bg))))
   `(bm-fringe-persistent-face ((t (:background ,displat-zenburn-green-1 :foreground ,displat-zenburn-bg))))
   `(bm-persistent-face ((t (:background ,displat-zenburn-green-1 :foreground ,displat-zenburn-bg))))
;;;;; clojure-test-mode
   `(clojure-test-failure-face ((t (:foreground ,displat-zenburn-orange :weight bold :underline t))))
   `(clojure-test-error-face ((t (:foreground ,displat-zenburn-red :weight bold :underline t))))
   `(clojure-test-success-face ((t (:foreground ,displat-zenburn-green+1 :weight bold :underline t))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,displat-zenburn-blue :foreground ,displat-zenburn-bg))))
   `(ctbl:face-continue-bar ((t (:background ,displat-zenburn-bg-05 :foreground ,displat-zenburn-bg))))
   `(ctbl:face-row-select ((t (:background ,displat-zenburn-cyan :foreground ,displat-zenburn-bg))))
;;;;; diff
   `(diff-added ((,class (:foreground ,displat-zenburn-green+4 :background nil))
                 (t (:foreground ,displat-zenburn-green-1 :background nil))))
   `(diff-changed ((t (:foreground ,displat-zenburn-yellow))))
   `(diff-removed ((,class (:foreground ,displat-zenburn-red :background nil))
                   (t (:foreground ,displat-zenburn-red-3 :background nil))))
   `(diff-refine-added ((t :inherit diff-added :weight bold)))
   `(diff-refine-change ((t :inherit diff-changed :weight bold)))
   `(diff-refine-removed ((t :inherit diff-removed :weight bold)))
   `(diff-header ((,class (:background ,displat-zenburn-bg+2))
                  (t (:background ,displat-zenburn-fg :foreground ,displat-zenburn-bg))))
   `(diff-file-header
     ((,class (:background ,displat-zenburn-bg+2 :foreground ,displat-zenburn-fg :bold t))
      (t (:background ,displat-zenburn-fg :foreground ,displat-zenburn-bg :bold t))))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,displat-zenburn-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,displat-zenburn-orange))))
   `(diredp-date-time ((t (:foreground ,displat-zenburn-magenta))))
   `(diredp-deletion ((t (:foreground ,displat-zenburn-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,displat-zenburn-red))))
   `(diredp-dir-heading ((t (:foreground ,displat-zenburn-blue :background ,displat-zenburn-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,displat-zenburn-cyan))))
   `(diredp-exec-priv ((t (:foreground ,displat-zenburn-red))))
   `(diredp-executable-tag ((t (:foreground ,displat-zenburn-green+1))))
   `(diredp-file-name ((t (:foreground ,displat-zenburn-blue))))
   `(diredp-file-suffix ((t (:foreground ,displat-zenburn-green))))
   `(diredp-flag-mark ((t (:foreground ,displat-zenburn-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,displat-zenburn-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,displat-zenburn-red))))
   `(diredp-link-priv ((t (:foreground ,displat-zenburn-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,displat-zenburn-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,displat-zenburn-orange))))
   `(diredp-no-priv ((t (:foreground ,displat-zenburn-fg))))
   `(diredp-number ((t (:foreground ,displat-zenburn-green+1))))
   `(diredp-other-priv ((t (:foreground ,displat-zenburn-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,displat-zenburn-red-1))))
   `(diredp-read-priv ((t (:foreground ,displat-zenburn-green-1))))
   `(diredp-symlink ((t (:foreground ,displat-zenburn-yellow))))
   `(diredp-write-priv ((t (:foreground ,displat-zenburn-magenta))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,displat-zenburn-green+4 :background ,displat-zenburn-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,displat-zenburn-red :background ,displat-zenburn-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,displat-zenburn-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,displat-zenburn-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment))))
   `(eshell-ls-directory ((t (:foreground ,displat-zenburn-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,displat-zenburn-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,displat-zenburn-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning))))
   `(eshell-ls-product ((t (:inherit font-lock-doc))))
   `(eshell-ls-special ((t (:foreground ,displat-zenburn-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,displat-zenburn-cyan :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,displat-zenburn-red) :inherit unspecified))
      (t (:foreground ,displat-zenburn-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,displat-zenburn-orange) :inherit unspecified))
      (t (:foreground ,displat-zenburn-orange :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,displat-zenburn-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,displat-zenburn-orange :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,displat-zenburn-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,displat-zenburn-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,displat-zenburn-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,displat-zenburn-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,displat-zenburn-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,displat-zenburn-green-1 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,displat-zenburn-orange) :inherit unspecified))
      (t (:foreground ,displat-zenburn-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,displat-zenburn-red) :inherit unspecified))
      (t (:foreground ,displat-zenburn-red-1 :weight bold :underline t))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,displat-zenburn-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning))))
   `(erc-default-face ((t (:foreground ,displat-zenburn-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default))))
   `(erc-error-face ((t (:inherit font-lock-warning))))
   `(erc-fool-face ((t (:inherit erc-default))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,displat-zenburn-yellow))))
   `(erc-keyword-face ((t (:foreground ,displat-zenburn-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,displat-zenburn-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,displat-zenburn-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default))))
   `(erc-notice-face ((t (:foreground ,displat-zenburn-green))))
   `(erc-pal-face ((t (:foreground ,displat-zenburn-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,displat-zenburn-orange :background ,displat-zenburn-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,displat-zenburn-green+1))))
   `(erc-underline-face ((t (:underline t))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,displat-zenburn-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,displat-zenburn-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,displat-zenburn-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,displat-zenburn-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,displat-zenburn-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,displat-zenburn-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,displat-zenburn-magenta :weight bold))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-from))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((t (:foreground ,displat-zenburn-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,displat-zenburn-blue))))
   `(gnus-summary-high-read ((t (:foreground ,displat-zenburn-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,displat-zenburn-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,displat-zenburn-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,displat-zenburn-blue))))
   `(gnus-summary-low-read ((t (:foreground ,displat-zenburn-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,displat-zenburn-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,displat-zenburn-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,displat-zenburn-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,displat-zenburn-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,displat-zenburn-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,displat-zenburn-fg))))
   `(gnus-summary-selected ((t (:foreground ,displat-zenburn-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,displat-zenburn-blue))))
   `(gnus-cite-10 ((t (:foreground ,displat-zenburn-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,displat-zenburn-yellow))))
   `(gnus-cite-2 ((t (:foreground ,displat-zenburn-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,displat-zenburn-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,displat-zenburn-green+2))))
   `(gnus-cite-5 ((t (:foreground ,displat-zenburn-green+1))))
   `(gnus-cite-6 ((t (:foreground ,displat-zenburn-green))))
   `(gnus-cite-7 ((t (:foreground ,displat-zenburn-red))))
   `(gnus-cite-8 ((t (:foreground ,displat-zenburn-red-1))))
   `(gnus-cite-9 ((t (:foreground ,displat-zenburn-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,displat-zenburn-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,displat-zenburn-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,displat-zenburn-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,displat-zenburn-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,displat-zenburn-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,displat-zenburn-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,displat-zenburn-bg+2))))
   `(gnus-signature ((t (:foreground ,displat-zenburn-yellow))))
   `(gnus-x ((t (:background ,displat-zenburn-fg :foreground ,displat-zenburn-bg))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,displat-zenburn-blue))))
   `(guide-key/key-face ((t (:foreground ,displat-zenburn-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,displat-zenburn-green+1))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,displat-zenburn-green
                      :background ,displat-zenburn-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,displat-zenburn-yellow
                      :background ,displat-zenburn-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,displat-zenburn-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,displat-zenburn-bg+1))))
   `(helm-visible-mark ((t (:foreground ,displat-zenburn-bg :background ,displat-zenburn-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,displat-zenburn-green+4 :background ,displat-zenburn-bg-1))))
   `(helm-ff-directory ((t (:foreground ,displat-zenburn-magenta))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,displat-zenburn-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,displat-zenburn-bg-05)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,displat-zenburn-bg+1))
                   (t :weight bold)))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,displat-zenburn-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,displat-zenburn-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,displat-zenburn-yellow))))
;;;;; js2-mode
   `(js2-warning-face ((t (:underline ,displat-zenburn-orange))))
   `(js2-error-face ((t (:foreground ,displat-zenburn-red :weight bold))))
   `(js2-jsdoc-tag-face ((t (:foreground ,displat-zenburn-green-1))))
   `(js2-jsdoc-type-face ((t (:foreground ,displat-zenburn-green+2))))
   `(js2-jsdoc-value-face ((t (:foreground ,displat-zenburn-green+3))))
   `(js2-function-param-face ((t (:foreground, displat-zenburn-green+3))))
   `(js2-external-variable-face ((t (:foreground ,displat-zenburn-orange))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,displat-zenburn-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,displat-zenburn-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,displat-zenburn-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,displat-zenburn-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,displat-zenburn-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,displat-zenburn-red+1))))
   `(jabber-activity-face((t (:foreground ,displat-zenburn-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,displat-zenburn-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; linum-mode
   `(linum ((t (:foreground ,displat-zenburn-green+2 :background ,displat-zenburn-bg))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,displat-zenburn-green+2 :background ,displat-zenburn-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,displat-zenburn-red+1 :background ,displat-zenburn-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,displat-zenburn-blue+1 :background ,displat-zenburn-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,displat-zenburn-magenta :background ,displat-zenburn-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,displat-zenburn-yellow :background ,displat-zenburn-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
   `(magit-section-title ((t (:foreground ,displat-zenburn-yellow :weight bold))))
   `(magit-branch ((t (:foreground ,displat-zenburn-orange :weight bold))))
   `(magit-item-highlight ((t (:background ,displat-zenburn-bg+1 :bold nil))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,displat-zenburn-fg))))
   `(egg-help-header-1 ((t (:foreground ,displat-zenburn-yellow))))
   `(egg-help-header-2 ((t (:foreground ,displat-zenburn-green+3))))
   `(egg-branch ((t (:foreground ,displat-zenburn-yellow))))
   `(egg-branch-mono ((t (:foreground ,displat-zenburn-yellow))))
   `(egg-term ((t (:foreground ,displat-zenburn-yellow))))
   `(egg-diff-add ((t (:foreground ,displat-zenburn-green+4))))
   `(egg-diff-del ((t (:foreground ,displat-zenburn-red+1))))
   `(egg-diff-file-header ((t (:foreground ,displat-zenburn-yellow-2))))
   `(egg-section-title ((t (:foreground ,displat-zenburn-yellow))))
   `(egg-stash-mono ((t (:foreground ,displat-zenburn-green+4))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment))))
   `(message-header-name ((t (:foreground ,displat-zenburn-green+1))))
   `(message-header-other ((t (:foreground ,displat-zenburn-green))))
   `(message-header-to ((t (:foreground ,displat-zenburn-yellow :weight bold))))
   `(message-header-from ((t (:foreground ,displat-zenburn-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,displat-zenburn-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,displat-zenburn-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,displat-zenburn-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,displat-zenburn-green))))
   `(message-mml ((t (:foreground ,displat-zenburn-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,displat-zenburn-orange))))
   `(mew-face-header-from ((t (:foreground ,displat-zenburn-yellow))))
   `(mew-face-header-date ((t (:foreground ,displat-zenburn-green))))
   `(mew-face-header-to ((t (:foreground ,displat-zenburn-red))))
   `(mew-face-header-key ((t (:foreground ,displat-zenburn-green))))
   `(mew-face-header-private ((t (:foreground ,displat-zenburn-green))))
   `(mew-face-header-important ((t (:foreground ,displat-zenburn-blue))))
   `(mew-face-header-marginal ((t (:foreground ,displat-zenburn-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,displat-zenburn-red))))
   `(mew-face-header-xmew ((t (:foreground ,displat-zenburn-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,displat-zenburn-red))))
   `(mew-face-body-url ((t (:foreground ,displat-zenburn-orange))))
   `(mew-face-body-comment ((t (:foreground ,displat-zenburn-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,displat-zenburn-green))))
   `(mew-face-body-cite2 ((t (:foreground ,displat-zenburn-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,displat-zenburn-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,displat-zenburn-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,displat-zenburn-red))))
   `(mew-face-mark-review ((t (:foreground ,displat-zenburn-blue))))
   `(mew-face-mark-escape ((t (:foreground ,displat-zenburn-green))))
   `(mew-face-mark-delete ((t (:foreground ,displat-zenburn-red))))
   `(mew-face-mark-unlink ((t (:foreground ,displat-zenburn-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,displat-zenburn-green))))
   `(mew-face-mark-unread ((t (:foreground ,displat-zenburn-red-2))))
   `(mew-face-eof-message ((t (:foreground ,displat-zenburn-green))))
   `(mew-face-eof-part ((t (:foreground ,displat-zenburn-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,displat-zenburn-cyan :background ,displat-zenburn-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,displat-zenburn-bg :background ,displat-zenburn-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,displat-zenburn-bg :background ,displat-zenburn-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,displat-zenburn-blue))))
   `(mingus-pausing-face ((t (:foreground ,displat-zenburn-magenta))))
   `(mingus-playing-face ((t (:foreground ,displat-zenburn-cyan))))
   `(mingus-playlist-face ((t (:foreground ,displat-zenburn-cyan ))))
   `(mingus-song-file-face ((t (:foreground ,displat-zenburn-yellow))))
   `(mingus-stopped-face ((t (:foreground ,displat-zenburn-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,displat-zenburn-yellow))))
   `(nav-face-button-num ((t (:foreground ,displat-zenburn-cyan))))
   `(nav-face-dir ((t (:foreground ,displat-zenburn-green))))
   `(nav-face-hdir ((t (:foreground ,displat-zenburn-red))))
   `(nav-face-file ((t (:foreground ,displat-zenburn-fg))))
   `(nav-face-hfile ((t (:foreground ,displat-zenburn-red-4))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,displat-zenburn-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,displat-zenburn-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,displat-zenburn-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,displat-zenburn-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,displat-zenburn-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,displat-zenburn-green-1 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,displat-zenburn-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,displat-zenburn-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,displat-zenburn-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,displat-zenburn-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,displat-zenburn-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,displat-zenburn-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,displat-zenburn-bg+1))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground "white" :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,displat-zenburn-fg :weight bold))))
   `(org-checkbox ((t (:background ,displat-zenburn-bg+2 :foreground "white"
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,displat-zenburn-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,displat-zenburn-red-1))))
   `(org-done ((t (:bold t :weight bold :foreground ,displat-zenburn-green+3))))
   `(org-formula ((t (:foreground ,displat-zenburn-yellow-2))))
   `(org-headline-done ((t (:foreground ,displat-zenburn-green+3))))
   `(org-hide ((t (:foreground ,displat-zenburn-bg-1))))
   `(org-level-1 ((t (:foreground ,displat-zenburn-orange))))
   `(org-level-2 ((t (:foreground ,displat-zenburn-green+4))))
   `(org-level-3 ((t (:foreground ,displat-zenburn-blue-1))))
   `(org-level-4 ((t (:foreground ,displat-zenburn-yellow-2))))
   `(org-level-5 ((t (:foreground ,displat-zenburn-cyan))))
   `(org-level-6 ((t (:foreground ,displat-zenburn-green+2))))
   `(org-level-7 ((t (:foreground ,displat-zenburn-red-4))))
   `(org-level-8 ((t (:foreground ,displat-zenburn-blue-4))))
   `(org-link ((t (:foreground ,displat-zenburn-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,displat-zenburn-green+4))))
   `(org-scheduled-previously ((t (:foreground ,displat-zenburn-red-4))))
   `(org-scheduled-today ((t (:foreground ,displat-zenburn-blue+1))))
   `(org-special-keyword ((t (:foreground ,displat-zenburn-fg-1 :weight normal))))
   `(org-table ((t (:foreground ,displat-zenburn-green+2))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,displat-zenburn-orange))))
   `(org-todo ((t (:bold t :foreground ,displat-zenburn-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,displat-zenburn-red :weight bold :underline nil))))
   `(org-column ((t (:background ,displat-zenburn-bg-1))))
   `(org-column-title ((t (:background ,displat-zenburn-bg-1 :underline t :weight bold))))
;;;;; outline
   `(outline-1 ((t (:foreground ,displat-zenburn-orange))))
   `(outline-2 ((t (:foreground ,displat-zenburn-green+4))))
   `(outline-3 ((t (:foreground ,displat-zenburn-blue-1))))
   `(outline-4 ((t (:foreground ,displat-zenburn-yellow-2))))
   `(outline-5 ((t (:foreground ,displat-zenburn-cyan))))
   `(outline-6 ((t (:foreground ,displat-zenburn-green+2))))
   `(outline-7 ((t (:foreground ,displat-zenburn-red-4))))
   `(outline-8 ((t (:foreground ,displat-zenburn-blue-4))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,displat-zenburn-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,displat-zenburn-green+2))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,displat-zenburn-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,displat-zenburn-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,displat-zenburn-green-1))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,displat-zenburn-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,displat-zenburn-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,displat-zenburn-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,displat-zenburn-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,displat-zenburn-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,displat-zenburn-green))))
   `( rainbow-delimiters-depth-12-face ((t (:foreground ,displat-zenburn-blue-5))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,displat-zenburn-blue))))
   `(rcirc-other-nick ((t (:foreground ,displat-zenburn-orange))))
   `(rcirc-bright-nick ((t (:foreground ,displat-zenburn-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,displat-zenburn-blue-2))))
   `(rcirc-server ((t (:foreground ,displat-zenburn-green))))
   `(rcirc-server-prefix ((t (:foreground ,displat-zenburn-green+1))))
   `(rcirc-timestamp ((t (:foreground ,displat-zenburn-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,displat-zenburn-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:bold t))))
   `(rcirc-prompt ((t (:foreground ,displat-zenburn-yellow :bold t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:bold t))))
   `(rcirc-url ((t (:bold t))))
   `(rcirc-keyword ((t (:foreground ,displat-zenburn-yellow :bold t))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,displat-zenburn-green))))
   `(rpm-spec-doc-face ((t (:foreground ,displat-zenburn-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,displat-zenburn-red))))
   `(rpm-spec-macro-face ((t (:foreground ,displat-zenburn-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,displat-zenburn-red))))
   `(rpm-spec-package-face ((t (:foreground ,displat-zenburn-red))))
   `(rpm-spec-section-face ((t (:foreground ,displat-zenburn-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,displat-zenburn-blue))))
   `(rpm-spec-var-face ((t (:foreground ,displat-zenburn-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,displat-zenburn-orange))))
   `(rst-level-2-face ((t (:foreground ,displat-zenburn-green+1))))
   `(rst-level-3-face ((t (:foreground ,displat-zenburn-blue-1))))
   `(rst-level-4-face ((t (:foreground ,displat-zenburn-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,displat-zenburn-cyan))))
   `(rst-level-6-face ((t (:foreground ,displat-zenburn-green-1))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,displat-zenburn-red-3 :background ,displat-zenburn-bg :weight bold))))
   `(show-paren-match ((t (:foreground ,displat-zenburn-blue-1 :background ,displat-zenburn-bg :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-inputed-output-face ((t (:foreground ,displat-zenburn-red))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,displat-zenburn-fg
                                    :background ,displat-zenburn-bg))))
   `(tabbar-selected ((t (:foreground ,displat-zenburn-fg
                                      :background ,displat-zenburn-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,displat-zenburn-fg
                                        :background ,displat-zenburn-bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,displat-zenburn-bg
                                       :background ,displat-zenburn-bg-1))))
   `(term-color-red ((t (:foreground ,displat-zenburn-red-2
                                       :background ,displat-zenburn-red-4))))
   `(term-color-green ((t (:foreground ,displat-zenburn-green
                                       :background ,displat-zenburn-green+2))))
   `(term-color-yellow ((t (:foreground ,displat-zenburn-orange
                                       :background ,displat-zenburn-yellow))))
   `(term-color-blue ((t (:foreground ,displat-zenburn-blue-1
                                      :background ,displat-zenburn-blue-4))))
   `(term-color-magenta ((t (:foreground ,displat-zenburn-magenta
                                         :background ,displat-zenburn-red))))
   `(term-color-cyan ((t (:foreground ,displat-zenburn-cyan
                                       :background ,displat-zenburn-blue))))
   `(term-color-white ((t (:foreground ,displat-zenburn-fg
                                       :background ,displat-zenburn-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,displat-zenburn-bg-05))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,displat-zenburn-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,displat-zenburn-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,displat-zenburn-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,displat-zenburn-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,displat-zenburn-green+2 :background ,displat-zenburn-bg))))
   `(w3m-lnum-match ((t (:background ,displat-zenburn-bg-1
                                     :foreground ,displat-zenburn-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,displat-zenburn-yellow))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,displat-zenburn-bg+1 :foreground ,displat-zenburn-bg+1))))
   `(whitespace-hspace ((t (:background ,displat-zenburn-bg+1 :foreground ,displat-zenburn-bg+1))))
   `(whitespace-tab ((t (:background ,displat-zenburn-red-1))))
   `(whitespace-newline ((t (:foreground ,displat-zenburn-bg+1))))
   `(whitespace-trailing ((t (:background ,displat-zenburn-red))))
   `(whitespace-line ((t (:background ,displat-zenburn-bg :foreground ,displat-zenburn-magenta))))
   `(whitespace-space-before-tab ((t (:background ,displat-zenburn-orange :foreground ,displat-zenburn-orange))))
   `(whitespace-indentation ((t (:background ,displat-zenburn-yellow :foreground ,displat-zenburn-red))))
   `(whitespace-empty ((t (:background ,displat-zenburn-yellow))))
   `(whitespace-space-after-tab ((t (:background ,displat-zenburn-yellow :foreground ,displat-zenburn-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,displat-zenburn-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,displat-zenburn-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,displat-zenburn-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,displat-zenburn-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,displat-zenburn-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,displat-zenburn-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,displat-zenburn-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,displat-zenburn-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,displat-zenburn-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,displat-zenburn-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,displat-zenburn-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,displat-zenburn-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,displat-zenburn-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,displat-zenburn-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,displat-zenburn-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,displat-zenburn-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,displat-zenburn-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,displat-zenburn-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,displat-zenburn-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,displat-zenburn-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,displat-zenburn-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,displat-zenburn-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,displat-zenburn-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,displat-zenburn-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,displat-zenburn-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,displat-zenburn-green+4))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,displat-zenburn-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,displat-zenburn-bg-1 :foreground ,displat-zenburn-bg-1))))
   ))

;;; Theme Variables
(displat-zenburn-with-color-variables
  (custom-theme-set-variables
   'displat-zenburn
;;;;; ansi-color
   `(ansi-color-names-vector [,displat-zenburn-bg ,displat-zenburn-red ,displat-zenburn-green ,displat-zenburn-yellow
                                          ,displat-zenburn-blue ,displat-zenburn-magenta ,displat-zenburn-cyan ,displat-zenburn-fg])
;;;;; fill-column-indicator
   `(fci-rule-color ,displat-zenburn-bg-05)
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,displat-zenburn-red-1)
       ( 40. . ,displat-zenburn-red)
       ( 60. . ,displat-zenburn-orange)
       ( 80. . ,displat-zenburn-yellow-2)
       (100. . ,displat-zenburn-yellow-1)
       (120. . ,displat-zenburn-yellow)
       (140. . ,displat-zenburn-green-1)
       (160. . ,displat-zenburn-green)
       (180. . ,displat-zenburn-green+1)
       (200. . ,displat-zenburn-green+2)
       (220. . ,displat-zenburn-green+3)
       (240. . ,displat-zenburn-green+4)
       (260. . ,displat-zenburn-cyan)
       (280. . ,displat-zenburn-blue-2)
       (300. . ,displat-zenburn-blue-1)
       (320. . ,displat-zenburn-blue)
       (340. . ,displat-zenburn-blue+1)
       (360. . ,displat-zenburn-magenta)))
   `(vc-annotate-very-old-color ,displat-zenburn-magenta)
   `(vc-annotate-background ,displat-zenburn-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar displat-zenburn-add-font-lock-keywords nil
  "Whether to add font-lock keywords for displat-zenburn color names.
In buffers visiting library `displat-zenburn-theme.el' the displat-zenburn
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar displat-zenburn-colors-font-lock-keywords nil)

;; (defadvice rainbow-turn-on (after displat-zenburn activate)
;;   "Maybe also add font-lock keywords for displat-zenburn colors."
;;   (when (and (derived-mode-p 'emacs-lisp-mode)
;;              (or displat-zenburn-add-font-lock-keywords
;;                  (equal (file-name-nondirectory (buffer-file-name))
;;                         "displat-zenburn-theme.el")))
;;     (unless displat-zenburn-colors-font-lock-keywords
;;       (setq displat-zenburn-colors-font-lock-keywords
;;             `((,(regexp-opt (mapcar 'car displat-zenburn-colors-alist) 'words)
;;                (0 (rainbow-colorize-by-assoc displat-zenburn-colors-alist))))))
;;     (font-lock-add-keywords nil displat-zenburn-colors-font-lock-keywords)))

;; (defadvice rainbow-turn-off (after displat-zenburn activate)
;;   "Also remove font-lock keywords for displat-zenburn colors."
;;   (font-lock-remove-keywords nil displat-zenburn-colors-font-lock-keywords))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'displat-zenburn)

;;;###autoload
(add-to-list 'safe-local-eval-forms
             '(when (require 'rainbow-mode nil t) (rainbow-mode 1)))

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; displat-zenburn-theme.el ends here
