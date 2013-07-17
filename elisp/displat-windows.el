;;; Displat Windows -- Convenient Window manipulation functions

(defun embiggen-window (cols)
  (interactive "nNumber of columns: ")
  (enlarge-window-horizontally cols))

(defun emsmallen-window (cols)
  (interactive "nNumber of columns: ")
  (shrink-window-horizontally cols))

(defun lengthen-window (lines)
  (interactive "nNumber of rows: ")
  (enlarge-window lines))

(defun shorten-window (lines)
  (interactive "nNumber of lines: ")
  (shrink-window lines))

