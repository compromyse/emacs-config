;;; simple-modeline-segments.el --- The segments for simple-modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021  Eder Elorriaga

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The segments for simple-modeline

;;; Code:

(require 'subr-x)

(defun simple-modeline-make-mouse-map (mouse function)
  "Return a keymap with single entry for mouse key MOUSE on the mode line.
MOUSE is defined to run function FUNCTION with no args in the buffer
corresponding to the mode line clicked."
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'mode-line mouse) function)
    map))

(defun simple-modeline-segment-modified ()
  "Displays a color-coded buffer modification/read-only indicator in the mode-line."
  (if (not (string-match-p "\\*.*\\*" (buffer-name)))
      (let* ((read-only (and buffer-read-only (buffer-file-name)))
             (modified (buffer-modified-p)))
        (propertize
         (if read-only " " (if modified " ●" " ○"))
         'face `(:inherit
                 ,(if modified 'simple-modeline-status-modified
                    (if read-only 'simple-modeline-status-error
                      'simple-modeline-unimportant)))
         'help-echo (format
                     "Buffer is %s and %smodified\nmouse-1: Toggle read-only status."
                     (if read-only "read-only" "writable")
                     (if modified "" "not "))
         'local-map (purecopy (simple-modeline-make-mouse-map
                               'mouse-1
                               (lambda (event)
                                 (interactive "e")
                                 (with-selected-window (posn-window (event-start event))
                                   (read-only-mode 'toggle)))))
         'mouse-face 'mode-line-highlight))))

(defun simple-modeline-segment-buffer-name ()
 "Displays the name of the current buffer in the mode-line."
 (propertize " %b" 'face 'mode-line-buffer-id))

(defun simple-modeline-segment-vc ()
 "Displays color-coded version control information in the mode-line."
 '(:eval (when vc-mode (concat "" (substring vc-mode 5)))))

(defvar simple-modeline-segment-encoding-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
      (lambda (e)
	(interactive "e")
	(with-selected-window (posn-window (event-start e))
	  (when (and enable-multibyte-characters
		     buffer-file-coding-system)
	    (describe-coding-system buffer-file-coding-system)))))
    (define-key map [mode-line mouse-3]
      (lambda (e)
	(interactive "e")
	(with-selected-window (posn-window (event-start e))
	  (call-interactively #'set-buffer-file-coding-system))))
    (purecopy map))
  "Local keymap for the coding-system part of the simple-modeline.")

(defun simple-modeline-segment-major-mode ()
 "Displays the current major mode in the mode-line."
 (propertize
  (concat " "
          (or (and (boundp 'delighted-modes)
                   (cadr (assq major-mode delighted-modes)))
              (format-mode-line mode-name)))
  'face 'bold))

(provide 'simple-modeline-segments)
;;; simple-modeline-segments.el ends here
