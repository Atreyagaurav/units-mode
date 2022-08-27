 ;;; units-mode.el --- conversion between different units -*- lexical-binding: t -*-

;; Author: Gaurav Atreya <allmanpride@gmail.com>
;; Maintainer:
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Homepage: https://github.com/Atreyagaurav/units-mode
;; Keywords: units,conversion,global-mode


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; This is emacs interface to gnu units program <https://www.gnu.org/software/units/units.html>.

;;; Code:

(defcustom units-binary-path "units"
  "Path to the units binary.")

(defcustom units-default-args "-t"
  "Default args to add to units command.")

(defcustom units-user-args ""
  "Extra args for user to add to units command.")

(defun units-command (value to-unit)
  (shell-command-to-string
   (format "%s %s %s \"%s\" \"%s\""
	   units-binary-path
	   units-default-args
	   units-user-args
	   value to-unit)))


(defun units-command-conformable (value)
  (shell-command-to-string
   (format "%s %s --conformable \"%s\""
	   units-binary-path
	   units-default-args
	   value)))

(defun units-convert (value to-unit)
  (let ((out-lines (split-string (string-trim-right (units-command value to-unit)) "\n")))
    (if (length= out-lines 1)
	(string-to-number (car out-lines))
      (user-error "%s" (string-join out-lines "\n")))))

(defun units-convert-single (value from-unit to-unit)
  (units-convert (format "%s %s" value from-unit) to-unit))


(defun units-conformable-list (value)
  (split-string
   (string-trim
    (units-command-conformable value)) "\n"))

(defun units-convert-region (region-text to-unit)
  (interactive
   (let ((region-text
	  (buffer-substring-no-properties
	   (region-beginning) (region-end))))
      (list region-text
	    (completing-read
	     "Convert to: "
	     (units-conformable-list region-text)))))
  (message "%s" (units-convert region-text to-unit)))

(defun units-convert-region-and-insert (region-text to-unit)
  (interactive
   (let ((region-text
	  (buffer-substring-no-properties
	   (region-beginning) (region-end))))
      (list region-text
	    (completing-read
	     "Convert to: "
	     (units-conformable-list region-text)))))
  (save-excursion
    (goto-char (region-end))
    (insert (concat " = "
		    (number-to-string
		     (units-convert region-text to-unit))
		    " "
		    to-unit))))

(provide 'units-mode)

;;; units-mode.el ends here
