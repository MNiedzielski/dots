;;; new-mouse-buffer-menu.el version 1.0

;;; Description:
;;; Improves the function mouse-buffer-menu distributed with Emacs
;;; 19.13.  This version shows the current buffer size in bytes and the
;;; modification and read-only flags, and adjusts the width of all
;;; columns so that the longest entry in each can fit.

;;; Usage:
;;; Include a line like the following in your .emacs file:
;;; (global-set-key [C-down-mouse-1] 'new-mouse-buffer-menu)

;;; Copyright:
;;; Copyright (C) 1993 Michael Maurer
;;; This code is based on the code for mouse-buffer-menu provided in
;;; mouse.el with version 19.13.
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Author:  Michael Maurer (maurer@nova.stanford.edu).

(defun new-mouse-buffer-menu (event)
  "Pop up a menu of buffers for selection with the mouse.
This switches buffers in the window that you clicked on,
and selects that window."
  (interactive "e")
  (let ((menu
	 (list
	  "Buffer Menu"
	  (cons
	   "Select Buffer"
	   (let ((tail (buffer-list))
		 (ptr (buffer-list))
		 (maxlen 0)
		 head)
	     (while ptr
	       (if (not (string-match "^ " (buffer-name (car ptr))))
		   (setq maxlen (max maxlen (length (buffer-name (car ptr))))))
	       (setq ptr (cdr ptr)))
	     (while tail
	       (save-excursion		; for set-buffer below
		 (let ((buf (car tail)))
		   (set-buffer buf)	; for buffer-read-only and buffer-size below
		   (if (not (string-match "^ " (buffer-name buf)))
		       (setq head (cons
				   (cons
				    (format
				     (concat "%s%s%" maxlen "s %8d %s")
				     (if (buffer-modified-p buf) "*" " ")
				     (if buffer-read-only "%" " ")
				     (buffer-name buf)
				     (buffer-size)
				     (or buffer-file-name
					 (concat "<" mode-name ">")))
				    buf)
				   head))))
		 (setq tail (cdr tail))))
	     (reverse head))))))
    (let ((buf (x-popup-menu event menu))
	  (window (posn-window (event-start event))))
      (if buf
	  (progn
	    (or (framep window) (select-window window))
	    (switch-to-buffer buf))))))

;;; (global-set-key [C-down-mouse-1] 'new-mouse-buffer-menu)
