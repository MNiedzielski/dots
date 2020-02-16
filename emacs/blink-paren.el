;; Blink the matching paren, just like Zmacs.  By devin@lucid.com.
;; Ported to version 19 by Jonathan Stigelman <Stig@netcom.com>
;; modified by Ray Nickson (nickson@cs.uq.oz.au).
;; Copyright (C) 1992-1993 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(copy-face 'default 'blink-paren-off)  ; used to be region
(copy-face 'default 'blink-paren-on)
(invert-face 'blink-paren-on)

;;; The blinking paren alternates between the faces blink-paren-on and
;;; blink-paren-off.  The default is for -on to look like the region text,
;;; and -off to be the reverse of the default.  You can change this so that,
;;; for example, the blinking paren fluctuates between bold and italic...

;; overlay used to change the face of the matching paren
(defvar blink-paren-overlay nil)

;; interval (in seconds) between blinks
(defvar blink-paren-interval 0.5)

;; find if we should look foward or backward to find the matching paren
(defun blink-paren-sexp-dir ()
  (cond ((and (< (point) (point-max))
	      (eq (char-syntax (char-after (point))) ?\())
	 1)
	((and (> (point) (point-min))
	      (eq (char-syntax (char-after (- (point) 1))) ?\)))
	 -1)
	(t nil)))

;; make an overlay on the matching paren if any.  return it.
(defun blink-paren-make-overlay ()
  (let ((dir (blink-paren-sexp-dir)))
    (and dir
	 (condition-case nil
	     (let* ((other-pos (save-excursion (forward-sexp dir) (point)))
		    (overlay (if (= dir 1)
				(make-overlay (- other-pos 1) other-pos)
			      (make-overlay other-pos (+ other-pos 1)))))
	       (overlay-put overlay 'face 'blink-paren-on)
	       overlay)
	   (error nil)))))

;; called after each command is executed in the post-command-hook
;; add the overlay and blink if we are on a paren.
(defun blink-paren-post-command ()
  (blink-paren-pre-command)
  (if (setq blink-paren-overlay (blink-paren-make-overlay))
      (while (sit-for blink-paren-interval)
        (overlay-put blink-paren-overlay 'face
                     (if (eq (overlay-get blink-paren-overlay 'face)
                             'blink-paren-on)
                         'blink-paren-off
                       'blink-paren-on)))))
            
;; called before a new command is executed in the pre-command-hook
;; cleanup by removing the overlay
(defun blink-paren-pre-command ()
  (condition-case c  ; don't ever signal an error in pre-command-hook!
      (let ((inhibit-quit t))
	(if blink-paren-overlay
	    (delete-overlay (prog1 blink-paren-overlay
			     (setq blink-paren-overlay nil)))))
    (error
     (message "blink paren error! %s" c))))


; install

(add-hook 'pre-command-hook 'blink-paren-pre-command)
(add-hook 'post-command-hook 'blink-paren-post-command)
; I like both at the same time...  Stig
;(setq blink-matching-paren nil)  ; don't need this loser any more

(provide 'blink-paren)

