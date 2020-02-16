;Some time ago, I picked up a file of acronyms off of simtel20, I
;believe.  I recently posted my own version of this file to the
;unix-sources Internet mailing list.  My copy has almost 2400 entries
;now.  The file's in the format:
;
;ACRONYM<TAB>- Expansion Of Acronym
;
;E.g, the entry for ICBM is
;
;ICBM	- InterContinental Ballistic Missile
;
;I also posted a shell script called `whats' that acts as a front-end
;for looking up acronyms and adding new ones to the file.
;

;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; acronyms.el --- Various commands for use with an acronym database
;; Author          : dsill@relay.nswc.navy.mil
;; Created On      : Thu Feb 10 08:33:26 1989
;; Last Modified By: dsill
;; Last Modified On: Thu Feb 16 09:37:50 1989
;; Update Count    : 3
;; Status          : No known bugs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; History 		
;; 16-Feb-1989		dsill	
;;    Readied for first release.

(defvar acronym-file "~/misc/acron"
  "Acronym database.  Entries are in the form \"ACRONYM\\t- Expansion Of Acronym\".")

(defvar acronym-expansion-buffer "*Acronym Expansion*"
  "Buffer containing the expansion(s) of an acronym.")

(defun look-up-acronym (acronym)
  "Pop up a window containing the expansion(s) of ACRONYM.  If none are found,
the option to call add-acronym is provided."
  (interactive "sAcronym: ")
  (save-excursion
    (let ((buf (get-file-buffer acronym-file))
	  error)
      (if buf
	  (set-buffer buf)
	(setq buf (create-file-buffer acronym-file))
	(set-buffer buf)
	(erase-buffer)
	(insert-file-contents acronym-file t))
      (goto-char (point-min))
      (setq acronym-re (concat "^" (upcase acronym) "	- "))
      (if (re-search-forward acronym-re nil t)
	  (progn
	    (goto-char (point-min))
	    (save-excursion
	      (set-buffer (get-buffer-create acronym-expansion-buffer))
	      (fundamental-mode)
	      (if buffer-read-only (toggle-read-only))
	      (erase-buffer))
	    (while (re-search-forward acronym-re nil t)
	      (let ((beg (point)))
		(end-of-line)
		(copy-region-as-kill beg (point)))
	      (save-excursion
		(set-buffer acronym-expansion-buffer)
		(insert (car kill-ring-yank-pointer) ?\n)))
	    (display-buffer acronym-expansion-buffer t)
	    (set-buffer acronym-expansion-buffer)
	    (toggle-read-only))
	(if (y-or-n-p
	     (concat "Not in database, do you know the expansion of "
		     (upcase acronym) "? "))
	    (add-acronym acronym)
	  (message " "))))))

(defun add-acronym (acronym &optional expansion)
  "Add a new ACRONYM and its EXPANSION to the acronym-file."
  (interactive "sAcronym: ")
  (setq acronym (upcase acronym))
  (setq expansion (read-string (concat "Expansion of " acronym ": ")))
  (if (equal expansion "")
      ()
    (save-excursion
      (let ((buf (get-file-buffer acronym-file))
	    error)
	(if buf
	    (set-buffer buf)
	  (setq buf (create-file-buffer acronym-file))
	  (set-buffer buf)
	  (erase-buffer)
	  (insert-file-contents acronym-file t))
	(goto-char (point-min))
	(setq acronym-re (concat "^" acronym))
	(end-of-buffer)
	(beginning-of-line)
	(insert acronym "	- " expansion ?\n)
	(basic-save-buffer)
	(message (concat acronym " - " expansion))))))

;; finis


