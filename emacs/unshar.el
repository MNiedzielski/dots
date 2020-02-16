;;
;; unshar.el -- unpack `shar' files under Emacs.
;;
;; Copyright (C) 1989 Free Software Foundation, if they want it.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.
;;
;; Author Dirk C. Grunwald (grunwald@flute.cs.uiuc.edu)
;; with some ideas liberal stolen from Dave Sill (dsill@relay.nswc.navy.mil)
;;

(defvar unshar-info-buffer "*Unshar Info*" 
  "* Buffer name of unshar output process *")

(defvar unshar-default-directory nil
  "*Default directory in which files are unshared")

(defun unshar-file (filename directory)
  "Unshar an existing file from within emacs into a directory.
If the directory does not exist, it is created. The default
directory is saved between invocations."
  
  (interactive (list
		(read-file-name "Unshar: "
			     (concat default-directory (unshar-current-word)))
		(read-file-name "To Directory: "
			     (if unshar-default-directory
				 unshar-default-directory
			       default-directory))))
  (let*
      ((make-backup-files nil)
       (temp-buffer (get-buffer-create (make-temp-name "shar")))
       (full-name (expand-file-name filename)))
    (setq directory (expand-file-name directory))
    (setq unshar-default-directory directory)
    ;;
    ;;	copy the file into another buffer
    ;;  (we'll be killing parts of the original file)
    ;;
    (save-excursion
      (set-buffer temp-buffer)
      (delete-region (point-min) (point-max))
      (goto-char (point-min))
      (insert-file full-name)
      (unshar-temporary-buffer directory)
      (kill-buffer temp-buffer))))

(defun unshar-buffer (directory)
  "Unshar the current buffer from within emacs into a directory.
If the directory does not exist, it is created. The default
directory is saved between invocations."
  (interactive (list
		(read-file-name "To Directory: "
			     (if unshar-default-directory
				 unshar-default-directory
			       default-directory))
		))
  (let*
      ((old-buffer (current-buffer))
       (temp-buffer (get-buffer-create (make-temp-name "shar"))))
    (setq directory (expand-file-name directory))
    (setq unshar-default-directory directory)
    ;;
    ;;	copy the file into another buffer
    ;;  (we'll be killing parts of the original file)
    ;;
    (save-excursion
      (set-buffer temp-buffer)
      (delete-region (point-min) (point-max))
      (goto-char (point-min))
      (insert-buffer old-buffer)
      (unshar-temporary-buffer directory)
      (kill-buffer temp-buffer))))


;;
;;	Boy howdy, wouldn't it be nice to have one standard version of this?
;;
(defun unshar-current-word ()
  "Get the current word, used by unshar-file."
  (save-excursion
    (let
	( beg end )
      ;;
      ;; Skip over white space
      (if (looking-at "[ 	]")
	  (re-search-forward "[ 	$]" nil t) )
      (if (not (re-search-forward "[ 	$]" nil t) )
	  (end-of-buffer))
      (re-search-backward "[^ 	$]" nil t)
      (setq beg (point))
      (re-search-backward "[ 	]")
      (if (looking-at "[ 	]")
	  (re-search-forward "[ 	$]" nil t) )
      (setq end (point))
      (buffer-substring beg end))))
;;
;
;	unshar-temporary-buffer does the actual unsharing, using the
;	current buffer. It assumes that it is free to step all over
;	the buffer.
;
(defun unshar-temporary-buffer (directory)
  (let
      ((mkdir-command nil))
    (if (not (file-directory-p directory))
	(if (y-or-n-p (concat "Create directory \"" directory "\" "))
	    (setq mkdir-command (concat "mkdir " directory " || exit \n"))))
    ;;
    ;; Find the comment line, indicating sh or csh
    ;;
    (if (re-search-forward "^#" nil t)
	(let
	    ((shell-name (if (looking-at "!/bin/csh")
			     "/bin/csh"
			   "/bin/sh"))
	     (old-buffer (current-buffer))
	     (send-point)
	     (process-connection-type nil)	; use pipe
	     (unshar-process nil)
	     (unshar-process-name (make-temp-name "*Unshar-Process" )))
	  (backward-char 1)
	  (setq send-point (point))
	  (insert (concat mkdir-command "cd " directory "\n pwd\n"))
	  ;;
	  ;;	Start the process
	  ;;
	  (if (get-buffer unshar-info-buffer)
	      (kill-buffer unshar-info-buffer))
	  (set-buffer (get-buffer-create unshar-info-buffer))
	  (display-buffer unshar-info-buffer nil)
	  (insert (concat "Unsharing buffer in directory "
			  directory "\n"))
	  (set-buffer old-buffer)
	  (call-process-region send-point (point-max) shell-name
			       nil unshar-info-buffer t))
      (error "I don't think this is a shar file."))))
