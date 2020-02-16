;;!emacs
;;
;; FILE:         info-mouse.el
;; SUMMARY:      Walks through Info networks using one mouse button
;; USAGE:        GNU Emacs Lisp Library
;;
;; AUTHOR:       Bob Weiner, Applied Research, Motorola, Inc., (407)738-2087
;; E-MAIL:       USENET:  weiner@novavax.UUCP
;; ORIG-DATE:    02/04/89
;; LAST-MOD:     02/08/89
;;
;; Info-mouse package for Emacs  -- could use a return after following
;;   cross-reference feature.
;; Copyright (C) 1989 Free Software Foundation, Inc.
;;
;; This file is not yet part of GNU Emacs.
;;
;; DESCRIPTION:  
;;;
;;; Note this code is machine independent.  Its only requirement is
;;; that you have a pointer device and an Emacs command that sets
;;; point to the location of the pointing device's cursor.
;;;
;;;
;;; To install:
;;;
;;; In your <GNUEMACS-LISP-LOCAL> directory, add this file and byte
;;; compile it.  Make sure that this directory is in your 'load-path'
;;; variable; we use the following lines in our ~/.emacs files:
;;;
;;;   ;; Add "lisp-local" directory to front of default library search
;;;   ;; path list 
;;;   (setq load-path '("/usr/local/gnu/emacs/lisp-local"
;;;                     "/usr/local/gnu/emacs/lisp")) 
;;;
;;; In your "site-load.el" file, add the line:
;;;
;;;   (load "info-mouse")
;;;
;;; Make sure that your mouse functions will also be loaded.
;;;
;;; (See the doc for the variable 'mouse-set-point-command' in this
;;; file.) 
;;; In your "site-init.el" file, add something akin to:
;;;
;;;   ;; Perform Apollo-specific setup using Zubkoff's Apollo DM
;;;   ;; extensions 
;;;   (let ((term (getenv "TERM")))
;;;     (if (equal term "apollo")
;;;         ;;
;;;         ;; The following settings are used by routines in
;;;         ;; <GNUEMACS-DIR>/lisp-local/info-mouse.el
;;;         ;;
;;;         (setq mouse-set-point-command 'apollo-mouse-move-point)
;;;         (bind-apollo-mouse-button "M2D" 'Info-mouse)
;;;         (unbind-apollo-mouse-button "M2U")))
;;;
;;;  OR
;;;
;;;   ;; Perform Sun-specific setup
;;;   ;; THIS SETUP NOT TESTED YET
;;;   ;; Make sure that UP transition of mouse button is unbound !
;;;
;;;   (let ((term (getenv "TERM")))
;;;     (if (equal term "sun")
;;;         ;;
;;;         ;; The following settings are used by routines in
;;;         ;; <GNUEMACS-DIR>/lisp-local/info-mouse.el
;;;         ;;
;;;         (setq mouse-set-point-command 'mouse-move-point)
;;;         (global-set-mouse '(text        middle) 'Info-mouse)))
;;;
;;;
;;; If Emacs dumps on your system, rebuild it.
;;;
;;; Run 'emacs'.  The mouse button that you have bound will walk you
;;; through Info networks.
;;;
;; DESCRIP-END.


(defvar mouse-set-point-command nil
"*Name of command that sets point to mouse cursor position.")

(defun Info-mouse ()
  "When bound to a mouse button, allows one to move through Info
documentation networks by using only that mouse button.  BE SURE TO CLEAR
ANY BINDING OF THE 'UP' TRANSITION OF THE MOUSE BUTTON THAT YOU BIND
THIS FUNCTION TO.

If button is clicked within:
 (1) any buffer other than *info*, then Info is called;
 (2) an Info Menu or Note, the desired node is found;
 (3) the Up,Next,or Previous entries of a Node Header (first line),
       the desired node is found;
 (4) the File entry of a Node Header (first line),       
       the 'Top' node within that file is found;
 (5) the *info* buffer but not within a node reference and not at the
     end of a node, the current node entry is scrolled up one screen
 (6) the *info* buffer at the end of the current node but not within
     a node reference, the Next node is found.

Returns t if button is clicked within an Info Node Header, Note
(cross-reference), or a Menu; otherwise returns nil.  Returns nil if
'mouse-set-point-command' is not bound to a valid function.
See mouse-set-point-command."

  (interactive)
  (if (not (fboundp mouse-set-point-command))
      nil
    ;;
    ;; Set point to cursor position
    ;;
    (funcall mouse-set-point-command)
    ;;
    ;; Test if in Info buffer already
    ;;
    (if (not (equal (buffer-name (current-buffer)) "*info*"))
	(info)
      (cond ((Info-handle-in-node-hdr))
	    ((Info-handle-in-note))
	    ;;
	    ;; If at end of node, go to next node
            ;;
	    ((last-line-p) (Info-next))
	    ((Info-handle-in-menu))
	    ;;
	    ;; If nothing else scroll forward one screen.
            ;;
	    ((scroll-up))
	    ))))

(defun Info-handle-in-node-hdr ()
  "If within an Info node header, move to <FILE>Top, <Up>, <Previous>, or
<Next> node, depending on which label point is on, and return t.
Otherwise, return nil."
  ;;
  ;; Test if on 1st line of node, i.e. node header
  ;;
  (let* ((first-line (1+ (count-lines 1 (point-min))))
	 (current-line (count-lines 1 (1+ (point)))))
    (if (not (equal current-line first-line))
	nil
      (let ((nodename "Top") (filep nil))
	(save-excursion
	  (if (re-search-forward "[:, \t\n]" nil t)
	      (progn
		(forward-char) ; Pass '[: \t]' char if point is in front of
		(if (re-search-backward
		      "\\(File\\|Node\\|Up\\|Prev\\|Previous\\|Next\\)[: \t]" nil t) 
		    (progn (setq filep (string-equal
					 "file"
					 (downcase (buffer-substring
						     (match-beginning 1)
						     (match-end 1)))))
			   (if (re-search-forward (concat ":[ \n]\\([^,.\t\n"
							  (if filep " ")
							  "]*\\)") nil t)
			       (setq nodename (buffer-substring
						(match-beginning 1)
						(match-end 1)))))))))
	(if filep (setq nodename (concat "(" nodename ")" "Top")))
	(Info-goto-node nodename)
	t))))

(defun Info-handle-in-note ()
  "If point is within an Info note (cross-reference), follow
cross-reference and return t; otherwise return nil."
  ;;
  ;; Test if point is within a Note
  ;;
  ;; Only works if entire "*Note NOTENAME" string is on one line.
  ;; Follows Note if user clicks anywhere on the line.
  ;;
  (let ((note-name nil) (bol nil))
    (save-excursion
      (if (re-search-forward "[:.\n]" nil t)
	    (progn
	      (forward-char) ; Pass ':' char if point is in front of
	      (save-excursion
		(beginning-of-line)
		(setq bol (point)))
	      (if (re-search-backward "\*Note[ \n]+\\([^:]*\\):" bol t)
		  (setq note-name (buffer-substring
				    (match-beginning 1)
				    (match-end 1)))))))
    (if (not note-name)
	nil
      (Info-follow-reference note-name)
      t)))

(defun Info-handle-in-menu ()
  "If point is within an Info menu entry, go to node referenced by
entry and return t; otherwise return nil."
  ;;
  ;; Test if there is a menu in this node
  ;;
  (let ((in-menu nil) (curr-point (point)))
    (save-excursion
      (goto-char (point-min))
      (setq in-menu 
	    (and (re-search-forward "^\* Menu:" nil t)
		 (< (point) curr-point))))
    (if (not in-menu)
	nil
      (forward-char) ; Pass '*' char if point is in front of
      (if (re-search-backward "^\*" nil t)
	  (progn (forward-char 2)
		 (Info-goto-node (Info-extract-menu-node-name))))
      t)))
    
;; end info-mouse.el

;; ************************************************************************
;;
;; Appended here for those who have Leonard Zubkoff's GNU Emacs 18.52
;; extensions for Apollo's DM window system is a function that is not
;; included in that distribution that unbinds a mouse key.
;; It is referred to in the info-mouse.el installation notes above.
;;
;;(defun unbind-apollo-mouse-button (mouse-button)
;;  "Disable an Apollo Mouse Button and return its control to DM."
;;  (interactive "sMouse Button: ")
;;  (let ((numeric-code (cdr (assoc mouse-button *apollo-mouse-buttons*))))
;;    (if (null numeric-code)
;;	(error "%s is not a legal Apollo function key name" mouse-button))
;;    (if (stringp numeric-code)
;;	(setq numeric-code
;;	      (cdr (assoc numeric-code *apollo-mouse-buttons*))))
;;    (disable-apollo-mouse-button numeric-code)))
;;
;; ************************************************************************



-- 
Bob Weiner, Motorola, Inc.,   USENET:  ...!gatech!uflorida!novavax!weiner
(407) 738-2087


