
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Simple non-standard functions to support key bindings	     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun delete-this-line ()
  "Delete current line, save as kill"
	(interactive)
	(let (curcol dot1)
	(setq curcol (current-column))
	(forward-line 0)
	(setq dot1 (dot))
	(forward-line 1)
	(kill-region dot1 (dot))
	(move-to-column curcol)))

(defun clear-this-line ()
  "Delete contents of current line, save as kill"
	(interactive)
	(forward-line 0)
	(kill-line))

(defun clear-entire-buffer ()
  "Delete contents of entire buffer, save as kill"
	(interactive)
	(mark-whole-buffer)
	(kill-region 1 (region-end)))

(defun switch-to-prev-buffer ()
  "Switch to previous buffer:  Like switch-to-buffer, but without interaction"
	(interactive)
	(switch-to-buffer (other-buffer (current-buffer))))

(defun start-end-kbd-macro ()
  "Start/stop capturing keystrokes for keyboard macro"
	(interactive)
	(if defining-kbd-macro
		(end-kbd-macro)
		(start-kbd-macro nil)))


( defvar grep-arg nil "Default arg for RE-search" )
( defun grep-arg ()
  (if (memq last-command '(research-forward research-backward)) grep-arg
    (let* ((command (car command-history))
	   (command-name (symbol-name (car command)))
	   (search-arg (car (cdr command)))
	   (search-command 
	    (and command-name (string-match "search" command-name)))
	   )
      (if (and search-command (stringp search-arg)) (setq grep-arg search-arg)
	(setq search-command this-command 
	      grep-arg (read-string "REsearch: " grep-arg)
	      this-command search-command)
	grep-arg)))
)

( defun research-forward ()
  "Repeat RE search forward."
  ( interactive )
  ( re-search-forward ( grep-arg ) )
)

( defun research-backward ()
  "Repeat RE search backward."
  ( interactive )
  ( re-search-backward ( grep-arg ) )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Map specific escape sequences	   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;;	Function key row along top of main cluster
;;;
(global-set-key [break]                 'compile)
(global-set-key [S-reset]               'grep)
(global-set-key [cancel]                'next-error)
(global-set-key [S-cancel]              'kill-compilation)
(global-set-key	[f1]			'start-end-kbd-macro)	; f1
(global-set-key	[f2]			'call-last-kbd-macro)	; f2
(global-set-key	[f3]			'bury-buffer)		; f3
(global-set-key	[f4]			'switch-to-prev-buffer)	; f4
(global-set-key	[menu]			'list-buffers)		; Menu
(global-set-key	[system]		'dired)			; System
(global-set-key	[S-user]		'rmail)			; User
(global-set-key	[f5]			'research-forward)	; f5
(global-set-key	[f6]			'research-backward)	; f6
(global-set-key [f7]			'replace-regexp)	; f7
(global-set-key [f8]			'query-replace-regexp)	; f8

(global-set-key	[clearline]		'clear-this-line)	; Clear line

;;; Not a good idea, too dangerous - let's do something nicer...
;;;(global-set-key	[clear]			'clear-entire-buffer)	; Clear display
(global-set-key	[clear]			'revert-buffer)	       	; Revert to latest version of file


;;;
;;;	Special purpose keys in main key cluster
;;;

(global-set-key	[deleteline]		'delete-this-line)	; Delete line

(global-set-key	[insertchar]		'overwrite-mode)	; Insert char

(global-set-key	[home]			'beginning-of-buffer)	; Home
(global-set-key	[S-home]		'end-of-buffer)		;
(global-set-key	[M-home]		'end-of-buffer)		;

(global-set-key	[select]		'newline-and-indent)	; Select


(global-set-key [up]			'previous-line)		; Up    arrow
(global-set-key [S-up]			'scroll-down)
(global-set-key [C-up]			'previous-line)
(global-set-key [M-up]  		'scroll-down)

(global-set-key [left]			'backward-char)		; Left  arrow
(global-set-key [S-left]		'backward-word)
(global-set-key [C-Left]		'backward-char)
(global-set-key [M-Left]		'backward-word)

(global-set-key [down]			'next-line)		; Down  arrow
(global-set-key [S-down]		'scroll-up)
(global-set-key [C-down]		'next-line)
(global-set-key [M-down]		'scroll-up)

(global-set-key [right]			'forward-char)		; Right arrow
(global-set-key [S-right]		'forward-word)
(global-set-key [C-right]		'forward-char)
(global-set-key [Meta_right]		'forward-word)
