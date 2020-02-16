;;;
;;; init file for emacs under suntools
;;;
;;; Mon Feb 20 10:20:39 1989  Mark Niedzielski  (niedziel@copper)
;;;

;;;
;;;
;;; some extra key definitions for emacs on sun running emacstool
;;;
;;;

( defun define-extra-emacstool-keys ()

  ( interactive )


  ;
  ; First, the left side function keys
  ;
  (define-key suntool-map "aL" 'manual-entry)		 ; L1 

  (define-key suntool-map "cl" 'electric-buffer-list)    ; l3 
  (define-key suntool-map "cL" 'electric-command-history); L3 

  (define-key suntool-map "fL" 'open-rectangle)		 ; L6 

  (define-key suntool-map "hL" 'yank-rectangle)		 ; L8 

  (define-key suntool-map "iL" 'isearch-forward-regexp)	 ; L9 

  (define-key suntool-map "jL" 'kill-rectangle)		 ; L10

  ;
  ;
  ; Now, the top function keys
  ;
  (define-key suntool-map "at" 'describe-function)	 ; f1 
  (define-key suntool-map "aT" 'describe-variable)	 ; F1 
  (define-key suntool-map "a4" 'undefined)		 ; C-f1
						
  (define-key suntool-map "bt" 'shell)			 ; f2 
  (define-key suntool-map "bT" 'undefined)		 ; F2 
  (define-key suntool-map "b4" 'undefined)		 ; C-f2
						
  (define-key suntool-map "ct" 'scroll-down-in-place)	 ; f3
  (define-key suntool-map "cT" 'scroll-other-down)	 ; F3
  (define-key suntool-map "c4" 'undefined)		 ; C-f3
	     
  (define-key suntool-map "dt" 'scroll-up-in-place)	 ; f4
  (define-key suntool-map "dT" 'scroll-other-up)       	 ; F4
  (define-key suntool-map "d4" 'undefined)		 ; C-f4

  (define-key suntool-map "et" 'eval-current-buffer)	 ; f5 
  (define-key suntool-map "eT" 'eval-defun)		 ; F5 
  (define-key suntool-map "e4" 'undefined)		 ; C-f5

  (define-key suntool-map "it" 'allow-backup-files)	 ; f9
  (define-key suntool-map "iT" 'undefined)		 ; F9 
  (define-key suntool-map "i4" 'undefined)		 ; C-f9


  ;
  ; Now, the right side function keys
  ;
  (define-key suntool-map "ar" 'compile)                 ; r1
  (define-key suntool-map "aR" 'my-compile)              ; R1
  (define-key suntool-map "a2" 'grep)		         ; C-r1

  (define-key suntool-map "br" 'kill-compilation)	 ; r2
  (define-key suntool-map "bR" 'kill-compilation)	 ; R2
  (define-key suntool-map "b2" 'kill-compilation)	 ; C-r2

  (define-key suntool-map "cr" 'next-error)		 ; r3
  (define-key suntool-map "cR" 'next-error)		 ; R3
  (define-key suntool-map "c2" 'next-error)		 ; C-r3

  (define-key suntool-map "dr" 'backward-word)		 ; r4
  (define-key suntool-map "dR" 'backward-word)		 ; R4
  (define-key suntool-map "d2" 'backward-word)		 ; C-r4

  (define-key suntool-map "er" 'kill-word)		 ; r5
  (define-key suntool-map "eR" 'kill-word)		 ; R5
  (define-key suntool-map "e2" 'kill-word)		 ; C-r5

  (define-key suntool-map "fr" 'forward-word)		 ; r6
  (define-key suntool-map "fR" 'forward-word)		 ; R6
  (define-key suntool-map "f2" 'forward-word)		 ; C-r6

  (define-key suntool-map "hr" 'previous-line)		 ; r8
  (define-key suntool-map "hR" 'previous-line)		 ; R8
  (define-key suntool-map "h2" 'previous-line)		 ; C-r8
  (define-key suntool-map "h\er" 'previous-line)         ; M-r8

  (define-key suntool-map "jr" 'backward-char)		 ; r10
  (define-key suntool-map "jR" 'backward-word)		 ; R10
  (define-key suntool-map "j2" 'backward-char)		 ; C-r10
  (define-key suntool-map "j\er" 'backward-char)         ; M-r10

  (define-key suntool-map "lr" 'forward-char)		 ; r12
  (define-key suntool-map "lR" 'forward-word)		 ; R12
  (define-key suntool-map "l2" 'forward-char)		 ; C-r12
  (define-key suntool-map "l\er" 'forward-char)		 ; M-r12

  (define-key suntool-map "nr" 'next-line)		 ; r14
  (define-key suntool-map "nR" 'next-line)		 ; R14
  (define-key suntool-map "n2" 'next-line)		 ; C-r14
  (define-key suntool-map "n\er" 'next-line)		 ; M-r14

)

( defvar suntool-map-hooks '( ( define-extra-emacstool-keys ) ) )

