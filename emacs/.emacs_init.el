;;;
;;; init file for emacs on Mac OSX
;;;
;;; Mon Feb 20 10:20:39 1989  Mark Niedzielski  (niedziel@copper)
;;; Fri 11 Apr 11:00:27 2014  Mark Niedzielski  (mniedzielski@paypal)
;;;

(setq mail-default-reply-to "mark.niedzielski@celect.com")

;;
;; Better mouse buttons
;;
(defun x-kill-rect (arg &optional kill)
  "Like kill-rectangle, but mouse based."
  (if (coordinates-in-window-p arg (selected-window))
      (save-excursion
	(let ((opoint (point))
	      beg end)
	  (x-mouse-set-point arg)
	  (setq beg (min opoint (point))
		end (max opoint (point)))
	  (x-store-cut-buffer (buffer-substring beg end))
	  (kill-rectangle beg end)
	  (if kill (delete-region beg end))
	  )
	)
    (message "Mouse not in selected window")
    )
)

(defun x-yank-rect (arg &optional kill)
  "Paste that rectangle back somewhere."
  (save-excursion
    (yank-rectangle)
  )
)

;;
;; Do you care for the menu bar along the top...
;;
( menu-bar-mode -1 )

;;
;; Mode Line
;;
(setq-default mode-line-format
              '("%e" mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                "   "
                mode-line-position
                (12 (:eval (if (use-region-p)
                               (format "{%d,w%d,c%d}"
                                       (abs (- (line-number-at-pos (point))
                                               (line-number-at-pos (mark))))
                                       (count-words-region (point) (mark))
                                       (abs (- (point) (mark)))
                                       )
                             (format "[%d]" (point))
                             )
                           )
                    )
                (vc-mode vc-mode)
;                (vc-state (buffer-file-name (current-buffer)))           ; 
;                (vc-working-revision (buffer-file-name (current-buffer)))
                "   "
                mode-line-modes
                )
              )

;; Default
;(setq-default mode-line-format
;              '("%e" mode-line-front-space
;                mode-line-mule-info
;                mode-line-client
;                mode-line-modified
;                mode-line-remote
;                mode-line-frame-identification
;                mode-line-buffer-identification
;                "   "
;                mode-line-position
;                (vc-mode vc-mode)
;                "  "
;                mode-line-modes
;                mode-line-misc-info
;                mode-line-end-spaces)
;              )

;
; Wierd, not for everyone.
; Causes neat highlighting, but problems with cut & paste...
;
( transient-mark-mode t )

;;;
;;; Style
;;;

; Tabs
( setq default-tab-width 8 )
( set-default 'indent-tabs-mode t )

; Tabs in JSON are two spaces and never hard tabs.
( add-hook 'json-mode-hook
  ( lambda ()
    ( make-local-variable 'js-indent-level )
    ( setq js-indent-level 2 )
    ( setq-default indent-tabs-mode nil )
  )
)

; And four spaces for Puppet
( setq puppet-indent-level 4 )

; Flymake
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))
  
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)


;
; C style
;
;( setq c-auto-newline t )
( setq c-indent-level 8 )
( setq c-brace-offset -8 )
( setq c-label-offset 0 )
( setq c-argdecl-indent 0 )
( setq c-continued-statement-offset 8 )
( setq c-continued-brace-offset 8 )
( setq c-tabs-should-be-tabs t )

( define-key c-mode-map "{"        'electric-c-semi)
( define-key c-mode-map "\e*KP_F1" 'c-header )
( define-key c-mode-map "\e*KP_F2" 'c-stdinc )
( define-key c-mode-map "\e*KP_F3" 'c-main )
( define-key c-mode-map "\e*KP_F4" 'c-add-comment )

( defun c-header ()
  "insert my standard C header at point."
  ( interactive )
  ( progn
    ( insert
      "/*\n"
      ( buffer-name )
      " --\nthoughts from "
    )
    ( insert
      ( user-full-name )
    )
    ( insert
      "\n*/\n"
    )
  )
)

( defun c-stdinc ()
  "insert my standard C includes at point."
  ( interactive )
  ( insert "#include <stdio.h>\n#include <math.h>\n#include <sys/types.h>\n\n" )
)

( defun c-main ()
  "insert my standard C main() at point."
  ( interactive )
  ( insert "void	main(argc, argv)\nint	argc;\nchar	*argv[];\n{\n}\n" )
)

( defun c-func ()
  "insert my standard C func() at point."
  ( interactive )
  ( insert "\n/*\n*/\n	()\n{\n\n}\n" )
)

( defun c-add-comment ()
  ( interactive )
  ( beginning-of-line 1)
  ( insert "/* " )
  ( end-of-line 1)
  ( insert " */" )
  ( forward-line 1 )
)

;
; For those of us who prefer <TAB> to really insert a tab.
; (not reformat the line)
;
( defun insert-hard-tab ()
  ( interactive )
  ( insert ( quote "\t" ) )
)
( defun set-c-hard-tabs ()
  ( interactive )
  ( setq c-tab-always-indent nil )
  ( define-key c-mode-map "\211" 'insert-hard-tab )
  ( define-key c++-mode-map "\211" 'insert-hard-tab )
)

( cond
  ( c-tabs-should-be-tabs
    ( defvar c-tab-always-indent nil )
    ( defvar c-mode-hook 'set-c-hard-tabs )
    ( define-key c-mode-map "\t" 'insert-hard-tab )
    ( define-key c++-mode-map "\t" 'insert-hard-tab )
    ( define-key c-mode-map "\" 'delete-backward-char ) ; \177 octal
    ( define-key c++-mode-map "\" 'delete-backward-char ) ; \177 octal
  )
)

( defun my-cmode-hook ()
  ( interactive )
  ( set-c-hard-tabs )

;  ( setq comment-multi-line t )
;  ( setq comment-start "/*\n* " )
;  ( setq comment-end "\n*/" )

)

( cond
  ( c-tabs-should-be-tabs
    ( defvar c-tab-always-indent nil )
    ( defvar c-mode-hook 'my-cmode-hook )
    ( define-key c-mode-map "\211" 'insert-hard-tab )
    ( define-key c-mode-map "\177" 'delete-backward-char ) ; \177 octal
    ( defvar c++-mode-hook 'my-cmode-hook )
    ( define-key c++-mode-map "\211" 'insert-hard-tab )
    ( define-key c++-mode-map "\177" 'delete-backward-char ) ; \177 octal
  )
)


;;;
;;; Key Bindings
;;;
( define-key esc-map "g" 'goto-line )

( define-key ctl-x-map "x" 'flip )
( define-key ctl-x-map "!" 'compile )
( define-key ctl-x-map "" 'save-buffer )
( define-key ctl-x-map "@" 'kill-compilation )

( define-key global-map "\034" 'research-forward )	    ; ctl-\
( setq search-repeat-char 28 )                              ; control-\
( setq search-exit-char 13 )                                ; carriage return

( define-key global-map "\C-x\C-b" 'select-another-buffer ) ; redef default
( autoload 'select-another-buffer "select-buf" nil t )

( define-key global-map "\C-xr" 'remote-editing )
( autoload 'remote-editing "~/emacs/remote" )
( autoload 'find-remote-file "~/emacs/remote" )
( autoload 'save-remote-file "~/emacs/remote" )
( autoload 'write-remote-file "~/emacs/remote" )


( defun flip ()
  ( interactive )
  ( switch-to-buffer ( other-buffer ) )
)

( defun insert-me ()
  "insert date and username at point."
  ( interactive )
  ( progn
    ( insert
      ( current-time-string )
      "  " ( user-full-name )
      "  (" ( user-login-name )
      "@" ( system-name )
      ")"
    )
  )
)

( defun insert-date ()
  "insert date and username at point."
  ( interactive )
  ( progn
    ( insert
      ( current-time-string )
    )
  )
)


( fset 'my-compile	"*ar12^" )

;( fset 'clean-shell	"[A[A*gr*mr" )
;( fset 'to-shell       	"b*shell*" )

;( defun my-shell-setup ()
;  ( interactive )
;  ( define-key global-map "*bt" 'to-shell )			; F2
;  ( define-key shell-mode-map "*bt" 'clean-shell )		; F2
;)

;( defvar shell-mode-hook 'my-shell-setup )


( defun scroll-other-up ()
  ( interactive )
  ( scroll-other-window ( - 0 ( window-height ) ) )
)

( defun scroll-other-down ()
  ( interactive )
  ( scroll-other-window ( window-height ) )
)

( defun allow-backup-files ()
  ( interactive )
  ( setq make-backup-files ( not make-backup-files ) )
  ( princ "make-backup-files = " )
  ( print make-backup-files )
)

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

;
; Recognize file extensions
;
( setq
  auto-mode-alist
  ( append
    auto-mode-alist
    '(
      ("\\.C$"   . c++-mode);
      ("\\.cc$"  . c++-mode);
      ("\\.c$"   . c++-mode);
      ("\\.h$"   . c++-mode);
      ("\\.ol$"  . outline-mode)	; outline-mode upon editings a .ol file
      ("\\.tex$" . tex-mode)	; outline-mode upon editings a .ol file
    )
  )
)

( defun auto-revert ()
  "revert-buffer with no questions asked."
  ( interactive )
  ( revert-buffer nil t )
  ( recenter )
)

( setq my-last-killed-buffer nil )

( defun my-kill-buffer ()
  "Get rid of the specified buffer. But remember the filename" 
  ( interactive )
  ( setq my-last-killed-buffer buffer-file-name )
  ( kill-buffer ( current-buffer ) )
)

( defun my-unkill-buffer ()
  "Recall the file of the buffer that we just killed"
  ( interactive )
  ( find-file my-last-killed-buffer )
)

;;;
;;; ispell stuff
;;;
( autoload 'ispell-word "ispell" "Check spelling of word at or before point" t )
( autoload 'ispell-complete-word "ispell" "Complete word at or before point" t )
( autoload 'ispell-region "ispell" "Check spelling of every word in the region" t )
( autoload 'ispell-buffer "ispell" "Check spelling of every word in the buffer" t )


;;;
;;;
;;; Crud...
;;;
;;;

;(fset 'insert_2 "[^\" \"	]*Left  *Down")
;( define-key global-map	"\e*KP_Tab"	'insert_2 )

;(fset 'extract-prompt
;   "*Down÷*Up*Meta_Right(me_token_type()) {*Up*Down*Up*Meta_Right*Meta_Right*Meta_Leftstate->prompt = lola*Meta_Left\");*Down÷")
;( define-key global-map	"\e*KP_Decimal"	'extract-prompt )
