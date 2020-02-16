; initialization file for vt100/vt220 terminals
; Thu Sep  3 09:59:59 1987  Doug Lea  (dl at rocky)
;
(defvar SS3-map nil
  "SS3-map maps the SS3 function & numeric keys on the VT220 keyboard.
   The functions provided are:

  -----------------------------------------------------------------
  |PF1            |PF2            |PF3            |PF4            |
  |find-file      |find-file      |switch-to-buff |save-buffer    |
  |               |other-window   |other-window   |               |
  |---------------+---------------+---------------+---------------|
  |7              |8              |9              |-              |
  |beginning-of   |end-of-        |backward-      |forward-       |
  |buffer         |buffer         |paragraph      |paragraph      |
  |---------------+---------------+---------------+---------------|
  |4              |5              |6              |,              |
  |beginning-of   |end-of-line    |open-previous  |open-next-line |
  |line           |               |line           |               |
  |---------------+---------------+---------------+---------------|
  |1              |2              |3              |Enter          |
  |backward-word  |forward-word   |kill-word      |               |
  |               |               |               |               |
  |---------------+---------------+---------------|               |
  |0                              |.              |execute        |
  |scroll-up                      |scroll-down    |extended       |
  |                               |               |command        |
  -----------------------------------------------------------------")

(setq SS3-map (make-keymap))                          ;; <ESC>O command map
(define-key SS3-map "A" 'previous-line)               ;; up arrow
(define-key SS3-map "B" 'next-line)                   ;; down-arrow
(define-key SS3-map "C" 'forward-char)                ;; right-arrow
(define-key SS3-map "D" 'backward-char)               ;; left-arrow
(define-key SS3-map "M" 'execute-extended-command)    ;; Enter
(define-key SS3-map "P" 'find-file)                   ;; PF1
(define-key SS3-map "Q" 'find-file-other-window)      ;; PF2
(define-key SS3-map "R" 'switch-to-buffer-other-window) ;; PF3
(define-key SS3-map "S" 'save-buffer)                 ;; PF4
(define-key SS3-map "l" 'open-next-line)              ;; ,
(define-key SS3-map "m" 'forward-paragraph)           ;; -
(define-key SS3-map "n" 'scroll-down)                 ;; .
(define-key SS3-map "p" 'scroll-up)                   ;; 0
(define-key SS3-map "q" 'backward-word)               ;; 1
(define-key SS3-map "r" 'forward-word)                ;; 2
(define-key SS3-map "s" 'kill-word)                   ;; 3
(define-key SS3-map "t" 'beginning-of-line)           ;; 4
(define-key SS3-map "u" 'end-of-line)                 ;; 5
(define-key SS3-map "v" 'open-previous-line)          ;; 6
(define-key SS3-map "w" 'beginning-of-buffer)         ;; 7
(define-key SS3-map "x" 'end-of-buffer)               ;; 8
(define-key SS3-map "y" 'backward-paragraph)          ;; 9

(define-key global-map "\eO" SS3-map)                 ; enable fct key map

; CSI keymap

     (setq CSI-map (make-keymap))  ;; <ESC>[ commands

;
; keypad over arrow keys
;
;
;  -------------------------------------------------
;  |Find           |Insert Here    |Remove         |
;  |isearch-       |quoted-insert  |kill-region    |
;  |forward-regexp |               |               |
;  |---------------+---------------+---------------+
;  |Select         |Prev Screen    |Next Screen    |
;  |copy-region-as |flip-window    |other-window   |
;  |kill           |               |               |
;  |---------------+---------------+---------------+

     (define-key CSI-map "1~" 'isearch-forward-regexp)     ;; Find
     (define-key CSI-map "2~" 'quoted-insert )             ;; Insert Here
     (define-key CSI-map "3~" 'kill-region)                ;; Remove
     (define-key CSI-map "4~" 'copy-region-as-kill)        ;; Select
     (define-key CSI-map "5~" 'flip)                       ;; Prev Screen
     (define-key CSI-map "6~" 'other-window)               ;; Next Screen

; upper row of fct keys
;
; F6-F10: LISP COMMANDS
;
;  -----------------------------------------------------------------
;  |F6         |F7            |F8             |F9          |F10    |
;  |run-lisp   |lisp-send-    |lisp-send-     |inferior-   |clean- |
;  |           |defun         |defun-and-go   |lisp-reset  |lisp   |
;  |-----------+--------------+---------------+------------+-------|
;

     (define-key CSI-map "17~" 'run-lisp)                  ;; F6
     (define-key CSI-map "18~" 'lisp-send-defun)           ;; F7
     (define-key CSI-map "19~" 'lisp-send-defun-and-go)    ;; F8
     (define-key CSI-map "20~" 'inferior-lisp-reset)       ;; F9
     (define-key CSI-map "21~" 'clean-lisp)                ;; F10
;
; F11-F14:
;
;  |--------+---------------+-----------+--------|   |-------+---------|
;  |F11     |F12            |F13        |F14     |   |Help   |Do       |
;  |ESCAPE  |repeat-complex |goto-line  |bigger  |   |help   |call-last|
;  |        |command        |           |        |   |       |kbd-macro|
;  |--------+---------------+-----------+--------|   |-------+---------|

     (define-key CSI-map "23~" 'ESC-prefix)                ;; F11 (ESC)
     (define-key CSI-map "24~" 'repeat-complex-command)    ;; F12
     (define-key CSI-map "25~" 'goto-line)                 ;; F13
     (define-key CSI-map "26~" 'bigger)                    ;; F14

     (define-key CSI-map "28~" 'help-command)              ;; Help
     (define-key CSI-map "29~" 'call-last-kbd-macro)       ;; Do
;
; F17-F20: Shell stuff
;
;  |----------+---------------+-----------+------------|
;  |F17       |F18            |F19        |F20         |
;  |Compile   |next-error     |shell      |send-region |
;  |          |               |           |to-shell    |
;  |----------+---------------+-----------+------------|

     (define-key CSI-map "31~" 'my-compile)                ;; F17
     (define-key CSI-map "32~" 'next-error)                ;; F18
     (define-key CSI-map "33~" 'shell)                     ;; F19
     (define-key CSI-map "34~" 'send-region-to-shell)      ;; F20

     (define-key global-map "\e[" CSI-map)


(send-string-to-terminal "\e=\e[62;1\"p")       ; turn on vt200 keypad mode

; enable XON/XOFF protocol: This requires re-mapping ^s/^q functions:
(set-input-mode nil t)

( define-key global-map "\023" 'undefined ) ; no annoying ^S
( define-key global-map "\021" 'undefined ) ; no annoying ^Q

( define-key ctl-x-map "\034" 'save-buffer )
( define-key ctl-x-map "\^z" 'save-buffer )
( define-key ctl-x-map "x" 'flip )
( define-key ctl-x-map "!" 'compile )
( define-key ctl-x-map "@" 'kill-compilation )


(define-key esc-map "s" 'cntr-line)

; remap isearch chars
(define-key global-map "\034" 'isearch-forward-regexp)  ; control-\
(setq search-repeat-char 28)                            ; control-\
(setq search-exit-char 13)                              ; carriage return

; tabs, c-mode indentation, etc.
(setq default-tab-width 8)                      ; 8-space tabs
(set-default 'indent-tabs-mode t)               ; represent tabs as tabs

(setq c-indent-level 8)                         ; 8-space ( 1 tab ) c-indent
(setq c-argdecl-indent -8)                      ; for fcn args
(setq c-brace-offset -8)                        ; line up braces under 'if'...
(setq c-label-offset -8)                        ; line up labels under 'if'...
(setq c-continued-statement-offset 8)           ; for simple if, etc
(setq c-tab-always-indent nil)                  ; for simple if, etc

(setq scroll-step 0)                            ; recenter if point leaves win

(put 'eval-expression 'disabled nil)            ; allow <esc><esc>

(defun insert-hard-tab ()
  "insert a REAL tab at point."
    (interactive)
    (progn (insert (quote "\t")))
)

(defun	set-c-hard-tabs ()
	"kludge in hard tabs which don't usually work"
	(interactive)
	(define-key c-mode-map "\t" 'insert-hard-tab)
)

( defvar     c-mode-hook 'set-c-hard-tabs )
( define-key c-mode-map "\t" 'insert-hard-tab )

( define-key c-mode-map "\^?" 'delete-backward-char )	; \177 octal

(autoload 'c++-mode "c++-mode" "c++-mode" t)
(autoload 'set-c-style "c-style.el" nil t)

; (autoload 'ispell-word "ispell" "Check the spelling of word in buffer." t)
; (global-set-key "\e$" 'ispell-word)


(define-key esc-map "\r" 'beginning-of-next-line)

(defun beginning-of-next-line ()
  "Move to the beginning of the next line."
  (interactive)
  (forward-line 1))

(defun open-previous-line ()
  "Open previous line for insertion"
  (interactive)
  (beginning-of-line)  (newline)  (backward-char)
)

(defun open-next-line ()
  "Open next line for insertion"
  (interactive)  
  (forward-line 1)  (newline)
  (if (not (eobp)) (backward-char))
)

(defun insert-me ()
  "insert date and user name at point."
    (interactive)
	(progn (insert (current-time-string)
		       "  " (user-full-name)
		       "  (" (user-login-name)
		       "@" (system-name) ")")))

(defun send-region-to-shell ()
  "send region to shell process. "
  (interactive)
  (send-region "*shell*" (point) (mark))
)

(defun cntr-line ()
  "Center the line point is on.
This means adjusting its indentation to match
the distance between the end of the text and fill-column."
  (interactive)
  (save-excursion
    (let (line-length)
      (beginning-of-line)
      (delete-horizontal-space)
      (end-of-line)
      (delete-horizontal-space)
      (setq line-length (current-column))
      (beginning-of-line)
      (indent-to 
	(+ left-margin 
	   (/ (- fill-column left-margin line-length) 2))))))


( defun flip ()
  ( interactive )
  ( switch-to-buffer ( other-buffer ) )
)



;
; more lisp stuff
;

;
; kcl stuff first
;
( defun kcl ()
  ( interactive )
  ( setq inferior-lisp-program "kcl" )
  ( setq
    inferior-lisp-load-command
    "(progn (load \"%s\" :verbose nil :print t) (values))\n"
  )
  ( setq lisp-mode-hook 'kcl-hook )
  ( eval
    '( defun inferior-lisp-reset ()
       "reset the current lisp process from an error condition"
       ( interactive )
       ( send-string "*lisp*" ":q\n" )
    )
  )
)

( defun kcl-hook ()
  ( setq mode-name "KCL" )
)



;
; now for franz
;

( defun franz ()
  ( interactive )
  ( setq inferior-lisp-program "lisp" )
  ( setq
    inferior-lisp-load-command
    "(let (val (old $ldprint)) (setq $ldprint nil) (setq val (load \"%s\")) (setq $ldprint old) val)\n"
  )
  ( setq lisp-mode-hook 'franz-hook )
  ( eval
    '( defun inferior-lisp-reset ()
       "reset the current lisp process from an error condition"
       ( interactive )
       ( send-string "*lisp*" "(reset)\n" )
    )
  )
)

( defun franz-hook ()
  ( setq mode-name "Franz" )
)

; default to franz
( franz )



( fset 'clean-lisp "OwOBOCOCODODOx" )
( fset 'bigger     "4^" )
( fset 'my-compile  "xcompile" )


( read-abbrev-file nil )

