; emacs script by David Oh (May 1990)
; based on script by John Carr on project Athena at MIT (Feb, 1989)
; on Sun Workstations (sparcs 1) the function and special keys emit "ESC [ string"
; the various keys map to different strings. For instance:

; F1                224z
; F2                225z   
; up arrow          A
; down arrow        B
; etc....

; First, define an empty keymap to hold the bindings.
(defvar fnkey-map (make-sparse-keymap) "Keymap for Function Keys" )

;Second, bind it to ESC- [ (which is the prefix used on the function keys
(define-key esc-map "[" fnkey-map)

; Third, bind functions to the various keys.  Note that you must use
; internal lisp function names, which are usually but not always the
; names used with meta-x.
; You can get them using ctrl-h k or a

;
; Right side `R' keys
;
(define-key fnkey-map "208z" 'compile)			; Pause key
(define-key fnkey-map "209z" 'kill-compilation)		; PrSc key
(define-key fnkey-map "210z" 'next-error)		; Scroll Lock key

(define-key fnkey-map "211z" 'backward-word)		; = key
(define-key fnkey-map "212z" 'kill-word)		; / key
(define-key fnkey-map "213z" 'forward-word)		; * key

(define-key fnkey-map "214z" 'beginning-of-buffer)	; Home key
(define-key fnkey-map "A" 'previous-line)		; up arrow
(define-key fnkey-map "216z" 'scroll-down)		; PgUp key

(define-key fnkey-map "C" 'forward-char)		; right arrow
(define-key fnkey-map "218z" 'recenter)                 ; centers page
(define-key fnkey-map "D" 'backward-char)		; left arrow

(define-key fnkey-map "220z" 'end-of-buffer)		; End key
(define-key fnkey-map "B" 'next-line)			; down arrow
(define-key fnkey-map "222z" 'scroll-up)		; PgDn key

;
; Left side `L' keys
;
(define-key fnkey-map "193z" 'electric-command-history)	; Again key
(define-key fnkey-map "194z" 'electric-buffer-list)	; Props key
(define-key fnkey-map "195z" 'undo)			; Undo key
(define-key fnkey-map "197z" 'copy-region-as-kill)	; Copy key
;(define-key fnkey-map "197z" 'open-rectangle)		; Copy key
(define-key fnkey-map "199z" 'yank)			; Paste key
;(define-key fnkey-map "199z" 'yank-rectangle)		; Paste key
(define-key fnkey-map "200z" 'query-replace)		; find key
;(define-key fnkey-map "200z" 'isearch-forward-regexp)	; find key
(define-key fnkey-map "201z" 'kill-region)		; Cut key
;(define-key fnkey-map "201z" 'kill-rectangle)		; Cut key
(define-key fnkey-map "202z" 'command-apropos)		; Help key

;
; Top `F' keys
;
(define-key fnkey-map "224z" 'scroll-down-in-place)	; F1 key
(define-key fnkey-map "225z" 'scroll-up-in-place)	; F2 key
(define-key fnkey-map "226z" 'scroll-other-down)        ; F3 key
(define-key fnkey-map "227z" 'scroll-other-up)     	; F4 key
(define-key fnkey-map "228z" 'eval-current-buffer)	; F5 key
(define-key fnkey-map "229z" 'shrink-window)		; F6 key
(define-key fnkey-map "230z" 'enlarge-window)		; F7 key
(define-key fnkey-map "231z" 'manual-entry)		; F8 key
(define-key fnkey-map "232z" 'info)			; F9 key
(define-key fnkey-map "233z" 'describe-key)		; F10 key
(define-key fnkey-map "234z" 'describe-variable)	; F11 key
(define-key fnkey-map "235z" 'describe-function)	; F12 key


