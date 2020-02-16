;
; Wed 3 November 1993  Mark Niedzielski (min@napa)
;

; You can get key information using ctrl-h k or a

;
; Right side `R' keys
;
(define-key global-map [f21]	'compile)		; Pause key
(define-key global-map [f22]	'kill-compilation)	; PrSc key
(define-key global-map [f23]	'next-error)		; Scroll Lock key

(define-key global-map [f24]	'backward-word)		; - key
(define-key global-map [f25]	'kill-word)		; / key
(define-key global-map [f26]	'forward-word)		; * key

(define-key global-map [f27]	'beginning-of-buffer)	; Home key
(define-key global-map [f28]	'previous-line)		; up arrow
(define-key global-map [f29]	'scroll-down)		; PgUp key

(define-key global-map [f30]	'forward-char)		; right arrow
(define-key global-map [f31]	'recenter)		; centers page
(define-key global-map [f32]	'backward-char)		; left arrow

(define-key global-map [f33]	'end-of-buffer)		; End key
(define-key global-map [f34]	'next-line)		; down arrow
(define-key global-map [f35]	'scroll-up)		; PgDn key

;
; Left side `L' keys
;
(define-key global-map [f11]	'keyboard-quit)		; L1 - Stop key
(define-key global-map [f12]	'electric-command-history)	; L2 - Again key
(define-key global-map [f13]	'electric-buffer-list)	; L3 - Props key
(define-key global-map [f14]	'undo)			; L4 - Undo key
(define-key global-map [f16]	'copy-region-as-kill)	; L6 - Copy key
;(define-key global-map [C-f16]	'open-rectangle)	; L6 - Copy key
(define-key global-map [f18]	'yank)			; L8 - Paste key
;(define-key global-map [C-f18]	'yank-rectangle)	; L8 - Paste key
(define-key global-map [f19]	'query-replace)		; L9 - find key
;(define-key global-map [C-f19]	'isearch-forward-regexp)	; L9 - find key
(define-key global-map [f20]	'kill-region)		; L10- Cut key
;(define-key global-map [C-f20]	'kill-rectangle)	; L10- Cut key

;
; Top `F' keys
;
(define-key global-map [help]	'kill-region)		; Help key
;(define-key global-map [C-@]	'set-mark-command)	; blank key next to Help key

(define-key global-map [f1]	'scroll-down-in-place)	; F1 key
(define-key global-map [f2]	'scroll-up-in-place)	; F2 key
(define-key global-map [f3]	'scroll-other-down)	; F3 key
(define-key global-map [f4]	'scroll-other-up)     	; F4 key
(define-key global-map [f5]	'eval-current-buffer)	; F5 key
(define-key global-map [f6]	'shrink-window)		; F6 key
(define-key global-map [f7]	'enlarge-window)	; F7 key
(define-key global-map [f8]	'manual-entry)		; F8 key
(define-key global-map [f9]	'info)			; F9 key
(define-key global-map [f10]	'describe-key)		; F10 key
;(define-key global-map [C-p]	'describe-variable)	; F11 key
;(define-key global-map [C-q]	'describe-function)	; F12 key
