
;
;   set up the arrows in the minibuffer to access the history elements
;

(define-key minibuffer-local-map [up] 'previous-history-element)
(define-key minibuffer-local-map [down] 'next-history-element)
(define-key minibuffer-local-ns-map [up] 'previous-history-element)
(define-key minibuffer-local-ns-map [down] 'next-history-element)
(define-key minibuffer-local-completion-map [up] 'previous-history-element)
(define-key minibuffer-local-completion-map [down] 'next-history-element)
(define-key minibuffer-local-must-match-map [up] 'previous-history-element)
(define-key minibuffer-local-must-match-map [down] 'next-history-element)

