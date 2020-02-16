;;
;; Better mouse buttons
;;

( load "multi-click"		nil t )
( load "new-mouse-buffer-menu"	nil t )


(defun mode-line-resize-dynamically ()
  "Resize a window by dragging the mode-line.
This must be bound to a mouse-down event in the mode-line."
  (interactive "@")
  (let* ((mouse (mouse-position))
         (start-frame (car mouse))
         (prev-y (cdr (cdr mouse)))
         (next (next-window)))
    (track-mouse
      (while (and (eq (car-safe (read-event)) 'mouse-movement)
                  (eq next (next-window)))
        (let* ((mouse (mouse-position))
               (frame (car mouse))
               (new-y (cdr (cdr mouse)))
               (delta (- new-y prev-y)))
          (cond ((and (eq frame start-frame)
                      (> (+ delta (window-height (selected-window)))
                         window-min-height))
                 (enlarge-window delta)
                 (setq prev-y new-y))))))))



;
; Cool rectangle stuff...
;
( defun x-kill-rect (event)
  "Like kill-rectangle, but mouse based."
  (interactive "@e")
  (let ((point-save (point)))
    (save-excursion
      (mouse-set-point event)
      (push-mark nil t)
      (if (> point-save (point))
	  (kill-rectangle (point) point-save)
	(kill-rectangle point-save (point))))))

( defun x-yank-rect ()
  "Paste that rectangle back somewhere."
  (interactive)
  (save-excursion
    (yank-rectangle)
  )
)


;
; Do the bindings...
;

; From multi-click
(global-set-key [mouse-1]                 'multi-click-set-point)
(global-set-key [down-mouse-1]            'multi-click-drag-region)
(global-set-key [double-mouse-1]          'multi-click-set-word)
(global-set-key [triple-mouse-1]          'multi-click-set-line)
(global-set-key [mouse-3]                 'multi-click-save-then-kill)
(global-set-key [drag-mouse-1]            'multi-click-set-region-store)

; From mouse-bits
( global-set-key [mode-line down-mouse-2]  'mode-line-resize-dynamically )
( global-set-key [C-S-mouse-2] 'x-kill-rect )
( global-set-key [C-S-mouse-3] 'x-yank-rect )

; From new-mouse-buffer-menu
( global-set-key [C-down-mouse-1] 'new-mouse-buffer-menu )
