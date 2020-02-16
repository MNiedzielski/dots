
( defun fix-x-crud ()
  ( interactive )
  ;;;
  ;;; yank/kill rectangle via the mouse
  ;;;
  ( define-key mouse-map x-button-c-s-middle 'x-kill-rect ) ; control+shift+middle
  ( define-key mouse-map x-button-c-s-right  'x-yank-rect ) ; control+shift+right


  ;;;
  ;;; Anything else you want done when the window system is brought up.
  ;;; (Which is way downstream of your .emacs file.)
  ;;;


)

;;; Patch it in.
( setq window-setup-hook 'fix-x-crud )



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


