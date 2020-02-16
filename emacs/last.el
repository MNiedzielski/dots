; GNU EMACS functions to repeat the last EMACS command
; 09/21/89, D. Spector

; --------------------------------------------------------------------
; Copyright (c) 1989 David Spector
; This software may be used, copied, and modified freely, except not
; as part of a product sold for profit, so long as these four copyright
; lines are retained without change.  There is no warranty of any kind.
; --------------------------------------------------------------------

(defun repeat (count)
  "Repeat last command with no query.
Repeats the command as many times as indicated by numeric argument COUNT.
If the last command had a numeric argument, it is ignored; if it had an
argument list (usually happens only for functions that are not invoked by
interactive keypath), the list is omitted; and if it was a self-inserted
character, the invoking keybinding is inserted instead."
  (interactive "p")
  (let (str pos)
    (if (eq last-command 'repeat)
        ()
      (setq pos (- (- (length (recent-keys)) 2)
                   (length (this-command-keys))))
      (setq str (substring (recent-keys) pos (+ pos 2)))
      (setq repeat-complex-flag nil)
      (setq repeat-kill-flag nil)
      (cond
       ((or (equal str "e") (equal str "E"))
        (setq repeat-command 'call-last-kbd-macro))
       ((or (equal str "d") (equal str "D"))
        (setq repeat-command 'kill-word)
        (setq repeat-kill-flag t))
       ((or (equal str "") (equal str ""))
        (setq repeat-command 'backward-kill-word)
        (setq repeat-kill-flag t))
       ((or (eq last-command 'exit-minibuffer) (eq last-command 'kill-region))
        (setq repeat-complex-flag t)
        (setq repeat-command (car command-history)))
       (t
        (setq repeat-command last-command))))
    ;(princ repeat-command)
    (while (> count 0)
      (if repeat-kill-flag
          (append-next-kill))
      (if repeat-complex-flag
          (eval repeat-command)
        (command-execute repeat-command))
      (setq this-command 'repeat)
      (setq count (1- count)))
    (setq last-command 'repeat)))
(global-set-key "\^\\" 'repeat)

(defun again ()
  "Repeat last complex command with no query."
  (interactive)
  (eval (prin1 (car command-history))))
(global-set-key "\^X\^\\" 'again)
