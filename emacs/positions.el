;; File: positions.el
;; enhanced registers for GNU Emacs
;; Copyright (C) 1988 Free Software Foundation, Inc., and Bard Bloom

;; This file is intended by the author to be part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; STACKS has three and a half related parts.  The simplest is a collection
;; of commands which save the window configuration and return to it.  For
;; example, if you're editing five files in eight windows, and a letter
;; arrives, you can read it in one window and then return to the eight
;; windows.
;; 
;; All the commands live under c-x c-j
;;
;; The commands for this part are:
;;
;; c-x c-j c-r: Return to the previous window configuration.
;;              (Pops the stack of saved configurations.)
;; --- the rest of these commands do their usual function in a single
;;     window; c-x c-j c-r returns to the window config before they
;;     were executed.  There is a stack of size (default) 16. ---
;; c-x c-j c-f: find file 
;; c-x c-j c-v: view file
;; c-x c-j c-m: read mail
;; c-x c-j m:   send mail
;; c-x c-j c-b: switch to buffer
;; c-x c-j 4 f: find file other window
;; c-x c-j 4 v: view file other window
;; c-x c-j 4 m: mail other window
;; c-x c-j 4 b: switch to buffer other window
;; c-x c-j c-@: push the the current window config and point, so that you
;;              can run off and do something else and return easily.

;; MOVEMENT UNDO 
;; c-x c-j c-u undoes the last of certain movement and window-splitting
;; commands.  For example, c-x 0, c-x 1, and c-x 2 have been hacked
;; so that they can be undone.  
;; There is a queue of the last 24 (default) positions and window 
;; configurations.  c-x c-j c-u, pressed repeatedly, moves
;; down this queue; it can take an arguemnt to go to a particular
;; element of it.
;;
;; If you don't like to have me reassign the behavior of these standard 
;; commands, set the variable movement-undo-inhibit to true BEFORE
;; loading this package.
;;
;; It might be useful for powerful movement commands to save either 
;; a position or a window configuration.  For example, end-of-buffer
;; pushes a mark, because otherwise it would be easy to go to the end
;; of the buffer by mistake and hard to find your way back.  
;; Searches also go long distances, but it's much harder to find
;; your way back.  The mark is used for so much that it's not wise
;; to set it at the beginning of each long movement.  These movement-undo
;; commands are intended purely for getting back to recently important
;; places; they don't have all the other uses of the mark, and so 
;; there's no great obstacle to using them liberally in places like
;; (e.g.) the beginning of searches.
;;   (stack-save-current-wc) saves the window configuration
;;   (stack-save-current-pos) saves the current position.
;;   (stack-save-current-wc-pos) saves both


;; EXTENDED REGISTER-LIKE THINGS:
;; Finally, stacks are a replacement for registers.  I keep getting 
;; the keys for point-to-register and register-to-point confused, 
;; and carefully saving a position and then losing it when I want to 
;; go to it.  That's the original motivation: so that point-to-register
;; wouldn't destroy the old value.  Stacks can hold a sequence of 
;; critters, where a critter can be:
;;   - a position
;;   - a window configuration
;;   - a string 
;;   - a rectangle
;;
;; The advantages over registers are:
;;  - They can store window configurations
;;  - It is harder to lose things you put in them.
;;
;; These stack commands all take a prefix argument telling which element
;; of the stack to use.  All default to the top of the stack.
;; 
;; REGISTER-LIKE COMMANDS
;;   c-x c-j c-p -- Stores point in a stack
;;   c-x c-j c-w -- Stores the window configuration and position in a stack.
;;                  Use this when you're thinking "Save as much as possible."
;;   c-x c-j c-q -- Stores only the window config in a stack.
;;                  (That is, point in the current window may be different
;;                  when you return to that config.)
;;                  This is of limited but occasional use.
;;   c-x c-j c-j -- Goes to the position or window config in a stack.
;;
;;   c-x c-j i   -- Insert register or rectangle from stack
;;   c-x c-j c   -- copy region to stack
;;   c-x c-j a   -- append region to stack
;;   c-x c-j r   -- copy rectangle to stack
;;
;;   c-x c-j =   -- Show contents of a stack
;;   c-x c-j +   -- Show all nonempty stacks
;;   c-x c-j c-] -- remove some entries from the top of a stack.
;;
;; There are some special-purpose stacks.
;;   Stack DEL is the dump, used for storing movement-undo stuff
;;         on.  
;;   Stack W is used for the saved window configurations 
;;         for all the commands which save them and do something else.

;; Unrelated commands: find-file-check and friends
;;   These behave like find-file and its relatives, except that
;;   if the file doesn't exist, they ask you if you want to create
;;   it.  Good for those of us careless about assuming that file completion
;;   works.  The variable find-file-check-inhibit, if true, turns off this
;;   checking and makes it behave just like find-file.

(provide 'positions)

; Stacks are represented as lists of:
;  markers
;  ('window-configuration . window-configuration)
;     -- since I can't find a window-configuration-p function
;  ('wc-pos window-configuration marker)
;     -- since point is not saved in a window configuration
;  strings 
;  rectangles (lists of string)


(require 'cl)

(defvar stack-vector-size 128)

(defvar movement-undo-inhibit nil
  "If true, don't reassign the standard keys that the positions
package usually reassigns.")

(defvar find-file-check-inhibit nil
  "*If true, find-file-check will NOT ask if it should create files.")

(defvar window-stack-name ?W "Stack-Name to use for pushing window configs on
to save them.")

(defvar window-stack-limit 16)

(defvar stack-dump-name ?\C-? "*Stack to stick popped stuff on.")

(defvar stack-dump-limit 24 "*Number of previous stack contents to remember.")

(defvar stack-vector (make-vector stack-vector-size nil))

(defun get-stack (stack-name)
  (aref stack-vector stack-name))

(defun set-stack (stack-name v)
  (aset stack-vector stack-name v))

(defun stack-el (stack-name &optional number)
  "From the stack named STACK-NAME, take the NUMBERth element.
NUMBER defaults to 1 = the top of the stack.  Returns nil
if the stack is empty."
  (cond
   (number (nth (1- number) (get-stack stack-name)))
   (t (car (get-stack stack-name)))))

(defun stack-nonempty-p (stack-name)
  "Returns true if the stack named STACK-NAME is nonempty."
  (get-stack stack-name))

(defun stack-length (stack-name)
  "Returns the number of things in the stack named STACK-NAME."
  (length (get-stack stack-name)))

(defun put-on-stack (thing stack-name &optional limit)
  "Puts THING on stack named STACK-NAME.  If LIMIT is present, 
truncate the stack to LIMIT elements."
  (let ((new-stuff (cons thing (get-stack stack-name))))
    (set-stack
     stack-name
     (if limit (first-n limit new-stuff) new-stuff))))

(defun first-n (n l)
  (cond
   ((<= n 0) nil)
   ((null l) nil)
   (t (cons (car l) (first-n (1- n) (cdr l))))))

(defun stackable-window-configuration ()
  (cons 'window-configuration (current-window-configuration)))

(defun stackable-wc-pos ()
  (list 'wc-pos
        (current-window-configuration)
        (point-marker)))

(defun goto-stack (stackname position)
  "From STACKNAME pick the POSITIONth element and switch to it 
as appropriate for what it is."
  (interactive "cGo to stack: \np")
  (let ((stack (get-stack stackname)))
    (cond
     ((null stack)
      (error "That stack is very very empty."))
     ((or (< position 1) (> position (length stack)))
      (error "Stack `%c' has %d stored position(s); can't go to %d."
             stackname (length stack)
             position))
     (t
      (let*
          ((the-mark (nth (1- position) stack))
           the-buffer this-here-window)
        (cond
         ((and (consp the-mark)
               (eq (car the-mark) 'window-configuration))
          (unless (= stackname stack-dump-name)
            (stack-save-current-wc))
          (set-window-configuration (cdr the-mark)))
         ((and (consp the-mark)
               (eq (car the-mark) 'wc-pos))
          (unless (= stackname stack-dump-name)
            (stack-save-current-wc-pos))
          (set-window-configuration (second the-mark))
          (goto-char (third the-mark)))
         ((progn
            ;; we need to execute some code.  This cond-clause always
            ;; fails and falls through.
            (setq the-buffer (marker-buffer the-mark))
            (setq this-here-window (selected-window))
            nil))
         ((or (null the-mark) (null the-buffer)
              (null (buffer-name the-buffer)))
          (error "That buffer (or position in it) has been destroyed."))
         (t
          (unless (= stackname stack-dump-name)
            (stack-save-current-pos))
          (switch-to-buffer the-buffer)
          (goto-char the-mark)
          )))))))


(defun stack-save-current-wc ()
  "Save the current window configuration on the dump stack,
in case the user typed something wrong."
  (put-on-stack (stackable-window-configuration) stack-dump-name
                stack-dump-limit))

(defun stack-save-current-wc-pos ()
  "Save the current window configuration adn position on the dump stack,
in case the user typed something wrong."
  (put-on-stack (stackable-wc-pos) stack-dump-name
                stack-dump-limit))

(defun stack-save-current-pos ()
  "Save the current position on the dump stack
in case the user typed something wrong."
  (put-on-stack (point-marker) stack-dump-name stack-dump-limit))

(defun point-to-stack (stack-name)
  "Shoves the current position on the stack called STACK-NAME."
  (interactive "cPut point on stack: \n")
  (put-on-stack (point-marker) stack-name))

(defun window-config-to-stack (sn)
  "Pushes the current window configuration onto stack named SN."
  (interactive "cPush window configuration on stack: \n")
  (put-on-stack (stackable-window-configuration) sn))

(defun wc-pos-to-stack (sn)
  "Pushes the current window configuration and position onto stack named SN."
  (interactive "cPush window configuration and position on stack: \n")
  (put-on-stack (stackable-wc-pos) sn))

(defun show-stack (stackname)
  (interactive "cWhich stack ya wanna see: ")
  (cond
   ((get-stack stackname)
    (with-output-to-temp-buffer (format "*Stuff in Stack*")
      (princ (format "Stuff in stack %s:\n"
                     (key-description (list stackname))))
      (do*
          ( (i 1 (1+ i))
            (stack (get-stack stackname) (cdr stack))
            (p (car stack) (car stack)))
          ((null stack))
        (princ
         (format "%d -- %s\n" i (prin1-to-string p))))
      (princ "And that's it.")))
   (t
    (message "Look, stack %c is empty." stackname))))

(defun pop-stack (stackname number)
  "Pops from stack named STACKNAME the first NUMBER elements.
It puts them on the dump. Does NOT go anywhere"
  (interactive "cPop stack: \nnHow many elements to throw away: ")
  (let ((s (get-stack stackname)))
    (while (and s (> number 0))
      (unless (= stackname stack-dump-name)
        (put-on-stack (car s) stack-dump-name (max number stack-dump-limit)))
      (setq number (1- number)
            s (cdr s)))
    (set-stack stackname s)))


(defun show-nonempty-stacks ()
  "Lists the nonempty stacks in a compact format in a buffer somewhere"
  (interactive)
  (with-output-to-temp-buffer "*Nonempty Stacks*"
    (do*
        ((i 0 (1+ i)))
        ((= i stack-vector-size))
      (when (get-stack i)
        (princ
         (format "%s:  %s"
                 (key-description (list i))
                 (prin1-to-string (get-stack i))))
        (terpri)))))


;; The next set of functions are like some common ones, but they
;; save the window configuration as well...

(defun window-conf-call-interactively (fun)
  "Save the current window configuration, so that resume-window-configuration 
restores it.  Call FUN interactively in one window."
  (put-on-stack (stackable-wc-pos)
                window-stack-name window-stack-limit)
  (delete-other-windows)
  (stack-hack-mode-line)
  (call-interactively fun))

(defun pop-wc ()
  "Returns to the window state on top of the window-stack-name stack,
as stored by several commands like rmail-wc.  Pops that stack, but
saves the current and the popped window configuration on the dump."
  (interactive)
  (stack-save-current-wc-pos)
  (goto-stack window-stack-name 1)
  (pop-stack window-stack-name 1)
  (stack-hack-mode-line))

(defvar mode-line-stack-depth ""
  "Reminder of how many things remain to be returned to via the pop-wc 
commadn.")

(defun stack-hack-mode-line ()
  "Changes a variable suitable for display in the mode line,
measuring depth of the stack of things you suspended and have
waiting to pop back to."
  (let ((sl (stack-length window-stack-name)))
    (setq  mode-line-stack-depth
           (cond
            ((= sl 0) "")
            (t (format "{%d}" sl))))))
  

(defun switch-to-buffer-wc ()
  "Push the current window configuration on the stack 
window-stack-name, and switch to a new buffer in one window."
  (interactive)
  (window-conf-call-interactively
   (function switch-to-buffer)))

(defun switch-to-buffer-dump (buf)
  "Switch to BUF, saving the current position on the dump.
Also it prompts if it looks as if it should create the buffer."
  (interactive
   (let*
       ;; switch-to-buffer seems to do a pretty good job of picking
       ;; buffers to go to, and I can't figure out how, so I'll use it...
       ((tb (save-excursion
              (switch-to-buffer nil)
              (current-buffer)))
        (b (read-buffer
            (format "Switch to buffer: " (buffer-name tb))
            tb)))
     (cond
      ((get-buffer b)
       (list (get-buffer b)))
      ((y-or-n-p
        (format "%s does not exist -- create it?" b))
       (list (get-buffer-create b)))
      (t
       (error "Buffer not created")))))
  (stack-save-current-pos)
  (switch-to-buffer buf))
       


(defun switch-to-buffer-other-window-wc ()
  "Push the current window configuration on the stack 
window-stack-name, and switch to a new buffer in one window."
  (interactive)
  (window-conf-call-interactively
   (function switch-to-buffer-other-window)))

(defun view-file-wc ()
  "Save window configuration and view a file. Pop-window-configuration
returns to the old window configuration."
  (interactive)
  (window-conf-call-interactively (function view-file)))

(defun view-file-other-window-wc ()
  "Save window configuration and view a file in another window. 
Pop-window-configuration returns to the old window configuration."
  (interactive)
  (window-conf-call-interactively (function view-file-other-window)))

(defun find-file-other-window-wc ()
  "Save window configuration and finds a file. Pop-window-configuration
returns to the old window configuration."
  (interactive)
  (window-conf-call-interactively (function find-file-other-window)))

(defun find-tag-other-window-wc ()
  (interactive)
  (window-conf-call-interactively (function find-tag-other-window)))


(defun find-file-check ()
  "Like find-file, but asks if it should create files."
  (interactive)
  (stack-save-current-wc-pos)
  (switch-to-buffer (find-file-noselect-check)))

(defun find-file-other-window-check ()
  (interactive)
  (stack-save-current-wc-pos)
  (switch-to-buffer-other-window (find-file-noselect-check)))

(defun find-file-noselect-check ()
  "Finds a file.  If it doesn't exist, it asks you before creating it."
  (interactive)
  (let ((dir default-directory)
        (fn nil)
        (ok nil))
    (while (not ok)
      (setq fn (expand-file-name
                (read-file-name "Find file: "
                                dir))
            ok (or (file-exists-p fn)
                   (find-accept-file fn))
            dir (file-name-directory fn)))
    (find-file-noselect fn)))

(defun find-accept-file (fn)
  "Asks whether or not you want to create the (non-existant)
file called FN.  Allows quitting."
  (cond
   (find-file-check-inhibit
    t)
   (t
    (message "%s does not exist. Create it? [ynq]: " fn)
    (let (c
          (done nil)
          (answer nil)
          )
      (while (not done)
        (setq c (read-char))
        (cond
         ((memq c '(?y ?Y 32 ?\C-m ?c ?C))
          (setq done t
                answer t))
         ((memq c '(?n ?N ?\C-?))
          (setq done t
                answer nil))
         ((memq c '(?q ?Q ?\C-g))
        (error "File not created."))))
      answer))))

              

(defun find-file-wc ()
  "Save window configuration and finds a file. Pop-window-configuration
returns to the old window configuration."
  (interactive)
  (window-conf-call-interactively (function find-file-check)))

(defun mail-other-window-wc ()
  "Send mail in another window.  Pop-window-configuration returns to the 
old window configuration."
  (interactive)
  (window-conf-call-interactively (function mail-other-window)))

(defun mail-wc ()
  "Send mail in one window.  Pop-window-configuration returns to the 
old window configuration."
  (interactive)
  (window-conf-call-interactively (function mail)))

(defun rmail-wc ()
  "Read mail in one window.  Pop-window-configuration returns to the 
old window configuration."
  (interactive)
  (window-conf-call-interactively (function rmail)))

(defun push-wc ()
  (interactive)
  (put-on-stack (stackable-wc-pos)
                window-stack-name window-stack-limit)
  (stack-hack-mode-line)
  (message "Window configuration pushed"))

(defvar window-undo-counter 1)

(defun window-undo (arg)
  "Return to the ARGth last position saved on the dump stack.
If pressed repeatedly with no argument, it will walk through
the dump stack."
  (interactive "P")
  (cond
   ((and (null arg) (eq last-command 'window-undo))
    (setq arg (+ window-undo-counter 1)
          window-undo-counter arg))
   (t
    (setq window-undo-counter (prefix-numeric-value arg))))
  (when (> window-undo-counter 1)
    (message "Undoing to position %d." window-undo-counter))
  (goto-stack stack-dump-name window-undo-counter))

(defun split-window-vertically-dump (&optional arg)
  "Split current window into two windows, one above the other.
This window becomes the uppermost of the two, and gets
ARG lines.  No arg means split equally.  Saves the current 
window config on the dump stack."
  (interactive "P")
  (stack-save-current-wc-pos)
  (split-window-vertically arg))

(defun delete-other-windows-dump ()
  "Make the selected window fill the screen. Saves the current 
window config on the dump stack."
  (interactive)
  (stack-save-current-wc-pos)
  (delete-other-windows))

(defun delete-window-dump ()
  "Remove the current window from the display. Saves the current 
window config on the dump stack."
  (interactive)
  (stack-save-current-wc-pos)
  (delete-window))

(defun split-window-excursion-wc ()
  "Splits the window and saves the old window configuration on
the window-configuration stack."
  (interactive)
  (window-conf-call-interactively (function split-window-vertically)))  
  

(defun end-of-line-dump ()
  "Move point to end of current line.  Saves position on dump.
With argument ARG not nil or 1, move forward ARG - 1 lines first.
If scan reaches end of buffer, stop there without error."
  (interactive)
  (stack-save-current-pos)
  (call-interactively (function end-of-line)))

(defun beginning-of-line-dump ()
  "Move point to beginning of current line.  Saves position on dump.
With argument ARG not nil or 1, move backward ARG - 1 lines first.
If scan reaches end of buffer, stop there without error."
  (interactive)
  (stack-save-current-pos)
  (call-interactively (function beginning-of-line)))

  
;; text manipulation

(defvar insert-stack-puts-point-before nil
  "*True makes insert-stack put point before the insertion.")

(defun insert-stack (char &optional num)
  "Insert contents of stack named SN;  SN is a character.  
Inserts the NUMth element, default 1.
Puts mark before and point after the inserted text, unless
the variable insert-stack-puts-point-before is set, in which
case it does it the other way around."
  (interactive "cInsert stack: \np")
  (push-mark)
  (let ((val (stack-el char num)))
    (cond
     ((and (consp val)
           (not (eq 'window-configuration (car val))))
	(insert-rectangle val))
     ((stringp val)
      (insert val))
     ((or (integerp val) (markerp val))
        (princ (+ 0 val) (current-buffer)))
     (t
      (error "Stack contains %s" (2str val)))))
  (when insert-stack-puts-point-before
    (exchange-point-and-mark)))

(defun copy-to-stack (char start end &optional delete-flag)
  "Copy region into stack SN.
With prefix arg, delete as well.
Called from program, takes four args:
SN, START, END and DELETE-FLAG.
START and END are buffer positions indicating what to copy."
  (interactive "cCopy to stack: \nr\nP")
  (put-on-stack (buffer-substring start end) char)
  (if delete-flag (delete-region start end)))

(defun append-to-stack (char start end &optional delete-flag)
  "Append region to text in stack SN.
With prefix arg, delete as well.
Called from program, takes four args:
SN, START, END and DELETE-FLAG.
START and END are buffer positions indicating what to append."
  (interactive "cAppend to stack: \nr\nP")
  (let* ((st (get-stack char))
        (s (car st)))
    (unless (stringp s)
      (error "Stack %s does not contain text." (key-description (list char))))
    (rplaca st (concat s (buffer-substring start end)))
    (if delete-flag (delete-region start end))))

(defun prepend-to-stack (char start end &optional delete-flag)
  "Prepend region to text in stack SN.
With prefix arg, delete as well.
Called from program, takes four args:
SN, START, END and DELETE-FLAG.
START and END are buffer positions indicating what to append."
  (interactive "cAppend to stack: \nr\nP")
  (let* ((st (get-stack char))
        (s (car st)))
    (unless (stringp s)
      (error "Stack %s does not contain text." (key-description (list char))))
    (rplaca st (concat (buffer-substring start end) s))
    (if delete-flag (delete-region start end))))

(defun copy-rectangle-to-stack (char start end &optional delete-flag)
  "Copy rectangular region into stack SN.
With prefix arg, delete as well.
Called from program, takes four args:
SN, START, END and DELETE-FLAG.
START and END are buffer positions giving two corners of rectangle."
  (interactive "cCopy rectangle to stack: \nr\nP")
  (put-on-stack (if delete-flag
		    (delete-extract-rectangle start end)
		  (extract-rectangle start end))
		char))


; Suggested Key Bindings:

(defvar stack-keymap nil
  "Keymap for stack commmands.")

(progn
  (setq stack-keymap (make-sparse-keymap))
  (define-key stack-keymap "\C-p" 'point-to-stack)
  (define-key stack-keymap "\C-q" 'window-config-to-stack)
  (define-key stack-keymap "\C-w" 'wc-pos-to-stack)
  (define-key stack-keymap "\C-j" 'goto-stack)

  (define-key stack-keymap "i" 'insert-stack)
  (define-key stack-keymap "\C-i" 'insert-stack)
  (define-key stack-keymap "c" 'copy-to-stack)
  (define-key stack-keymap "r" 'copy-rectangle-to-stack)
  (define-key stack-keymap "a" 'append-to-stack)
  (define-key stack-keymap "=" 'show-stack)
  (define-key stack-keymap "\C-]" 'pop-stack)
  (define-key stack-keymap "+" 'show-nonempty-stacks)


  (define-key stack-keymap "\C-u" 'window-undo)

  (define-key stack-keymap "\C-r" 'pop-wc)
  (define-key stack-keymap "\C-@" 'push-wc)
  (define-key stack-keymap "\C-b" 'switch-to-buffer-wc)
  (define-key stack-keymap "\C-f" 'find-file-wc)
  (define-key stack-keymap "\C-v" 'view-file-wc)
  (define-key stack-keymap "4." 'find-tag-other-window-wc)
  (define-key stack-keymap "4\C-f" 'find-file-other-window-wc)
  (define-key stack-keymap "4f" 'find-file-other-window-wc)
  (define-key stack-keymap "4\C-v" 'view-file-other-window-wc)
  (define-key stack-keymap "4v" 'view-file-other-window-wc)
  (define-key stack-keymap "4m" 'mail-other-window-wc)
  (define-key stack-keymap "4\C-b" 'switch-to-buffer-other-window-wc)
  (define-key stack-keymap "4b" 'switch-to-buffer-other-window-wc)
  (define-key stack-keymap "2" 'split-window-excursion-wc)
  (define-key stack-keymap "\C-m" 'rmail-wc)
  (define-key stack-keymap "m" 'mail-wc)

  (global-set-key "\C-x\C-j" stack-keymap)

  (unless movement-undo-inhibit
    
    (global-set-key "\C-x2" 'split-window-vertically-dump)
    (global-set-key "\C-x1" 'delete-other-windows-dump)
    (global-set-key "\C-x0" 'delete-window-dump)
    (global-set-key "\C-e" 'end-of-line-dump)
    (global-set-key "\C-a" 'beginning-of-line-dump)
    (global-set-key "\C-xb" 'switch-to-buffer-dump)
    (substitute-key-definition 'find-file 'find-file-check ctl-x-map)
    (substitute-key-definition 'find-file-other-window
                               'find-file-other-window-check
                               ctl-x-4-map) 
    )
)
