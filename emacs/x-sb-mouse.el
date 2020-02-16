;; LCD Archive Entry:
;; x-sb-mouse|Sullivan Beck|beck@qtp.ufl.edu|
;; Better mouse support for X.  Allows vertical/horizontal resizing.|
;; 92-06-17|$Revision: 1.5 $|~/misc/x-sb-mouse.el.Z|

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
;;--------------------------------------------------------------------

;;; This is a new and hopefully better mouse package for emacs running
;;; under X.  Some of its features are:
;;;  1) It combines the features of x-mouse-drag and alt-mouse.
;;;  2) It allows you to use control, shift, and meta modifiers with the
;;;     mouse (in any combination).
;;;  3) It distinguishes between click and drag events.  A drag is where
;;;     the mouse button is moved with a button held down).
;;;  4) It recognizes 5 different parts of the window:
;;;       window : the window itself
;;;       mode   : the mode line of a window
;;;       border : the vertical border separating two windows split side
;;;                by side
;;;       inter  : the intersection of a mode line and the vertical border
;;;       mini   : the minibuffer
;;;     This allows a great deal of extra functionality not available in
;;;     the other two mouse packages such as both horizontal AND vertical
;;;     resizing of the windows.  You can even resize the minibuffer (as
;;;     long as you give it at least 1 line).
;;;  5) 120 click events and 24 drag events can be bound simply to emacs
;;;     functions by simply setting the appropriate variable (see
;;;     installation instructions below).
;;;  6) It is fully x-mouse compatible.  All packages that run on top
;;;     of x-mouse should also run on top of this one.

;;; Although I have rewritten almost everything, segments of this package
;;; have been borrowed from both of the packages mentioned above.  Thanks
;;; to the authors (Eirik Fuller for alt-mouse and Peter Moore for
;;; x-mouse-drag) for their work.

;;; The 120 valid click events are defined by where they occured (one of
;;; the 5 different recognized parts of the window) and what keyboard
;;; modifiers are used (none OR control, shift, meta, or any combination
;;; of the three modifiers).  Drag events are defined by where they
;;; start, where they end, and what keyboard modifiers are used.  Obviously
;;; there are a lot of them, but only a small number of them actually do
;;; anything.  Dragging mode lines around can resize windows vertically.
;;; Dragging borders around resize windows horizontally.  Dragging an
;;; intersection resizes horizontally and vertically simoultaneously.
;;; Keyboard modifiers are ignored for these drags.  The other type of
;;; drag that is recognized are those that begin and end in the same
;;; window.  All 24 of these (with keyboard modifiers) can be set by the
;;; user.

;;; To examine the default bindings, either look at the 'setq' lines at
;;; the bottom of the file or run the command x-mouse-help (bound to
;;; shift-button-1 in the mode line.

;;; HISTORY:
;;;
;;;   1.5        On a mouse release, the cursor moves to the release
;;;              position for 1 second followed by the press position
;;;              for 1 second (if the two are different).  If you do
;;;              something else in those 2 seconds, the jumping around
;;;              is cancelled and your new command executed.  Thanks to
;;;              Mike Gunter.  Also, added the x-mouse-blink-cursor
;;;              variable.  Thanks to Massimo and Stephen Gabe.
;;;
;;;              Fixed a problem that x-mouse-help couldn't deal with
;;;              undocumented functions.  Thanks to Mark Wright for the fix.
;;;              Also now handles lambda function documentation.  Thanks
;;;              to Vasco Lopes Paulo.
;;;
;;;              Rearranged some of the mouse bindings.  I won't do this
;;;              again but I wanted to organize them a little to make them
;;;              easier to remember.  Use x-mouse-help to find out what's
;;;              changed.  Also renamed some of the functions to make the
;;;              names follow the same convention as the others.  I will
;;;              not rename functions again.
;;;
;;;              Modified the mini-click bindings.  Thanks to Vasco Lopes
;;;              Paulo.
;;;
;;;              Added x-mouse-scroll-line and x-mouse-call-last-kbd-macro.
;;;              If you use macros, check out this second one.  Thanks to Vasco
;;;              Lopes Paulo for the code.
;;;
;;;              Added (defvar x-mouse-press nil) so that if the first mouse
;;;              press made was in some other package, x-mouse-release does
;;;              not report an error.  Thanks to Stephen Tweedie.
;;;
;;;              Moved the user functions to a separate file.
;;;
;;;              Added x-mouse-execute-extended-command.  It is bound to
;;;              cms3-window-click and cms3-window-drag.  This will allow
;;;              you to execute any click or drag command by name.  Thanks
;;;              to Don Comeau.
;;;
;;;              x-buffer-menu now executes buffer-menu if popup-menus
;;;              aren't available.  Thanks to Mats Lidell and Ric Claus.
;;;
;;;              Buffer menu fields are now lined up nicely.  Thanks to Chris.
;;;
;;;              x-mouse-scroll-to-proportion will now go all the way to the
;;;              bottom rather than only most of the way to the bottom.  I.e.
;;;              clicking on the bottom line of a 10 line buffer will now go
;;;              to 100% of the way through the file rather then only 90% of
;;;              the way.  Thanks to Mats Lidell.
;;;
;;;              Changed all the setq's to defvar's in the bottom section
;;;              and removed the 'x-sb-mouse-hook.  It was stupid not to
;;;              do it this way to begin with.  Also added the provide line.
;;;              Thanks to Torp Anders.
;;;
;;;              Added x-mouse-dir variable.
;;;
;;;              Added the info file.  This is only a first draft.  Please
;;;              send me comments/suggestions/corrections.
;;;
;;;              Added x-mouse-split-vertically and x-mouse-split-horizontally.
;;;              Thanks to Mats Lidell.
;;;
;;;              x-mouse-help now handles nil documentation and very long
;;;              documentation.  Thanks to Nitan More.  I also added the
;;;              x-mouse-help-to-menu variable and the option in the help
;;;              menu to send the output to the *Mouse Help* buffer.
;;;              Thanks to Chris Moore.
;;;
;;;              Added x-mouse-copy-thing and x-mouse-cut-thing.  Thanks
;;;              to Nitin More, Dag Wanvik.  Thanks to Vasco Lopes Paulo
;;;              for pointing out thing.el as an easy way to do this.
;;;              These allow copying/cutting words and sexps.
;;;
;;;              Added x-mouse-delete-this-window.  Thanks to Adam Hudd.
;;;
;;;   patch 2    Fixed a problem for people who don't have x-popup-menu.
;;;              Thanks to Richard Gonzalez and Joerg-Cyril Hoele.
;;;
;;;   patch 1    Fixed x-mouse-help when output is to "*Mouse Help*" buffer.
;;;
;;; The history of previous versions has been moved to the info file.
;;;
;;; I have tried to acknowledge all people who have contributed something,
;;; but occasionally I forget.  I apologize if I forgot you.  Mail me and
;;; I'll correct it.

;;; COMMON QUESTIONS:
;;;
;;; Highlighting: I do not support highlighting (yet) in x-sb-mouse.
;;; The reason for this is that there is no standard way to do highlighting
;;; inside of emacs without first applying an unofficial patch to the
;;; source code and recompiling emacs.  Because it is unofficial, I do
;;; not support it.  Emacs 19 will have highlighting.  So does Epoch, but
;;; I don't use Epoch.  If anyone happens to fix this to highlight under
;;; Epoch, I'd appreciate it if you'd send me the diffs so that I can
;;; add it to the standard distribution.
;;;
;;; Popup menus:  Popup menus only work if emacs was compiled with
;;; HAVE_X_MENUS defined in the config.h file.  This does not work on
;;; all computers, but is part of the standard source code so I use it.

;;; KNOWN BUGS:
;;;
;;; This has been moved to the info file.

;;; INSTALLING AND CUSTOMIZING
;;;
;;; To try this out once (but NOT have it load every time you start up
;;; emacs), just load it with the 'M-x load-file' command.  To have it
;;; automatically start up each time you use emacs, just add the
;;; following to your .emacs file:
;;;
;;;   (setq term-setup-hook '(lambda () (load-library "x-sb-mouse")))
;;;
;;; If x-sb-mouse is not in your elisp load path (if you've got it
;;; somewhere in your home directory), add the full path to the
;;; command.  For example, I keeps some stuff in a lisp subdirectory
;;; so I would add to my .emacs file:
;;;
;;;   (setq term-setup-hook
;;;         '(lambda () (load "~/lisp/x-sb-mouse")))
;;;
;;; If you use any packages that require x-mouse (such as hyperbole or
;;; gdbsrc), they should be loaded AFTER x-sb-mouse.  They will probably
;;; redefine some of the default key bindings.  The function
;;; x-mouse-init-mouse-map will redefine these keys to whatever functions
;;; I have defined or that you have defined in your .emacs file.  The
;;; problem is that this will remove the bindings used by the other
;;; packages.  Hyperbole does NOT have this problem.  It allows you to
;;; toggle between it's bindings and your personal mouse bindings.  I
;;; am planning on adding a simple toggle function to x-sb-mouse which
;;; will hopefully solve this problem.
;;; 
;;; To change the bindings, simply add a line to your .emacs file putting
;;; in a new command.  For example, if you want clicking the first button
;;; with a meta modifier to insert the letter "a" and clicking the second
;;; button with the control and shift keys pressed to recenter the screen.
;;; and clicking the 3rd button with no modifiers in the mode line to do
;;; nothing, just add the following to your .emacs file:
;;;    (setq x-mouse-m1-window-click '(lambda () (insert "a")))
;;;    (setq x-mouse-cs2-window-click 'recenter)
;;;    (setq x-mouse-3-mode-click 'x--mouse-ignore)
;;;
;;; In setting the variable, the modifiers (c, s, or m) must be included
;;; in alphabetical order, so variable x-mouse-csm1-window-drag is valid
;;; (and is one of the 24 redefinable window drags), but
;;; x-mouse-msc1-window-drag is not.

;;; Please send any comments or suggestions to me at beck@qtp.ufl.edu.


;;;*************************************************************************
;;; User definable variables.  Set these in your .emacs file.

;;(defvar x-mouse-dir "~/lisp")
(defvar x-mouse-dir nil 
  "*The directory where x-sb-mouse lives.
If it is set to nil, then it looks in the load path.")

(defvar x-mouse-blink-cursor nil
  "*If this is t, the cursor will blink temporarily to where a mouse event is.
Pressing a mouse button will cause the cursor to appear there for a second.
Releasing the mouse will cause the cursor to appear there for a second and
then appear at the position of the press (if it was different) for a second.
These 'blinks' do not interfere with executing commands during the delay.")

(defvar x-mouse-auto-set-mark t
  "*If this is non-nil, the mark will be set every time the point is set.
With this set, clicking in two spots defines a region (which you can
kill, copy, etc.)")

(defvar x-mouse-duplicate-cut t
  "*If non-nil, saving to the x-cut-buffer saves to the kill ring too.
In other words, every time a region is saved/appended to the x-cut-buffer,
it is also saved/appended to the kill ring.")

(defvar x-mouse-init-map t
  "*If non-nil, initializes the mouse map to include all my default bindings.
If it is t, all of the functionality still exists but the buttons are not
defined and you'll have to do it yourself.")

(defvar x-mouse-help-to-menu t
  "*If nil, sends help to a buffer rather than popup menus.
Nice if you want to be able to refer back to the help while you type.
Rather then set this manually, there is now an option in the menu to
do this as a one time only command.")


;;;*************************************************************************
;;; First we'll define the mouse map and some other necessary things.  This
;;; section should probably not be altered.
(provide 'x-mouse)
(provide 'x-sb-mouse)

(defconst x-button-right (char-to-string 0))
(defconst x-button-right-up (char-to-string 4))
(defconst x-button-middle (char-to-string 1))
(defconst x-button-middle-up (char-to-string 5))
(defconst x-button-left (char-to-string 2))
(defconst x-button-left-up (char-to-string 6))

(defconst x-button-s-right (char-to-string 16))
(defconst x-button-s-right-up (char-to-string 20))
(defconst x-button-s-middle (char-to-string 17))
(defconst x-button-s-middle-up (char-to-string 21))
(defconst x-button-s-left (char-to-string 18))
(defconst x-button-s-left-up (char-to-string 22))

(defconst x-button-m-right (char-to-string 32))
(defconst x-button-m-right-up (char-to-string 36))
(defconst x-button-m-middle (char-to-string 33))
(defconst x-button-m-middle-up (char-to-string 37))
(defconst x-button-m-left (char-to-string 34))
(defconst x-button-m-left-up (char-to-string 38))

(defconst x-button-c-right (char-to-string 64))
(defconst x-button-c-right-up (char-to-string 68))
(defconst x-button-c-middle (char-to-string 65))
(defconst x-button-c-middle-up (char-to-string 69))
(defconst x-button-c-left (char-to-string 66))
(defconst x-button-c-left-up (char-to-string 70))

(defconst x-button-m-s-right (char-to-string 48))
(defconst x-button-m-s-right-up (char-to-string 52))
(defconst x-button-m-s-middle (char-to-string 49))
(defconst x-button-m-s-middle-up (char-to-string 53))
(defconst x-button-m-s-left (char-to-string 50))
(defconst x-button-m-s-left-up (char-to-string 54))

(defconst x-button-c-s-right (char-to-string 80))
(defconst x-button-c-s-right-up (char-to-string 84))
(defconst x-button-c-s-middle (char-to-string 81))
(defconst x-button-c-s-middle-up (char-to-string 85))
(defconst x-button-c-s-left (char-to-string 82))
(defconst x-button-c-s-left-up (char-to-string 86))

(defconst x-button-c-m-right (char-to-string 96))
(defconst x-button-c-m-right-up (char-to-string 100))
(defconst x-button-c-m-middle (char-to-string 97))
(defconst x-button-c-m-middle-up (char-to-string 101))
(defconst x-button-c-m-left (char-to-string 98))
(defconst x-button-c-m-left-up (char-to-string 102))

(defconst x-button-c-m-s-right (char-to-string 112))
(defconst x-button-c-m-s-right-up (char-to-string 116))
(defconst x-button-c-m-s-middle (char-to-string 113))
(defconst x-button-c-m-s-middle-up (char-to-string 117))
(defconst x-button-c-m-s-left (char-to-string 114))
(defconst x-button-c-m-s-left-up (char-to-string 118))

(defvar x-process-mouse-hook nil
  "Hook to run after each mouse event is processed.  Should take two
arguments; the first being a list (XPOS YPOS) corresponding to character
offset from top left of screen and the second being a specifier for the
buttons/keys.  This will normally be set on a per-buffer basis.")
(defun x-flush-mouse-queue ()
  "Process all queued mouse events."
  ;; A mouse event causes a special character sequence to be given
  ;; as keyboard input.  That runs this function, which process all
  ;; queued mouse events and returns.
  (interactive)
  (while (> (x-mouse-events) 0)
    (x-proc-mouse-event))
  (and (boundp 'x-process-mouse-hook)
       (symbol-value 'x-process-mouse-hook)
       (funcall x-process-mouse-hook x-mouse-pos x-mouse-item)))
(define-key global-map "\C-c\C-m" 'x-flush-mouse-queue)
(define-key global-map "\C-x\C-@" 'x-flush-mouse-queue)



;;;*************************************************************************
;;; The following functions are not called directly by the user.  They
;;; are used in the main functions and can used in making new mouse
;;; functions.  Feel free to add others.

(defun x-mouse-mini-p (arg)
  "If arg is in the minibuffer, return the minibuffer window, else NIL."
  (let* ((s (screen-height))
         (w (minibuffer-window))
         (m (window-height w))
         (y (car (cdr arg))))
    (if (>= y (- s m))
        w
      '())))

(defun x-mouse-mode-p (arg &optional win)
  "If arg is on a mode line, return the window, else NIL."
  (let* ((w (if win win (x-mouse-window arg)))
         (m (eq w (minibuffer-window)))
         (y (car (cdr arg)))
         (bot   (nth 3 (window-edges w))))
    (if (= y (1- bot))
        (if m () w)
      ())))

(defun x-mouse-border-p (arg &optional win)
  "If arg is on a border, returns the window, else NIL."
  (let* ((w (if win win (x-mouse-window arg)))
         (x (car arg))
         (s (screen-width))
         (r (nth 2 (window-edges w))))
    (if (= x (1- r))
        (if (= x (1- s))
            '()
          w)
      '())))

(defun x-mouse-inter-p (arg &optional win)
  "If arg is on an intersection, returns the window, else NIL."
  (let ((w (if win win (x-mouse-window arg))))
    (if (and (x-mouse-mode-p arg w) (x-mouse-border-p arg w))
        w
      '())))

(defun x-mouse-window (arg)
  "Returns the window that the mouse is currently in, even the minibuffer.
Also works for the mode line which the old version didn't."
  (let* ((start-w (selected-window))
         (done nil)
         (x (car arg))
         (y (car (cdr arg)))
         (w start-w)
         (edges nil)
         (top nil)
         (bot nil)
         (left nil)
         (right nil))
    (while (not done)
      (setq edges (window-edges w))
      (setq left  (nth 0 edges))
      (setq top   (nth 1 edges))
      (setq right (nth 2 edges))
      (setq bot   (nth 3 edges))
      (if (and (>= x left) (< x right)
               (>= y top)  (< y bot))
          (setq done t)
        (setq w (next-window w t))
        (if (eq w start-w)
            (setq done t w nil))))
    w))

(defun x-mouse-coords (arg &optional win)
  "Returns the coordinates of the mouse with respect to the window it's in.
NIL is returned if the mouse is on the mode line or border."
  (let* ((w (if win win (x-mouse-window arg)))
         (m (x-mouse-mode-p arg w))
         (b (x-mouse-border-p arg w)))
    (if (or m b)
        ()
      (coordinates-in-window-p arg w))))

(defun x-mouse-pos (win char)
  "Returns the position of char in the window.
The position returned is EXACTLY the same as if a mouse event occurred at
that point.  Win must be a window other than the minibuffer and char must
be visible."
  (let* ((edges (window-edges win))
	 (x     (nth 0 edges))
	 (y     (nth 1 edges))
	 (right (nth 2 edges))
	 (wid   (window-width win))
	 (swid  (screen-width))
	 trun col col0 pt)
    (save-window-excursion
      (select-window win)
      (setq trun truncate-lines
	    col  (current-column)
	    pt   (point))
      (save-excursion
	(goto-char (window-start))
	(if (= (point) pt) ()
	  (if (/= swid right)
	      (setq wid (1- wid)))
	  (while (<= (point) pt)
	    (vertical-motion 1)
	    (setq y (1+ y)))
	  (vertical-motion -1)
	  (setq y    (1- y)
		col0 (current-column)
		x    (+ x (- col (- col0 (% col0 (1- wid)))))))))
    (list x y)))

(defun x-mouse-point (arg &optional win)
  "Returns the POINT of the arg in the window it's in."
  (let* ((w      (if win win (x-mouse-window arg)))
         (coords (x-mouse-coords arg w))
         (x ())
         (y ())
         (margin-column ())
         (prompt-width ()))
    (save-excursion
      (save-window-excursion
        (select-window w)
        (if coords
            (progn
              (setq prompt-width (if (x-mouse-mini-p arg)
                                     (if (boundp 'minibuffer-prompt-width)
                                         minibuffer-prompt-width 0) 0)
                    x (car coords)
                    y (car (cdr coords)))
              (move-to-window-line y)
              (setq margin-column
                    (if (or truncate-lines (> (window-hscroll) 0))
                        (current-column)
                      (- (current-column)
                         (% (current-column) (1- (window-width))))))
              (move-to-column (+ x (1- (max 1 (window-hscroll)))
                                 (if (= (point) 1)
                                     (- prompt-width) 0)
                                 margin-column))))
        (point)))))

(defun x-mouse-resize ()
  "Test to see if a resize is possible.  Do it if it is."
  (let* ((sw (selected-window))
         (ow x-mouse-win-d)
         (nw x-mouse-win-u)
         (oy (car (cdr x-mouse-pos-d)))
         (ox (car x-mouse-pos-d))
         (ny (car (cdr x-mouse-pos-u)))
         (nx (car x-mouse-pos-u))
         (ot x-mouse-type-d)
         (nt x-mouse-type-u)
         (edges (window-edges nw))
         (left (nth 0 edges))
         (top (nth 1 edges))
         (right (nth 2 edges))
         (bot (nth 3 edges)))
    (cond
     ;; drag lowest mode line up
     ((and (eq ot 'mode) (eq nt 'window) (or (= oy (1- bot)) (= oy (1- top)))
           (x-mouse-mini-p (list ox (1+ oy))))
      (select-window (minibuffer-window))
      (enlarge-window (- oy ny))
      (select-window ow))
     ;; drag any mode line into a window
     ((and (eq ot 'mode) (eq nt 'window) (or (= oy (1- bot)) (= oy (1- top))))
      (select-window ow)
      (enlarge-window (- ny oy)))
     ;; drag any border into a window
     ((and (eq ot 'border) (eq nt 'window)
           (or (= ox (1- right)) (= ox (1- left))))
      (select-window ow)
      (enlarge-window (- nx ox) t))
     ;; drag an inter into a window
     ((and (eq ot 'inter) (eq nt 'window)
           (or (= oy (1- bot)) (= oy (1- top)) (= ox (1- right))
               (= ox (1- left))))
      (select-window ow)
      (enlarge-window (- ny oy))
      (enlarge-window (- nx ox) t))
     ;; drag lowest mode line down
     ((and (eq ot 'mode) (eq nt 'mini)
           (= oy (- (1- (screen-height)) (window-height (minibuffer-window))))
           (> (- (window-height (minibuffer-window)) (- ny oy)) 0))
      (select-window (minibuffer-window))
      (enlarge-window (- oy ny))
      (select-window ow))
     ;; drag an inter in the lowest mode line down
     ((and (eq ot 'inter) (eq nt 'mini)
           (= oy (- (1- (screen-height)) (window-height (minibuffer-window))))
           (> (- (window-height (minibuffer-window)) (- ny oy)) 0))
      (select-window (minibuffer-window))
      (enlarge-window (- oy ny))
      (select-window ow)
      (enlarge-window (- nx ox) t))
    )
    (select-window sw))
  t)


;;;*************************************************************************
;;; The following three functions (and the progn statement after) are what
;;; actually decide what to do when a press or release event occurs.  This
;;; section may be modified eventually as I add minibuffer functions and
;;; dragging from one window to another.

;;; All a mouse press does is set some variables.
(defun x-mouse-press (arg)
  "Sets up all the parameters describing the environment of a button press.
The following variables are set:
  x-mouse-point-0     The current point
  x-mouse-win-0       The current selected window
  x-mouse-pos-d       The position (relative to the screen) of the mouse
  x-mouse-win-d       The window the mouse is in (nil if minibuffer)
  x-mouse-coords-d    The position (relative to the window) of the mouse
  x-mouse-point-d     The (point) of the mouse position in the window
  x-mouse-type-d      The \"type\" of position where the press occured.
      'window refers to a position actually in the window
      'mode   refers to a position on the mode line of a window
      'border refers to the column separating two horizontally split windows
      'inter  refers to the intersection of a mode line and a border
      'mini   refers to the minibuffer
  x-mouse-press        This function was called for the press."
  (save-window-excursion
    (setq x-mouse-point-0 (point)
          x-mouse-win-0 (selected-window)
          x-mouse-press t
          x-mouse-pos-d arg
          x-mouse-win-d (x-mouse-window arg)
          x-mouse-coords-d (x-mouse-coords x-mouse-pos-d x-mouse-win-d)
          x-mouse-point-d (x-mouse-point arg x-mouse-win-d)
          x-mouse-type-d (cond
                          ((x-mouse-mini-p arg)
                           'mini)
                          ((x-mouse-inter-p arg x-mouse-win-d)
                           'inter)
                          ((x-mouse-mode-p arg x-mouse-win-d)
                           'mode)
                          ((x-mouse-border-p arg x-mouse-win-d)
                           'border)
                          (t 'window)))
    (if x-mouse-blink-cursor
	(progn
	  (select-window x-mouse-win-d)
	  (save-excursion
	    (goto-char x-mouse-point-d)
	    (sit-for 1))))))

(defvar x-mouse-press nil)
;;; This runction is called on a mouse release event and it sets up the
;;; remaining variables used to perform the desired action.
(defun x-mouse-release (arg)
  "Sets up all the parameters describing the environment of a button press.
The following variables are set:
  x-mouse-pos-u       The position (relative to the screen) of the mouse
  x-mouse-win-u       The window the mouse is in (nil if minibuffer)
  x-mouse-coords-u    The position (relative to the window) of the mouse
  x-mouse-point-u     The (point) of the mouse position in the window
  x-mouse-type-u      The \"type\" of position where the press occured.
      'window refers to a position actually in the window
      'mode   refers to a position on the mode line of a window
      'border refers to the column separating two horizontally split windows
      'inter  refers to the intersection of a mode line and a border
      'mini   refers to the minibuffer
  x-mouse-click       't if the down and up events occurred at the same place."
  (save-window-excursion
    (setq x-mouse-press ()
          x-mouse-pos-u arg
          x-mouse-click (equal x-mouse-pos-d x-mouse-pos-u)
          x-mouse-win-u (x-mouse-window arg)
          x-mouse-coords-u (x-mouse-coords x-mouse-pos-u x-mouse-win-u)
          x-mouse-point-u (x-mouse-point arg x-mouse-win-u)
          x-mouse-type-u (cond
                          ((x-mouse-mini-p arg)
                           'mini)
                          ((x-mouse-inter-p arg x-mouse-win-u)
                           'inter)
                          ((x-mouse-mode-p arg x-mouse-win-u)
                           'mode)
                          ((x-mouse-border-p arg x-mouse-win-u)
                           'border)
                          (t 'window)))
    (if x-mouse-blink-cursor
	(progn
	  (select-window x-mouse-win-u)
	  (save-excursion
	    (goto-char x-mouse-point-u)
	    (sit-for 1))
	  (if (not x-mouse-click)
	      (progn
		(select-window x-mouse-win-d)
		(save-excursion
		  (goto-char x-mouse-point-d)
		  (sit-for 1))))))))

;;; This function is called when a button is released.  The three arguments
;;; are the position of the mouse, the button number (1, 2, or 3), and
;;; a list of modifiers containing 'c, 's, and 'm in any combination.
(defun x-mouse (arg button modifier)
  (if x-mouse-press
      (progn
        (x-mouse-release arg)
        ;; base is a string "x-mouse-"
        ;; mods is one of the following: 
        ;;    "", "c", "m", "s", "cm", "cs", "ms", "cms"
        ;; but  is "1", "2", or "3"
        ;; op   contains base mods, but, and "-"
        ;; type contains "window", "mode", "border", "inter", or "mini"
        ;; function contains the whole string
        ;; x-mouse-windows contains info used in the resize commands
        (let* ((base "x-mouse-")
               (mods (if (memq 'c modifier) "c" ""))
               (mods (if (memq 'm modifier) (concat mods "m") mods))
               (mods (if (memq 's modifier) (concat mods "s") mods))
               (but  (int-to-string button))
               (op   (concat base mods but "-"))
               (type (symbol-name x-mouse-type-d))
               (function "x--mouse-ignore"))
          ;; list ALL the combinations under which the function is evaluated
          (cond
           ;; any click event
           ((and x-mouse-click
                 (setq function (concat op type "-click"))))
           ;; drag in a single window
           ((and (eq x-mouse-type-d 'window) (eq x-mouse-type-u 'window)
                 (eq x-mouse-win-d x-mouse-win-u)
                 (setq function (concat op "window-drag"))))
           ;; drag from mode to adjacent window
           ((and (eq x-mouse-type-d 'mode) (eq x-mouse-type-u 'window)
                 (x-mouse-resize)
                 (setq function "x--mouse-ignore")))
           ;; drag in a single mode line
           ((and (eq x-mouse-type-d 'mode) (eq x-mouse-type-u 'mode)
                 (eq x-mouse-win-d x-mouse-win-u)
                 (setq function (concat op "mode-click"))))
           ;; drag from mode to adjacent minibuffer
           ((and (eq x-mouse-type-d 'mode) (eq x-mouse-type-u 'mini)
                 (x-mouse-resize)
                 (setq function "x--mouse-ignore")))
           ;; drag from border to adjacent window
           ((and (eq x-mouse-type-d 'border) (eq x-mouse-type-u 'window)
                 (x-mouse-resize)
                 (setq function "x--mouse-ignore")))
           ;; drag in a single border
           ((and (eq x-mouse-type-d 'border) (eq x-mouse-type-u 'border)
                 (eq x-mouse-win-d x-mouse-win-u)
                 (setq function (concat op "border-click"))))
           ;; drag from inter to adjacent window
           ((and (eq x-mouse-type-d 'inter) (eq x-mouse-type-u 'window)
                 (x-mouse-resize)
                 (setq function "x--mouse-ignore")))
           ;; drag from inter to adjacent mini
           ((and (eq x-mouse-type-d 'inter) (eq x-mouse-type-u 'mini)
                 (x-mouse-resize)
                 (setq function "x--mouse-ignore")))
           ;; drag in mini
           ((and (eq x-mouse-type-d 'mini) (eq x-mouse-type-u 'mini)
                 (setq function (concat op "mini-click"))))
           )
          (setq x-mouse-press nil)
          (eval (list (symbol-value (intern-soft function))))))))

(defun x-mouse-init-mouse-map ()
  "Sets up the mouse map (or resets it if it has gotten altered).
This is useful for when some package alters the key bindings and
you want to recover the default ones."
  (interactive)
  (define-key mouse-map x-button-left 'x-mouse-press)
  (define-key mouse-map x-button-left-up
    '(lambda (arg) (x-mouse arg 1 '())))
  (define-key mouse-map x-button-middle 'x-mouse-press)
  (define-key mouse-map x-button-middle-up
    '(lambda (arg) (x-mouse arg 2 '())))
  (define-key mouse-map x-button-right 'x-mouse-press)
  (define-key mouse-map x-button-right-up
    '(lambda (arg) (x-mouse arg 3 '())))

  (define-key mouse-map x-button-c-left 'x-mouse-press)
  (define-key mouse-map x-button-c-left-up
    '(lambda (arg) (x-mouse arg 1 '(c))))
  (define-key mouse-map x-button-c-middle 'x-mouse-press)
  (define-key mouse-map x-button-c-middle-up
    '(lambda (arg) (x-mouse arg 2 '(c))))
  (define-key mouse-map x-button-c-right 'x-mouse-press)
  (define-key mouse-map x-button-c-right-up
    '(lambda (arg) (x-mouse arg 3 '(c))))

  (define-key mouse-map x-button-m-left 'x-mouse-press)
  (define-key mouse-map x-button-m-left-up
    '(lambda (arg) (x-mouse arg 1 '(m))))
  (define-key mouse-map x-button-m-middle 'x-mouse-press)
  (define-key mouse-map x-button-m-middle-up
    '(lambda (arg) (x-mouse arg 2 '(m))))
  (define-key mouse-map x-button-m-right 'x-mouse-press)
  (define-key mouse-map x-button-m-right-up
    '(lambda (arg) (x-mouse arg 3 '(m))))

  (define-key mouse-map x-button-s-left 'x-mouse-press)
  (define-key mouse-map x-button-s-left-up
    '(lambda (arg) (x-mouse arg 1 '(s))))
  (define-key mouse-map x-button-s-middle 'x-mouse-press)
  (define-key mouse-map x-button-s-middle-up
    '(lambda (arg) (x-mouse arg 2 '(s))))
  (define-key mouse-map x-button-s-right 'x-mouse-press)
  (define-key mouse-map x-button-s-right-up
    '(lambda (arg) (x-mouse arg 3 '(s))))

  (define-key mouse-map x-button-c-m-left 'x-mouse-press)
  (define-key mouse-map x-button-c-m-left-up
    '(lambda (arg) (x-mouse arg 1 '(c m))))
  (define-key mouse-map x-button-c-m-middle 'x-mouse-press)
  (define-key mouse-map x-button-c-m-middle-up
    '(lambda (arg) (x-mouse arg 2 '(c m))))
  (define-key mouse-map x-button-c-m-right 'x-mouse-press)
  (define-key mouse-map x-button-c-m-right-up
    '(lambda (arg) (x-mouse arg 3 '(c m))))

  (define-key mouse-map x-button-c-s-left 'x-mouse-press)
  (define-key mouse-map x-button-c-s-left-up
    '(lambda (arg) (x-mouse arg 1 '(c s))))
  (define-key mouse-map x-button-c-s-middle 'x-mouse-press)
  (define-key mouse-map x-button-c-s-middle-up
    '(lambda (arg) (x-mouse arg 2 '(c s))))
  (define-key mouse-map x-button-c-s-right 'x-mouse-press)
  (define-key mouse-map x-button-c-s-right-up
    '(lambda (arg) (x-mouse arg 3 '(c s))))

  (define-key mouse-map x-button-m-s-left 'x-mouse-press)
  (define-key mouse-map x-button-m-s-left-up
    '(lambda (arg) (x-mouse arg 1 '(m s))))
  (define-key mouse-map x-button-m-s-middle 'x-mouse-press)
  (define-key mouse-map x-button-m-s-middle-up
    '(lambda (arg) (x-mouse arg 2 '(m s))))
  (define-key mouse-map x-button-m-s-right 'x-mouse-press)
  (define-key mouse-map x-button-m-s-right-up
    '(lambda (arg) (x-mouse arg 3 '(m s))))

  (define-key mouse-map x-button-c-m-s-left 'x-mouse-press)
  (define-key mouse-map x-button-c-m-s-left-up
    '(lambda (arg) (x-mouse arg 1 '(c m s))))
  (define-key mouse-map x-button-c-m-s-middle 'x-mouse-press)
  (define-key mouse-map x-button-c-m-s-middle-up
    '(lambda (arg) (x-mouse arg 2 '(c m s))))
  (define-key mouse-map x-button-c-m-s-right 'x-mouse-press)
  (define-key mouse-map x-button-c-m-s-right-up
    '(lambda (arg) (x-mouse arg 3 '(c m s)))))

(if x-mouse-init-map
    (x-mouse-init-mouse-map))


;;;*************************************************************************
;;; Define the valid click/drag functions.  These are the functions that
;;; can be bound directly to the mouse buttons.  Feel free to add functions.
;;; Any function here can be assigned to either a drag or click event as
;;; appropriate.  Many have an optional argument.  This argument is to
;;; make the function compatible with x-mouse.  Hopefully, any package
;;; that required x-mouse will work equally well with x-sb-mouse instead.

(if x-mouse-dir
    (progn
      (load (concat (file-name-as-directory x-mouse-dir) "thing"))
      (load (concat (file-name-as-directory x-mouse-dir) "xsbm-userfuns")))
  (load "thing")
  (load "xsbm-userfuns"))


;;;*************************************************************************
;;; These are the default settings.  To change any of them, just
;;; add a line to your x-sb-mouse-hook with an altenate bindings.
;;; The lines should be self explanatory and probably don't need to
;;; be changed.

(defvar x-mouse-1-window-click    'x-mouse-set-point)
(defvar x-mouse-c1-window-click   'x-mouse-call-last-kbd-macro)
(defvar x-mouse-m1-window-click   'x-mouse-insert-rect-000-here)
(defvar x-mouse-s1-window-click   'x-mouse-copy-thing)
(defvar x-mouse-cm1-window-click  'x-mouse-yank-here)
(defvar x-mouse-cs1-window-click  'x-mouse-scroll-to-top)
(defvar x-mouse-ms1-window-click  'x-mouse-copy-bol-to-x)
(defvar x-mouse-cms1-window-click 'x-mouse-scroll-to-proportion)

(defvar x-mouse-2-window-click    'x-mouse-paste-text)
(defvar x-mouse-c2-window-click   'x-mouse-paste-text)
(defvar x-mouse-m2-window-click   'x-mouse-insert-rect-000)
(defvar x-mouse-s2-window-click   'x-mouse-cut-thing)
(defvar x-mouse-cm2-window-click  'x-mouse-yank-there)
(defvar x-mouse-cs2-window-click  'x-mouse-scroll-to-center)
(defvar x-mouse-ms2-window-click  'x-mouse-copy-line-to-x)
(defvar x-mouse-cms2-window-click 'x-mouse-paste-text)

(defvar x-mouse-3-window-click    'x-mouse-paste-there)
(defvar x-mouse-c3-window-click   'x-mouse-copy-kill-to-x)
(defvar x-mouse-m3-window-click   'x-mouse-insert-rect-000-there)
(defvar x-mouse-s3-window-click   'x-mouse-paste-there)
(defvar x-mouse-cm3-window-click  'x-mouse-paste-there)
(defvar x-mouse-cs3-window-click  'x-mouse-scroll-to-bottom)
(defvar x-mouse-ms3-window-click  'x-mouse-copy-eol-to-x)
(defvar x-mouse-cms3-window-click 'x-mouse-execute-extended-command)

(defvar x-mouse-1-mode-click      'x-mouse-select)
(defvar x-mouse-c1-mode-click     'x-mouse-buffer-menu)
(defvar x-mouse-m1-mode-click     'x-mouse-buffer-menu)
(defvar x-mouse-s1-mode-click     'x-mouse-help)
(defvar x-mouse-cm1-mode-click    'x-mouse-select)
(defvar x-mouse-cs1-mode-click    'x-mouse-select)
(defvar x-mouse-ms1-mode-click    'x-mouse-scroll-to-proportion)
(defvar x-mouse-cms1-mode-click   'x-mouse-scroll-to-proportion)

(defvar x-mouse-2-mode-click      'x-mouse-scroll-down)
(defvar x-mouse-c2-mode-click     'x-mouse-keep-one-window)
(defvar x-mouse-m2-mode-click     'x-mouse-scroll-down)
(defvar x-mouse-s2-mode-click     'x-mouse-scroll-down)
(defvar x-mouse-cm2-mode-click    'x-mouse-scroll-down)
(defvar x-mouse-cs2-mode-click    'x-mouse-scroll-down)
(defvar x-mouse-ms2-mode-click    'x-mouse-scroll-to-proportion)
(defvar x-mouse-cms2-mode-click   'x-mouse-scroll-to-proportion)

(defvar x-mouse-3-mode-click      'x-mouse-scroll-up)
(defvar x-mouse-c3-mode-click     'x-mouse-delete-this-window)
(defvar x-mouse-m3-mode-click     'x-mouse-scroll-up)
(defvar x-mouse-s3-mode-click     'x-mouse-scroll-up)
(defvar x-mouse-cm3-mode-click    'x-mouse-scroll-up)
(defvar x-mouse-cs3-mode-click    'x-mouse-scroll-up)
(defvar x-mouse-ms3-mode-click    'x-mouse-scroll-to-proportion)
(defvar x-mouse-cms3-mode-click   'x-mouse-scroll-to-proportion)

(defvar x-mouse-1-border-click    'x-mouse-select)
(defvar x-mouse-c1-border-click   'x-mouse-select)
(defvar x-mouse-m1-border-click   'x-mouse-select)
(defvar x-mouse-s1-border-click   'x-mouse-select)
(defvar x-mouse-cm1-border-click  'x-mouse-select)
(defvar x-mouse-cs1-border-click  'x-mouse-select)
(defvar x-mouse-ms1-border-click  'x-mouse-scroll-to-proportion)
(defvar x-mouse-cms1-border-click 'x-mouse-scroll-to-proportion)

(defvar x-mouse-2-border-click    'x-mouse-scroll-down)
(defvar x-mouse-c2-border-click   'x-mouse-scroll-down)
(defvar x-mouse-m2-border-click   'x-mouse-scroll-down)
(defvar x-mouse-s2-border-click   'x-mouse-scroll-down)
(defvar x-mouse-cm2-border-click  'x-mouse-scroll-down)
(defvar x-mouse-cs2-border-click  'x-mouse-scroll-down)
(defvar x-mouse-ms2-border-click  'x-mouse-scroll-to-proportion)
(defvar x-mouse-cms2-border-click 'x-mouse-scroll-to-proportion)

(defvar x-mouse-3-border-click    'x-mouse-scroll-up)
(defvar x-mouse-c3-border-click   'x-mouse-scroll-up)
(defvar x-mouse-m3-border-click   'x-mouse-scroll-up)
(defvar x-mouse-s3-border-click   'x-mouse-scroll-up)
(defvar x-mouse-cm3-border-click  'x-mouse-scroll-up)
(defvar x-mouse-cs3-border-click  'x-mouse-scroll-up)
(defvar x-mouse-ms3-border-click  'x-mouse-scroll-to-proportion)
(defvar x-mouse-cms3-border-click 'x-mouse-scroll-to-proportion)

(defvar x-mouse-1-inter-click     'x-mouse-buffer-menu)
(defvar x-mouse-c1-inter-click    'x-mouse-buffer-menu)
(defvar x-mouse-m1-inter-click    'x-mouse-buffer-menu)
(defvar x-mouse-s1-inter-click    'x-mouse-buffer-menu)
(defvar x-mouse-cm1-inter-click   'x-mouse-buffer-menu)
(defvar x-mouse-cs1-inter-click   'x-mouse-buffer-menu)
(defvar x-mouse-ms1-inter-click   'x-mouse-buffer-menu)
(defvar x-mouse-cms1-inter-click  'x-mouse-buffer-menu)

(defvar x-mouse-2-inter-click     'x-mouse-buffer-menu)
(defvar x-mouse-c2-inter-click    'x-mouse-buffer-menu)
(defvar x-mouse-m2-inter-click    'x-mouse-buffer-menu)
(defvar x-mouse-s2-inter-click    'x-mouse-buffer-menu)
(defvar x-mouse-cm2-inter-click   'x-mouse-buffer-menu)
(defvar x-mouse-cs2-inter-click   'x-mouse-buffer-menu)
(defvar x-mouse-ms2-inter-click   'x-mouse-buffer-menu)
(defvar x-mouse-cms2-inter-click  'x-mouse-buffer-menu)

(defvar x-mouse-3-inter-click     'x-mouse-buffer-menu)
(defvar x-mouse-c3-inter-click    'x-mouse-buffer-menu)
(defvar x-mouse-m3-inter-click    'x-mouse-buffer-menu)
(defvar x-mouse-s3-inter-click    'x-mouse-buffer-menu)
(defvar x-mouse-cm3-inter-click   'x-mouse-buffer-menu)
(defvar x-mouse-cs3-inter-click   'x-mouse-buffer-menu)
(defvar x-mouse-ms3-inter-click   'x-mouse-buffer-menu)
(defvar x-mouse-cms3-inter-click  'x-mouse-buffer-menu)

(defvar x-mouse-1-mini-click
  '(lambda () 
     "Repeats last complex command or goes to the previous complex command.
If point is in minibuffer goes to the previous complex command."
     (if (not (boundp 'repeat-complex-command-arg))
	 (repeat-complex-command 1))
     (previous-complex-command 1)))
(defvar x-mouse-c1-mini-click     'x--mouse-ignore)
(defvar x-mouse-m1-mini-click     'x--mouse-ignore)
(defvar x-mouse-s1-mini-click     'x--mouse-ignore)
(defvar x-mouse-cm1-mini-click    'x--mouse-ignore)
(defvar x-mouse-cs1-mini-click    'x--mouse-ignore)
(defvar x-mouse-ms1-mini-click    'x--mouse-ignore)
(defvar x-mouse-cms1-mini-click   'x--mouse-ignore)

(defvar x-mouse-2-mini-click
  '(lambda () 
     "Repeats last complex command or pastes X cut buffer.
If point is in minibuffer pastes X cut buffer."
     (if (eq (window-buffer (minibuffer-window)) (current-buffer))
	 (x-mouse-paste-text)
       (repeat-complex-command 1))))
(defvar x-mouse-c2-mini-click     'x--mouse-ignore)
(defvar x-mouse-m2-mini-click     'x--mouse-ignore)
(defvar x-mouse-s2-mini-click     'x--mouse-ignore)
(defvar x-mouse-cm2-mini-click    'x--mouse-ignore)
(defvar x-mouse-cs2-mini-click    'x--mouse-ignore)
(defvar x-mouse-ms2-mini-click    'x--mouse-ignore)
(defvar x-mouse-cms2-mini-click   'x--mouse-ignore)

(defvar x-mouse-3-mini-click
  '(lambda ()
     "Repeats last complex command or goes to the next complex command.
If point is in minibuffer goes to the next complex command."
     (if (not (boundp 'repeat-complex-command-arg))
	 (repeat-complex-command 1))
     (next-complex-command 1)))
(defvar x-mouse-c3-mini-click     'x--mouse-ignore)
(defvar x-mouse-m3-mini-click     'x--mouse-ignore)
(defvar x-mouse-s3-mini-click     'x--mouse-ignore)
(defvar x-mouse-cm3-mini-click    'x--mouse-ignore)
(defvar x-mouse-cs3-mini-click    'x--mouse-ignore)
(defvar x-mouse-ms3-mini-click    'x--mouse-ignore)
(defvar x-mouse-cms3-mini-click   'x--mouse-ignore)

(defvar x-mouse-1-window-drag     'x-mouse-copy-text)
(defvar x-mouse-c1-window-drag    'x-mouse-scroll-line)
(defvar x-mouse-m1-window-drag    'x-mouse-copy-rect-to-000)
(defvar x-mouse-s1-window-drag    'x-mouse-copy-text-to-point)
(defvar x-mouse-cm1-window-drag   'x-mouse-copy-text)
(defvar x-mouse-cs1-window-drag   'x-mouse-copy-text)
(defvar x-mouse-ms1-window-drag   'x-mouse-copy-rect-to-x)
(defvar x-mouse-cms1-window-drag  'x-mouse-copy-text)

(defvar x-mouse-2-window-drag     'x-mouse-cut-text)
(defvar x-mouse-c2-window-drag    'x-mouse-cut-text)
(defvar x-mouse-m2-window-drag    'x-mouse-cut-rect-to-000)
(defvar x-mouse-s2-window-drag    'x-mouse-cut-text-to-point)
(defvar x-mouse-cm2-window-drag   'x-mouse-cut-text)
(defvar x-mouse-cs2-window-drag   'x-mouse-cut-text)
(defvar x-mouse-ms2-window-drag   'x-mouse-cut-rect-to-x)
(defvar x-mouse-cms2-window-drag  'x-mouse-cut-text)

(defvar x-mouse-3-window-drag     'x-mouse-append-drag)
(defvar x-mouse-c3-window-drag    'x-mouse-append-drag)
(defvar x-mouse-m3-window-drag    'x-mouse-open-rect)
(defvar x-mouse-s3-window-drag    'x-mouse-append-drag)
(defvar x-mouse-cm3-window-drag   'x-mouse-append-drag)
(defvar x-mouse-cs3-window-drag   'x-mouse-append-drag)
(defvar x-mouse-ms3-window-drag   'x-mouse-append-drag)
(defvar x-mouse-cms3-window-drag  'x-mouse-execute-extended-command)
