;;;; -*-Emacs-Lisp-*- Improved Vertical Scrolling Commands
;;;; Written by Eric Eide, last modified on January 21, 1993.
;;;; (C) Copyright 1993, Eric Eide and the University of Utah
;;;;
;;;; COPYRIGHT NOTICE
;;;;
;;;; This program is free software; you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation; either version 1, or (at your option) any later
;;;; version.
;;;;
;;;; This program is distributed in the hope that it will be useful, but 
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;;;; for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License along
;;;; with this program; if not, write to the Free Software Foundation, Inc.,
;;;; 675 Mass Ave, Cambridge, MA 02139, USA.

;;;; AUTHORS
;;;;
;;;; This package was written by Eric Eide (eeide@cs.utah.edu) and was based on
;;;; a very similar package ("scroll-fix") by Joe Wells.  Almost all of the
;;;; code in this file is original, but I owe a great debt to Mr. Wells for his
;;;; ideas and his original implementation.
;;;;
;;;;   Eric Eide (eeide@cs.utah.edu)
;;;;   University of Utah
;;;;   3190 Merrill Engineering Building
;;;;   Salt Lake City, Utah  84112
;;;;
;;;;   Joe Wells (jbw@cs.bu.edu)
;;;;
;;;; Joe Wells' "scroll-fix" package is Copyright (C) 1988, 1989, and 1991 by
;;;; the Free Software Foundation.  It is distributed under the terms of the
;;;; GNU General Public License.

;;;; LISP CODE DIRECTORY INFORMATION
;;;;
;;;; LCD Archive Entry:
;;;; scroll-in-place|Eric Eide|eeide@cs.utah.edu|
;;;; Improved vertical scrolling commands|
;;;; January 21 1993|||

;;;; SUMMARY
;;;;
;;;; This package provides improved vertical scrolling commands for GNU Emacs.
;;;; These new commands offer the following features:
;;;;
;;;; + When a scrolling command is executed, GNU Emacs tries to keep point as
;;;;   close as possible to its original window position (window line and
;;;;   column).  This is what "scroll in place" means: point stays "in place"
;;;;   within the window.  (There are times when point must be moved from its
;;;;   original window position in order to execute the scroll; see below.)
;;;;
;;;; + Because the improved scrolling commands keep point at its original
;;;;   window position, these scrolling commands are "reversible."  The
;;;;   scroll-up command undoes the effect of the immediately previous
;;;;   scroll-down command (if any) and vice versa.  In other words, if you
;;;;   scroll up and then immediately scroll back down, the window
;;;;   configuration is restored to its exact original state.  This allows you
;;;;   to browse through a buffer more easily, as you can always get back to
;;;;   the original configuration.
;;;;
;;;;   Note, however, that the improved scrolling commands are guaranteed to be
;;;;   reversible only if there are no intervening non-scrolling commands.
;;;;   Also, if you give a prefix argument to a scrolling command (in order to
;;;;   specify the scrolling distance), previous scrolling commands may no
;;;;   longer be reversible.  The new prefix argument takes precedence.
;;;;
;;;;   You might find it useful to think of the scrolling commands as forming
;;;;   "chains."  A scrolling command either starts or continues a chain.
;;;;   Non-scrolling commands and explicit prefix arguments break the chain.
;;;;   Scrolling commands are guaranteed to be reversible only within the
;;;;   current chain.  Hopefully that's clear enough.
;;;;
;;;; + When a scrolling command is given a prefix argument (which specifies the
;;;;   number of lines to scroll), then that argument becomes the default
;;;;   scrolling distance for all immediately subsequent scrolling commands.
;;;;   This means that you can easily set the scrolling distance for a chain
;;;;   of scrolling commands.  Note that a new prefix argument or any non-
;;;;   scrolling command breaks the chain (as described above), and any further
;;;;   scrolling commands will use the usual defaults (or the prefix argument
;;;;   you specify at that time, of course).
;;;;
;;;; + The improved scrolling commands will avoid displaying empty lines past
;;;;   the end of the buffer when possible.  In other words, just as you can't
;;;;   see "dead space" before the beginning of the buffer text, the new
;;;;   scrolling commands try to avoid displaying "dead space" past the end of
;;;;   the buffer text.  This behavior is somewhat configurable; see the
;;;;   documentation for the variable scroll-allow-blank-lines-past-eob.
;;;;
;;;;   Dead space will be displayed if it is necessary in order to make a
;;;;   previous scrolling action reversible, however.
;;;;
;;;; + If the scrolling commands cannot keep point at its initial window
;;;;   position (because a buffer boundary is on screen and the window can't be
;;;;   scrolled as far as necessary to keep point at the right place), point is
;;;;   allowed to temporarily stray from its initial window position.  That is,
;;;;   point moves the correct number of window lines, even if it means that it
;;;;   has to stray from its desired window position.  This straying is undone
;;;;   when (and if) the scrolling action is reversed.
;;;;
;;;; + If a scrolling command tries to move point past a buffer boundary, point
;;;;   is instead moved to the boundary (the beginning or the end of the buffer
;;;;   as appropriate) and an appropriate message is displayed.  This motion is
;;;;   reversible, of course.
;;;;
;;;;   However, if point was already at the buffer boundary when the scrolling
;;;;   command was invoked, the command signals an appropriate error instead.
;;;;
;;;; + When the minibuffer window is the selected window, the new versions of
;;;;   scroll-up and scroll-down either scroll the minibuffer-scroll-window
;;;;   (which is usually the window of completions) or the next-window if there
;;;;   is no minibuffer-scroll-window.  This is usually much more useful than
;;;;   scrolling the minibuffer itself.
;;;;
;;;; + When a scrolling command is scrolling a window other than the selected
;;;;   window, it will signal an appropriate buffer boundary error if the
;;;;   window cannot be scrolled (because the appropriate buffer boundary is
;;;;   already visible).  This means that an error is signalled even in cases
;;;;   that would be allowed (by "straying" point or by moving it to the buffer
;;;;   boundary) if the window were selected.
;;;;
;;;;   (If an error were not signalled in these cases, then there would be many
;;;;   cases in which the last scroll in a particular direction would appear to
;;;;   do nothing because only the point position would change -- the displayed
;;;;   text would stay the same!  To avoid these cases the scrolling commands
;;;;   signal boundary errors "prematurely" when the window to be scrolled is
;;;;   not selected.)
;;;;
;;;; So how is this package different than Joe Wells' "scroll-fix" package?
;;;;
;;;; + "scroll-fix" behaves differently when the window is near a buffer
;;;;   boundary.  Instead of allowing point to stray, "scroll-fix" first does
;;;;   an incomplete scroll (i.e., moves point less than the full distance in
;;;;   order to keep point at the desired window position) and then pops point
;;;;   to the buffer boundary.  I think that the behavior of this package is
;;;;   somewhat move intuitive, especially for small scrolling distances.
;;;;
;;;; + The scrolling commands in this package will appropriately signal buffer
;;;;   boundary errors; the commands in "scroll-fix" never signal boundary
;;;;   errors.  This makes it difficult to allow "scroll-fix" to replace the
;;;;   standard scroll-down and scroll-up commands because some other packages
;;;;   (e.g., VM and GNUS) expect the scrolling commands to signal these errors
;;;;   as necessary.
;;;;
;;;; + This package keeps track of the set of "in place" scrolling commands
;;;;   dynamically, in order to detect "chains" of scrolling commands.
;;;;   "scroll-fix" has a fixed list of scrolling commands, so "scroll-fix"
;;;;   cannot keep track of some chains.  (Again, "scroll-fix" interacts badly
;;;;   with VM and GNUS.)  And because "scroll-fix" keeps a static list of
;;;;   scrolling commands, it is a bad idea to call its "in place" commands
;;;;   from a program.  This package, because it maintains the information
;;;;   dynamically, has no such problems.
;;;;
;;;; + This package handles long lines correctly.  (But see PROBLEMS, below.)
;;;;
;;;; + This package provides an "in place" version of the standard GNU Emacs
;;;;   command scroll-other-window (and a replacement for scroll-other-window,
;;;;   too).
;;;;
;;;; + This package will refuse to scroll non-selected windows (by signalling
;;;;   an error) when the displayed text would not change, as described in the
;;;;   feature list above.
;;;;
;;;; + When the minibuffer is selected, this package always scrolls a window
;;;;   other than the minibuffer.  "scroll-fix" will scroll another window only
;;;;   if the entire minibuffer contents are visible.
;;;;
;;;; + "scroll-fix" provides a command to toggle the "in place" behavior of the
;;;;   standard GNU Emacs commands.  This package doesn't; you'll have to set
;;;;   the option manually with set-variable.
;;;;
;;;; + This package has gratuitous variable renaming (insert smile here!):
;;;;
;;;;   "scroll-fix" user variable            Equivalent in this package
;;;;   -----------------------------------------------------------------------
;;;;   scroll-in-place                       (none)
;;;;   scroll-in-place-replace-original      scroll-in-place
;;;;   scroll-in-place-eob-blank-allowed     scroll-allow-blank-lines-past-eob
;;;;
;;;; + This package provides "in place" behavior for the standard GNU Emacs
;;;;   commands by default; "scroll-fix" does not.

;;;; COMMANDS AND FUNCTIONS
;;;;
;;;; This package provides the following "in place" versions of GNU Emacs'
;;;; standard vertical scrolling commands:
;;;;
;;;;   scroll-down-in-place
;;;;   scroll-up-in-place
;;;;   scroll-other-window-in-place
;;;;
;;;; The variable scroll-in-place, which is true by default, determines whether
;;;; or not the new versions of the standard GNU Emacs scrolling commands
;;;; (scroll-down, scroll-up, and scroll-other-window) use the "in place"
;;;; features listed above.  When scroll-in-place is nil, the standard GNU
;;;; Emacs scrolling commands simply call the original versions of themselves.
;;;;
;;;; NOTE that this package redefines the standard GNU Emacs commands scroll-
;;;; down, scroll-up, and scroll-other-window (in order to check the variable
;;;; scroll-in-place, as described above).
;;;;
;;;; This package also provides the following functions which are of use to
;;;; programmers:
;;;;
;;;;   scroll-window
;;;;   scroll-window-in-place
;;;;   scroll-window-in-place-continue-sequence
;;;;
;;;; scroll-window-in-place is the heart of the "in place" scrolling commands.
;;;; scroll-window is a function that checks the variable scroll-in-place and
;;;; calls the appropriate scrolling function (either scroll-window-in-place or
;;;; one of the original versions of scroll-down and scroll-up).  The function
;;;; scroll-window-in-place-continue-sequence is provided in order to preserve
;;;; running "chains" of scrolling commands as described above.

;;;; YOUR .EMACS FILE
;;;;
;;;; To use this package, you simply need to load it from within your ".emacs"
;;;; file:
;;;;
;;;;   (require 'scroll-in-place)
;;;;
;;;; By default, this package provides for the standard GNU Emacs vertical
;;;; scrolling commands (scroll-down, scroll-up, and scroll-other-window) to
;;;; use the "in place" features.  If you would rather not have this, set the
;;;; variable scroll-in-place to nil:
;;;;
;;;;   (setq scroll-in-place nil)
;;;;
;;;; When scroll-in-place is nil, you will have to bind keys in order to call
;;;; the "in place" scrolling commands.  For example, you might want to do the
;;;; following:
;;;;
;;;;   (global-set-key "\M-v" 'scroll-down-in-place)
;;;;   (global-set-key "\C-v" 'scroll-up-in-place)
;;;;
;;;; Sun users should also read the PROBLEMS section, below.

;;;; PROBLEMS
;;;;
;;;; + It is sometimes difficult for one's eyes to follow a partial scroll,
;;;;   especially when the scrolled window is not selected (and therefore that
;;;;   window's point is not highlighted).  One can lose one's place in the
;;;;   text.
;;;;
;;;; + The names scroll-down-in-place and scroll-up-in-place conflict with two
;;;;   commands in the GNU Emacs terminal-specific file "term/sun.el".  This
;;;;   means that in order to load this package correctly, Sunterm users will
;;;;   have to use the hook term-setup-hook.  For example, you might put the
;;;;   following form in your ".emacs" file:
;;;;
;;;;   (setq term-setup-hook (function (lambda () (require 'scroll-in-place))))
;;;;
;;;;   If this is confusing, get help from your local GNU Emacs guru.
;;;;
;;;; + scroll-determine-goal-column tries to honor the variable track-eol if it
;;;;   is set.  But when lines are being wrapped we can't move point past the
;;;;   wrap -- or else it is possible that scrolling won't work correctly.  In
;;;;   short, this package honors track-eol as best it can.
;;;;
;;;; + scroll-window-in-place can get confused when something changes the
;;;;   window "out from under it."  By "confused" I mean that it is possible
;;;;   for scroll-window-in-place to think that it should continue the running
;;;;   sequence of "in place" scrolls when it should really probably start a
;;;;   new sequence.  For example, if a process filter inserts text into the
;;;;   buffer and moves point, scroll-window-in-place loses track of where
;;;;   point should be and where the window should start.  Commands that call a
;;;;   "scroll in place" function and then subsequently move point can also
;;;;   confuse scroll-window-in-place.
;;;;
;;;;   scroll-window-in-place is not confused by VM 4.41's message scrolling
;;;;   commands, which do renarrowing when an end-of-buffer error is signalled.
;;;;   (I don't use VM 5.32 yet, but a quick look at the code doesn't reveal
;;;;   any obvious problems.)  scroll-window-in-place can be confused by GNUS
;;;;   3.13's and 3.14's article scrolling commands because they move point to
;;;;   the last line of the article window and then scroll the text.
;;;;
;;;;   To correct this confusion, scroll-window-in-place would have to keep
;;;;   track of the final positions of window-start and window-point, possibly
;;;;   with both markers and character positions.  In my experience the "in
;;;;   place" scrolling commands are almost never confused, so the extra sanity
;;;;   checking isn't worth the effort.  If your mileage varies, let me know.

(provide 'scroll-in-place)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Here are the variable declarations, both user options and internal
;;;; variables.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar scroll-in-place t
  "*When this variable is true (i.e., non-nil), the standard GNU Emacs vertical
scrolling commands scroll-down, scroll-up, and scroll-other-window will attempt
to keep point at its current position in the window (window line and column).
In other words, point stays \"in place\" within the window.

When this variable is nil, the standard GNU Emacs vertical scrolling commands
behave as usual.  The \"in place\" equivalents, however, are still available as
separate commands."
  ;; I have thought about dividing scroll-in-place into three variables: a list
  ;; of commands that always scroll in place, a list of commands that never
  ;; scroll in place, and a flag that determines the default behavior of other
  ;; scrolling commands.  This could make it easier to make "in place"
  ;; scrolling the default because one could single out certain ill-behaved
  ;; commands.  But as of now I'm sure that the added complexity would really
  ;; be worth it.
  )

(defvar scroll-allow-blank-lines-past-eob nil
  "*When this variable is nil, the \"in place\" scrolling commands will avoid
displaying empty lines past the end of the buffer text.  In other words, just
as you can't see \"dead space\" before the beginning of the buffer text, the
\"in place\" scrolling commands try to avoid displaying \"dead space\" past the
end of the buffer text.  This helps make the most of window real estate.

Note that sometimes it is necessary to display \"dead space\" in order to make
a previous scrolling action reversible.

When this variable is non-nil, the \"in place\" scrolling commands will always
allow blank lines to be shown past the end of the buffer.")

;;;;
;;;; The variables below this point are internal to this package.
;;;;

(defvar scroll-window-in-place-commands nil
  "The list of commands that call the function scroll-window-in-place.  This
list is kept so that scroll-window-in-place can detect sequences of consecutive
scrolling commands."
  ;; It is almost always sufficient to keep track of only the last command that
  ;; called scroll-window-in-place, but there's a significant exception.  When
  ;; a single invocation of a command calls scroll-window-in-place more than
  ;; once, we need to have more information about what's going on.
  )

(defvar scroll-initially-displayed-lines 0
  "The number of window lines that contained buffer text when the current
sequence of \"in place\" scrolling commands started.  Unless the variable
scroll-in-place-allow-blank-lines-past-eob is true, the \"in place\" scrolling
commands ensure that at least this many text lines are visible at all times.")

(defvar scroll-previous-window nil
  "The window that was most recently scrolled by an \"in place\" scrolling
command.")

(defvar scroll-previous-lines 0
  "The number of window lines that the previous \"in place\" scrolling command
attempted to scroll.")

(defvar scroll-goal-column 0
  "The desired horizontal window position for point, used by the \"in place\"
scrolling commands.")

(defvar scroll-boundary-previous-point nil
  "The value of point before point was moved to a buffer boundary.")

(defvar scroll-boundary-previous-lines 0
  "The number of lines that point moved when it moved to a buffer boundary.")

(defvar scroll-boundary-error-point nil
  "The value of point when an \"in place\" scrolling command signalled a buffer
boundary error.  This is used to decide how to subsequent scrolling commands
should recover from the error.")

(defvar scroll-window-debt 0
  "The difference between the number of lines an \"in place\" scrolling command
tried to scroll a window and the number of lines that the window actually
scrolled.  This difference is the \"debt\" in the window's starting position.
Subsequent \"in place\" scrolling commands try to make up this debt.")

(defconst scroll-pos-visible-bug-p
  ;; As of this writing, the most recent version of Epoch is 4.2.  This test
  ;; will probably have to be rewritten when a newer version of Epoch becomes
  ;; available.
  (and (boundp 'epoch::version)
       (let ((old-match-data (match-data)))
	 (unwind-protect
	     (if (string-match "\\`4\\." emacs-version) t nil)
	   (store-match-data old-match-data))))
  "A flag, set when this version of GNU Emacs has a buggy version of the
function pos-visible-in-window-p that returns nil when given (point-max) and
\(point-max) is on the last line of the window.  Currently, this flag is set
for all versions of Epoch 4.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Here are the "in place" scrolling commands (interactive functions) and the
;;;; replacements for the standard GNU Emacs vertical scrolling commands.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
;;;; But first, an auxiliary function.
;;;;

(defun scroll-choose-window ()
  "Choose the window to be scrolled by the \"in place\" scrolling commands
scroll-down-in-place and scroll-up-in-place.  When the variable scroll-in-place
is true, this function also selects the window for the commands scroll-down and
scroll-up.

The rules are simple.  If the selected window is not the minibuffer window,
then scroll the selected window.  When the minibuffer window is selected,
either scroll the minibuffer-scroll-window (if it exists) or scroll the next
window (otherwise).  The minibuffer-scroll-window is usually the window that
displays completions."
  (let ((selected-window (selected-window)))
    (if (eq selected-window (minibuffer-window))
	(if (and minibuffer-scroll-window
		 ;; window-point is nil if the window has been deleted.
		 (window-point minibuffer-scroll-window))
	    minibuffer-scroll-window
	  (next-window selected-window))
      selected-window)))

;;;;
;;;; Here are the new scroll-in-place commands.
;;;;

(defun scroll-down-in-place (&optional lines)
  "Scroll the text of the current window downward by ARG lines, leaving point
as close as possible to its current window position (window line and column).
In other words, point is left \"in place\" within the window.  If ARG is nil,
scroll the window by the same amount it was moved by the immediately previous
\"in place\" scrolling command, or by almost a complete windowful if the
previous command was not an \"in place\" scrolling command (or when that
previous command scrolled some other window).

If the window cannot be scrolled by the full distance, point is allowed to
stray from its initial position so that it can move the full number of lines.
If point cannot move the full number of lines, point is moved to the buffer
boundary.  Any immediately subsequent \"in place\" scrolling commands will try
to restore point to its initial window position.

As a special case, when the current window is the minibuffer window, this
command scrolls the minibuffer-scroll-window (which is usually the list of
completions) if it exists, or otherwise the next window in the canonical
ordering of windows."
  (interactive "P")
  (scroll-window-in-place (scroll-choose-window) lines -1))

;;;
;;;
;;;

(defun scroll-up-in-place (&optional lines)
  "Scroll the text of the current window upward by ARG lines, leaving point as
close as possible to its current window position (window line and column).  In
other words, point is left \"in place\" within the window.  If ARG is nil,
scroll the window by the same amount it was moved by the immediately previous
\"in place\" scrolling command, or by almost a complete windowful if the
previous command was not an \"in place\" scrolling command (or when that
previous command scrolled some other window).

If the window cannot be scrolled by the full distance, point is allowed to
stray from its initial position so that it can move the full number of lines.
If point cannot move the full number of lines, point is moved to the buffer
boundary.  Any immediately subsequent \"in place\" scrolling commands will try
to restore point to its initial window position.

As a special case, when the current window is the minibuffer window, this
command scrolls the minibuffer-scroll-window (which is usually the list of
completions) if it exists, or otherwise the next window in the canonical
ordering of windows."
  (interactive "P")
  (scroll-window-in-place (scroll-choose-window) lines 1))

;;;
;;;
;;;

(defun scroll-other-window-in-place (&optional lines)
  "Scroll the text of the next window upward by ARG lines, leaving point in
that window as close as possible to its current window position (window line
and column).  In other words, point is left \"in place\" within the window.
The next window is the one below the current one, or the one at the top of the
screen if the current window is at the bottom of the screen.

If ARG is nil, scroll the window by the amount it was moved by the immediately
previous \"in place\" scrolling command, or by almost a complete windowful if
the previous command was not an \"in place\" scrolling command (or when that
previous command scrolled some other window).

If the window cannot be scrolled by the full distance, point is allowed to
stray from its initial position so that it can move the full number of lines.
If point cannot move the full number of lines, point is moved to the buffer
boundary.  Any immediately subsequent \"in place\" scrolling commands will try
to restore point to its initial window position.

If it is impossible to scroll the text of the window at all (because a buffer
boundary is already visible), this command signals a buffer boundary error.
The error is signalled even if point could otherwise move the full number of
lines."
  (interactive "P")
  (let* ((selected-window (selected-window))
	 (other-window (if (and (eq selected-window (minibuffer-window))
				minibuffer-scroll-window
				;; window-point is nil if the window has been
				;; deleted.
				(window-point minibuffer-scroll-window))
			   minibuffer-scroll-window
			 (next-window selected-window))))
    (if (eq selected-window other-window)
	(error "There is no other window."))
    (scroll-window-in-place other-window lines 1)))

;;;;
;;;; Here are the replacements for GNU Emacs' standard vertical scrolling
;;;; commands.
;;;;

(or (fboundp 'original-scroll-down)
    (fset 'original-scroll-down (symbol-function 'scroll-down)))
(or (fboundp 'original-scroll-up)
    (fset 'original-scroll-up (symbol-function 'scroll-up)))
(or (fboundp 'original-scroll-other-window)
    (fset 'original-scroll-other-window (symbol-function 'scroll-other-window))
    )

;;;
;;;
;;;

(defun scroll-down (&optional lines)
  "Scroll the text of the current window downward by ARG lines.

When the variable scroll-in-place is true, this command invokes the command
scroll-down-in-place to scroll the current window and leave point \"in place\"
within the window.  See the documentation for scroll-down-in-place for more
information.

When the variable scroll-in-place is nil, this command invokes the standard GNU
Emacs version of scroll-down.  In that case, when ARG is nil the current window
is scrolled by nearly a complete windowful of text."
  (interactive "P")
  (if scroll-in-place
      (scroll-down-in-place lines)
    ;; Paranoid, we forcibly break any running sequence of "in place" scrolls
    ;; in the selected window.
    (if (eq (selected-window) scroll-previous-window)
	(setq scroll-previous-window nil))
    (original-scroll-down lines)))

;;;
;;;
;;;

(defun scroll-up (&optional lines)
  "Scroll the text of the current window upward by ARG lines.

When the variable scroll-in-place is true, this command invokes the command
scroll-up-in-place to scroll the current window and leave point \"in place\"
within the window.  See the documentation for scroll-up-in-place for more
information.

When the variable scroll-in-place is nil, this command invokes the standard GNU
Emacs version of scroll-up.  In that case, when ARG is nil the current window
is scrolled by nearly a complete windowful of text."
  (interactive "P")
  (if scroll-in-place
      (scroll-up-in-place lines)
    ;; Paranoid, we forcibly break any running sequence of "in place" scrolls
    ;; in the selected window.
    (if (eq (selected-window) scroll-previous-window)
	(setq scroll-previous-window nil))
    (original-scroll-up lines)))

;;;
;;;
;;;

(defun scroll-other-window (&optional lines)
  "Scroll the text of the next window upward by ARG lines.  The next window is
the one below the current one, or the one at the top of the screen if the
current window is at the bottom of the screen.

When the variable scroll-in-place is true, this command invokes the command
scroll-other-window-in-place to scroll the next window and leave point \"in
place\" within that window.  See the documentation for scroll-other-window-in-
place for more information.

When the variable scroll-in-place is nil, this command invokes the standard GNU
Emacs version of scroll-other-window.  In that case, when ARG is nil the next
window is scrolled by nearly a complete windowful of text."
  (interactive "P")
  (if scroll-in-place
      (scroll-other-window-in-place lines)
    ;; Paranoid, we forcibly break any running sequence of "in place" scrolls
    ;; in the other window.
    (let* ((selected-window (selected-window))
	   (other-window (if (and (eq selected-window (minibuffer-window))
				  minibuffer-scroll-window
				  ;; window-point is nil if the window has been
				  ;; deleted.
				  (window-point minibuffer-scroll-window))
			     minibuffer-scroll-window
			   (next-window selected-window))))
      (if (and (not (eq other-window selected-window))
	       (eq other-window scroll-previous-window))
	  (setq scroll-previous-window nil)))
    (original-scroll-other-window lines)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Here are the new functions scroll-window-in-place, scroll-window, and
;;;; scroll-window-in-place-continue-sequence.  These functions are intended to
;;;; be available to programs outside this package.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scroll-window-in-place (window lines direction)
  "Scroll WINDOW vertically by the given number of window LINES in the given
DIRECTION, leaving the window's point as close as possible to its original
window position (window line and column).  In other words, the window's point
is left \"in place\" within the window.

Note that the window to be scrolled does not have to be the selected window,
and that this function does not change which window is selected.

LINES specifies the number of window lines to scroll and is interpreted as if
it were a raw prefix argument.  If LINES is nil, the window is scrolled by the
amount it was moved by the previous \"in place\" scrolling command, or by
almost a complete windowful if the previous command was not an \"in place\"
scrolling command (or when WINDOW is not the previously scrolled window).  If
LINES is the symbol -, then DIRECTION is reversed and LINES is treated as if it
were nil.

DIRECTION determines the direction of the scrolling motion.  The values -1 and
'down indicate downward motion; the values 1 and 'up indicate upward motion.
Any other value causes an error.

If the window cannot be scrolled by the full distance (because the window hits
the boundary of its buffer), the window's point is allowed to stray from its
initial position so that it can move the full number of lines.  If point cannot
move the full number of lines, point is moved to the buffer boundary (unless it
was already there, in which case a buffer boundary error is signalled).  Any
immediately subsequent \"in place\" scrolling commands will try to restore
point to its initial window position.

Unless the variable scroll-allow-blank-lines-past-eob is true, this function
avoids displaying blank lines past the end of the buffer except as necessary to
make a previous \"in place\" scrolling action reversible.  Effectively, this
means that this function will not display any more past-end-of-buffer blank
lines than were visible when the current sequence of \"in place\" scrolling
commands started.  When the variable scroll-allow-blank-lines-past-eob is true,
this function will display as many blank lines as is necessary to keep point
\"in place\" in the window.

Note that if WINDOW is not the selected window and it is impossible to scroll
the text of WINDOW at all (because a buffer boundary is already visible), then
this function signals a buffer boundary error.  The error is signalled even if
point could otherwise move the full number of lines."
  (let* (;; Make sure that the user doesn't quit in the middle and leave us
	 ;; with our variables out of sync.
	 (inhibit-quit t)
	 (original-window (selected-window))
	 (original-buffer (current-buffer))
	 (window-height (- (window-height window)
			   (if (eq window (minibuffer-window)) 0 1)))
	 (continue-scroll-p
	  (and ;; We're scrolling the previously scrolled window...
	       (windowp scroll-previous-window)
	       (eq window scroll-previous-window)
	       ;; ...and the last command was an "in place" scrolling command.
	       (or (memq last-command scroll-window-in-place-commands)
		   ;; If the previous command signalled an error, last-command
		   ;; is t.  Try to see if we signalled the error and if point
		   ;; is where we left it.
		   (and (eq last-command t)
			scroll-boundary-error-point
			(eq (window-point window) scroll-boundary-error-point))
		   )))
	 )
    
    ;; Record the current command in our list of "in place" scrolling commands.
    (or (memq this-command scroll-window-in-place-commands)
	(setq scroll-window-in-place-commands
	      (cons this-command scroll-window-in-place-commands)))
    
    ;; Parse the direction into a unit distance (1 or -1).  If lines is -,
    ;; treat it as though we received an argument of nil and the opposite
    ;; direction.
    (setq direction (scroll-parse-direction direction))
    (if (eq lines '-)
	(setq lines nil
	      direction (- direction)))
    
    (setq scroll-previous-window window
	  scroll-boundary-error-point nil)
    (unwind-protect
	(progn
	  ;; select-window does an implicit set-buffer.
	  (select-window window)
	  
	  (if (or ;; We were given an explicit number of lines to scroll...
	          (numberp lines) (consp lines)
		  ;; ...or this is not a continuation of a running sequence of
		  ;; "in place" scrolling commands...
		  (not continue-scroll-p)
		  ;; ...or the last successful scrolling command moved to a
		  ;; buffer boundary, but the buffer is no longer in the state
		  ;; we left it.  (This can occur if, for example, we signal an
		  ;; end-of-buffer error and something catches it and moves
		  ;; point or renarrows.  VM, for example, does this.)
		  (and scroll-boundary-previous-point
		       (or (not (or (bobp) (eobp)))
			   (< scroll-boundary-previous-point (point-min))
			   (> scroll-boundary-previous-point (point-max))
			   (eq scroll-boundary-previous-point (point)))))
	      
	      ;; We're starting a new sequence of scrolling commands.
	      (setq lines (if (or (numberp lines) (consp lines))
			      (prefix-numeric-value lines)
			    ;; The default number of lines...
			    (max (- window-height next-screen-context-lines)
				 1))
		    scroll-previous-lines lines
		    scroll-goal-column (scroll-determine-goal-column window)
		    scroll-boundary-previous-point nil
		    ;; scroll-boundary-previous-lines 0 not necessary.
		    scroll-window-debt 0
		    scroll-initially-displayed-lines
		    (if scroll-allow-blank-lines-past-eob
			0
		      (save-excursion
			(goto-char (window-start window))
			(vertical-motion (1- window-height)))))
	    
	    ;; Otherwise we want to scroll by the same amount we scrolled in
	    ;; previous invocations of this function.
	    (setq lines scroll-previous-lines))
	  
	  (setq lines (* direction lines))
	  
	  ;; If point is not in the window, center window around point.  We try
	  ;; to account for a bug in pos-visible-in-window-p in some versions
	  ;; of Emacs (namely, Epoch 4.x).
	  (save-excursion
	    (if (pos-visible-in-window-p (let ((point (point)))
					   (if (and scroll-pos-visible-bug-p
						    (= point (point-max)))
					       (max (1- point) (point-min))
					     point))
					 window)
		nil
	      (vertical-motion (/ (- window-height) 2))
	      (set-window-start window (point))))
	  
	  (cond ((and scroll-boundary-previous-point
		      ;; lines is the same sign as the direction from point to
		      ;; the scroll-boundary-previous-point.
		      (cond ((> lines 0)
			     (> (- scroll-boundary-previous-point (point)) 0))
			    ((< lines 0)
			     (< (- scroll-boundary-previous-point (point)) 0))
			    (t nil)))
		 ;; We're moving away from the buffer boundary.
		 (goto-char scroll-boundary-previous-point)
		 ;; Always move here (i.e., don't reject cases in which the
		 ;; window doesn't move).
		 (scroll-set-window-start window
					  (- scroll-boundary-previous-lines))
		 ;; (message "Back, window debt is %s." scroll-window-debt)
		 (setq scroll-boundary-previous-point nil))

		((= lines 0)
		 ;; We're going nowhere, so save ourselves some work.
		 ;; (message "Scrolled zero lines.")
		 )
		
		(t
		 ;; Perform the scrolling motion.
		 (let ((initial-point (point))
		       (moved nil))
		   ;; First move point and see how far it goes.
		   (setq moved (vertical-motion lines))
		   (if (= moved lines)
		       (progn
			 ;; Point moved the full distance.  Move to the desired
			 ;; column and then try to move the window the full
			 ;; distance, too.
			 (move-to-column (+ (current-column)
					    scroll-goal-column))
			 (or (scroll-set-window-start window moved
						      original-window)
			     (scroll-signal-boundary-error initial-point
							   lines))
			 ;; (message "Normal, window debt is %s."
			 ;;          scroll-window-debt)
			 )
		     ;; Point couldn't move all the way.  Move to the buffer
		     ;; boundary if we're not already there, or signal a buffer
		     ;; boundary error otherwise.
		     (let ((boundary-point (if (< lines 0)
					       (point-min)
					     (point-max)))
			   (boundary-symbol (if (< lines 0)
						'beginning-of-buffer
					      'end-of-buffer)))
		       (if (= initial-point boundary-point)
			   (scroll-signal-boundary-error initial-point lines)
			 ;; Scroll the window by as many lines as point could
			 ;; move.
			 (or (scroll-set-window-start window moved
						      original-window)
			     (scroll-signal-boundary-error initial-point
							   lines))
			 (message "%s" (get boundary-symbol 'error-message))
			 ;; (message "Boundary, window debt is %s."
			 ;;          scroll-window-debt)
			 (setq scroll-boundary-previous-lines moved)
			 (setq scroll-boundary-previous-point initial-point)
			 (goto-char boundary-point))
		       )))
		 )))
      
      ;; The unwind forms of the unwind-protect, above.  Restore the originally
      ;; selected window and current buffer.
      (select-window original-window)
      (set-buffer original-buffer)))
  
  ;; The standard GNU Emacs scrolling commands return nil, so we do, too.
  nil)

;;;
;;;
;;;

(defun scroll-window (window lines direction)
  "Scroll WINDOW vertically by the given number of window LINES in the given
DIRECTION.  Note that the window to be scrolled does not have to be the
selected window, and that this function does not change which window is
selected.

When the variable scroll-in-place is true, this function simply invokes the
function scroll-window-in-place to scroll the window and leave point \"in
place\" within that window.  See the documentation for scroll-window-in-place
for more information.

When the variable scroll-in-place is nil, this function invokes the original
version of the standard GNU Emacs command scroll-down or scroll-up, as
determined by DIRECTION, to scroll the window.  If DIRECTION is -1 or 'down,
scroll-down is called; if DIRECTION is 1 or 'up, scroll-up is called.  Any
other DIRECTION is an error.  LINES is interpreted as if it were a raw prefix
argument.  If LINES is nil, the window is scrolled by almost a complete
windowful."
  (if scroll-in-place
      (scroll-window-in-place window lines direction)
    (let ((current-buffer (current-buffer))
	  (selected-window (selected-window)))
      (unwind-protect
	  (progn
	    (select-window window)
	    (if (= (scroll-parse-direction direction) 1)
		(original-scroll-up lines)
	      (original-scroll-down lines)))
	(select-window selected-window)
	(set-buffer current-buffer))
      )))

;;;
;;; The following function is sometimes useful.  For example, I call it from
;;; functions that are invoked by certain mouse button down events in order to
;;; preserve any running chain of "in place" scrolling commands.  This lets me
;;; continue the sequence from my mouse button up functions.
;;;
;;; I haven't yet needed a function to purposely break a running sequence of
;;; "in place" scrolling commands.  Such a function would be easy to write,
;;; however; just set scroll-previous-window to nil.
;;;

(defun scroll-window-in-place-continue-sequence ()
  "If the previous command was a \"scroll in place\" command, set this-command
to the name of that previous command.  This ensures that any running sequence
of \"in place\" scrolling commands will not be broken by the current command.
See the documentation for the commands scroll-down-in-place and scroll-down-in-
place for more information about \"in place\" scrolling.

NOTE that you don't need to call this function if the current command scrolls
in place!  You only need to call this function when the current command is not
a \"scroll in place\" command but you still want to preserve any running
sequence of \"in place\" commands.  Such situations are rare.

NOTE that this function sets this-command in order to trick the \"in place\"
scrolling commands.  If something else subsequently sets this-command, any
running sequence of scrolling commands will probably be broken anyway."
  (if (or (memq last-command scroll-window-in-place-commands)
	  ;; If the previous command signalled an error, last-command is t.
	  (and (eq last-command t)
	       scroll-boundary-error-point))
      (setq this-command last-command)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Here are the various auxiliary functions called by scroll-window-in-place.
;;;; None of the functions are intended to be called from outside this package.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scroll-parse-direction (direction)
  "Return the signed unit distance for the given DIRECTION.  If DIRECTION is
unacceptable, signal an error."
  (cond ((or (eq direction 1) (eq direction -1)) direction)
	((eq direction 'up) 1)
	((eq direction 'down) -1)
	(t (signal 'args-out-of-range (list 'direction direction)))
	))

;;;
;;;
;;;

(defun scroll-determine-goal-column (window)
  "Return the goal column for the \"in place\" vertical scrolling commands.
This is the horizontal window position at which these commands try to keep
point."
  (cond ((or truncate-lines
	     (and truncate-partial-width-windows
		  (< (window-width window) (screen-width)))
	     (> (window-hscroll window) 0))
	 ;; Lines in this window are being truncated.
	 (if (and track-eol (eolp))
	     9999
	   (current-column)))
	((and track-eol (eolp))
	 ;; In some ways this isn't quite right, as point doesn't track the
	 ;; ends of wrapped lines.  But if it did so, point would be on the
	 ;; wrong window line.  This is the best we can do.
	 (1- (window-width window)))
	(t (% (current-column) (1- (window-width window))))
	))

;;;
;;;
;;;

(defun scroll-set-window-start (window lines &optional original-window)
  "Move the window-start of the given window, which must be the selected
window.  If the window was successfully scrolled, update the scroll-window-debt
and return t.  Otherwise return nil.

This function is an auxiliary function for the function scroll-window-in-place.
Don't call this function from other code."
  (save-excursion
    (goto-char (window-start window))
    ;; Try to move the window start by the specified number of lines.  In
    ;; addition, try to make up any existing debt in the window start's
    ;; position and make sure that we don't move too close to the end of the
    ;; buffer.
    (let ((moved (+ (vertical-motion (+ lines
					scroll-window-debt
					scroll-initially-displayed-lines))
		    (vertical-motion (- scroll-initially-displayed-lines)))))
      ;; If we're not scrolling the original-window (i.e., the originally
      ;; selected window), punt if we didn't move the window start at all.
      (if (and original-window
	       (not (eq window original-window))
	       (= moved 0))
	  nil
	;; Otherwise update the window start and keep track of the debt in our
	;; position.  Return t to indicate success.
	(set-window-start window (point))
	(setq scroll-window-debt (- (+ lines scroll-window-debt) moved))
	t))
    ))

;;;
;;;
;;;

(defun scroll-signal-boundary-error (initial-point lines)
  "Move point to its initial location and signal an appropriate buffer boundary
error.  This function is an auxiliary function for the function scroll-window-
in-place.  Don't call this function from other code."
  (goto-char initial-point)
  ;; Remember where point was when we signalled the error so that subsequent
  ;; "in place" scrolling commands can decide how to recover.
  (setq scroll-boundary-error-point initial-point)
  (signal (if (< lines 0) 'beginning-of-buffer 'end-of-buffer)
	  nil))

;; End of file.
