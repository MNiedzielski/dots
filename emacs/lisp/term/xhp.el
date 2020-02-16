;;
;;	HP function key rebindings for X11
;;	4-25-89		Paul Raveling
;;	11-11-89	Mark Reichert - added \e* for distinguishability.
;;				      - enumerated all shifts, stls, & metas
;;

;;	The next section, which swaps control-H and ascii del, is
;;	a verbatim copy of the original term/bobcat.el.  All other
;;	definitions and logic is

;;; HP terminals usually encourage using ^H as the rubout character

;;(let ((the-table (make-string 128 0)))
;;  (let ((i 0))
;;    (while (< i 128)
;;      (aset the-table i i)
;;      (setq i (1+ i))))
;;  ;; Swap ^H and DEL
;;  (aset the-table ?\177 ?\^h)
;;  (aset the-table ?\^h ?\177)
;;  (setq keyboard-translate-table the-table))


( if (not (fboundp 'x-rebind-keysym))
  ( defun x-rebind-keysym (a b c)
    "You don't have x-rebind-keysym - Sorry."
    (interactive)
    nil
  )
)

;;;;	Bind keysyms to escape sequences consistent with
;;;;	the distributed version of emacs

; Arrow  Keys
(x-rebind-keysym "Left"		"none"	"\e*Left")
(x-rebind-keysym "Left"		"shift"	"\e*Shift_Left")
(x-rebind-keysym "Left"		"ctl"	"\e*Ctl_Left")
(x-rebind-keysym "Left"		"meta"	"\e*Meta_Left")

(x-rebind-keysym "Right"	"none"	"\e*Right")
(x-rebind-keysym "Right"	"shift"	"\e*Shift_Right")
(x-rebind-keysym "Right"	"ctl"	"\e*Ctl_Right")
(x-rebind-keysym "Right"	"meta"	"\e*Meta_Right")

(x-rebind-keysym "Up"		"none"	"\e*Up")
(x-rebind-keysym "Up"		"shift"	"\e*Shift_Up")
(x-rebind-keysym "Up"		"ctl"	"\e*Ctl_Up")
(x-rebind-keysym "Up"		"meta"	"\e*Meta_Up")

(x-rebind-keysym "Down"		"none"	"\e*Down")
(x-rebind-keysym "Down"		"shift"	"\e*Shift_Down")
(x-rebind-keysym "Down"		"ctl"	"\e*Ctl_Down")
(x-rebind-keysym "Down"		"meta"	"\e*Meta_Down")

(x-rebind-keysym "F1"		"none"	"\e*F1")
(x-rebind-keysym "F1"		"shift"	"\e*Shift_F1")
(x-rebind-keysym "F1"		"ctl"	"\e*Ctl_F1")
(x-rebind-keysym "F1"		"meta"	"\e*Meta_F1")

(x-rebind-keysym "F2"		"none"	"\e*F2")
(x-rebind-keysym "F2"		"shift"	"\e*Shift_F2")
(x-rebind-keysym "F2"		"ctl"	"\e*Ctl_F2")
(x-rebind-keysym "F2"		"meta"	"\e*Meta_F2")

(x-rebind-keysym "F3"		"none"	"\e*F3")
(x-rebind-keysym "F3"		"shift"	"\e*Shift_F3")
(x-rebind-keysym "F3"		"ctl"	"\e*Ctl_F3")
(x-rebind-keysym "F3"		"meta"	"\e*Meta_F3")

(x-rebind-keysym "F4"		"none"	"\e*F4")
(x-rebind-keysym "F4"		"shift"	"\e*Shift_F4")
(x-rebind-keysym "F4"		"ctl"	"\e*Ctl_F4")
(x-rebind-keysym "F4"		"meta"	"\e*Meta_F4")

(x-rebind-keysym "F5"		"none"	"\e*F5")
(x-rebind-keysym "F5"		"shift"	"\e*Shift_F5")
(x-rebind-keysym "F5"		"ctl"	"\e*Ctl_F5")
(x-rebind-keysym "F5"		"meta"	"\e*Meta_F5")

(x-rebind-keysym "F6"		"none"	"\e*F6")
(x-rebind-keysym "F6"		"shift"	"\e*Shift_F6")
(x-rebind-keysym "F6"		"ctl"	"\e*Ctl_F6")
(x-rebind-keysym "F6"		"meta"	"\e*Meta_F6")

(x-rebind-keysym "F7"		"none"	"\e*F7")
(x-rebind-keysym "F7"		"shift"	"\e*Shift_F7")
(x-rebind-keysym "F7"		"ctl"	"\e*Ctl_F7")
(x-rebind-keysym "F7"		"meta"	"\e*Meta_F7")

(x-rebind-keysym "F8"		"none"	"\e*F8")
(x-rebind-keysym "F8"		"shift"	"\e*Shift_F8")
(x-rebind-keysym "F8"		"ctl"	"\e*Ctl_F8")
(x-rebind-keysym "F8"		"meta"	"\e*Meta_F8")

(x-rebind-keysym "F9"		"none"	"\e*F9")
(x-rebind-keysym "F9"		"shift"	"\e*Shift_F9")
(x-rebind-keysym "F9"		"ctl"	"\e*Ctl_F9")
(x-rebind-keysym "F9"		"meta"	"\e*Meta_F9")

(x-rebind-keysym "F10"		"none"	"\e*F10")
(x-rebind-keysym "F10"		"shift"	"\e*Shift_F10")
(x-rebind-keysym "F10"		"ctl"	"\e*Ctl_F10")
(x-rebind-keysym "F10"		"meta"	"\e*Meta_F10")

(x-rebind-keysym "F11"		"none"	"\e*F11")
(x-rebind-keysym "F11"		"shift"	"\e*Shift_F11")
(x-rebind-keysym "F11"		"ctl"	"\e*Ctl_F11")
(x-rebind-keysym "F11"		"meta"	"\e*Meta_F11")

(x-rebind-keysym "F12"		"none"	"\e*F12")
(x-rebind-keysym "F12"		"shift"	"\e*Shift_F12")
(x-rebind-keysym "F12"		"ctl"	"\e*Ctl_F12")
(x-rebind-keysym "F12"		"meta"	"\e*Meta_F12")

(x-rebind-keysym "F13"		"none"	"\e*F13")
(x-rebind-keysym "F13"		"shift"	"\e*Shift_F13")
(x-rebind-keysym "F13"		"ctl"	"\e*Ctl_F13")
(x-rebind-keysym "F13"		"meta"	"\e*Meta_F13")

(x-rebind-keysym "F14"		"none"	"\e*F14")
(x-rebind-keysym "F14"		"shift"	"\e*Shift_F14")
(x-rebind-keysym "F14"		"ctl"	"\e*Ctl_F14")
(x-rebind-keysym "F14"		"meta"	"\e*Meta_F14")

(x-rebind-keysym "F15"		"none"	"\e*F15")
(x-rebind-keysym "F15"		"shift"	"\e*Shift_F15")
(x-rebind-keysym "F15"		"ctl"	"\e*Ctl_F15")
(x-rebind-keysym "F15"		"meta"	"\e*Meta_F15")

(x-rebind-keysym "Help"		"none"	"\e*Help")
(x-rebind-keysym "Help"		"shift"	"\e*Shift_Help")
(x-rebind-keysym "Help"		"ctl"	"\e*Ctl_Help")
(x-rebind-keysym "Help"		"meta"	"\e*Meta_Help")

(x-rebind-keysym "F16"		"none"	"\e*F16")
(x-rebind-keysym "F16"		"shift"	"\e*Shift_F16")
(x-rebind-keysym "F16"		"ctl"	"\e*Ctl_F16")
(x-rebind-keysym "F16"		"meta"	"\e*Meta_F16")

(x-rebind-keysym "Menu"		"none"	"\e*Menu")
(x-rebind-keysym "Menu"		"shift"	"\e*Shift_Menu")
(x-rebind-keysym "Menu"		"ctl"	"\e*Ctl_Menu")
(x-rebind-keysym "Menu"		"meta"	"\e*Meta_Menu")

(x-rebind-keysym "F17"		"none"	"\e*F17")
(x-rebind-keysym "F17"		"shift"	"\e*Shift_F17")
(x-rebind-keysym "F17"		"ctl"	"\e*Ctl_F17")
(x-rebind-keysym "F17"		"meta"	"\e*Meta_F17")

(x-rebind-keysym "F18"		"none"	"\e*F18")
(x-rebind-keysym "F18"		"shift"	"\e*Shift_F18")
(x-rebind-keysym "F18"		"ctl"	"\e*Ctl_F18")
(x-rebind-keysym "F18"		"meta"	"\e*Meta_F18")

(x-rebind-keysym "F19"		"none"	"\e*F19")
(x-rebind-keysym "F19"		"shift"	"\e*Shift_F19")
(x-rebind-keysym "F19"		"ctl"	"\e*Ctl_F19")
(x-rebind-keysym "F19"		"meta"	"\e*Meta_F19")

(x-rebind-keysym "F20"		"none"	"\e*F20")
(x-rebind-keysym "F20"		"shift"	"\e*Shift_F20")
(x-rebind-keysym "F20"		"ctl"	"\e*Ctl_F20")
(x-rebind-keysym "F20"		"meta"	"\e*Meta_F20")


(x-rebind-keysym "Find"		"none"	"\e*Find")
(x-rebind-keysym "Find"		"shift"	"\e*Shift_Find")
(x-rebind-keysym "Find"		"ctl"	"\e*Ctl_Find")
(x-rebind-keysym "Find"		"meta"	"\e*Meta_Find")

(x-rebind-keysym "Insert"	"none"	"\e*Insert")
(x-rebind-keysym "Insert"	"shift"	"\e*Shift_Insert")
(x-rebind-keysym "Insert"	"ctl"	"\e*Ctl_Insert")
(x-rebind-keysym "Insert"	"meta"	"\e*Meta_Insert")

;;(x-rebind-keysym "Delete"	"none"	"\e*Delete_DEL")
;;(x-rebind-keysym "Delete"	"shift"	"\e*Shift_Delete_DEL")
;;(x-rebind-keysym "Delete"	"ctl"	"\e*Ctl_Delete_DEL")
;;(x-rebind-keysym "Delete"	"meta"	"\e*Meta_Delete_DEL")

(x-rebind-keysym "Select"	"none"	"\e*Select")
(x-rebind-keysym "Select"	"shift"	"\e*Shift_Select")
(x-rebind-keysym "Select"	"ctl"	"\e*Ctl_Select")
(x-rebind-keysym "Select"	"meta"	"\e*Meta_Select")

(x-rebind-keysym "Prior"	"none"	"\e*Prior")
(x-rebind-keysym "Prior"	"shift"	"\e*Shift_Prior")
(x-rebind-keysym "Prior"	"ctl"	"\e*Ctl_Prior")
(x-rebind-keysym "Prior"	"meta"	"\e*Meta_Prior")

(x-rebind-keysym "Next"		"none"	"\e*Next")
(x-rebind-keysym "Next"		"shift"	"\e*Shift_Next")
(x-rebind-keysym "Next"		"ctl"	"\e*Ctl_Next")
(x-rebind-keysym "Next"		"meta"	"\e*Meta_Next")


;;;;	Bind keysyms unused by distributed version of emacs
;;;;	but used on HP keyboards to escape sequences


;;(x-rebind-keysym "Tab"  	"none"	"\e*Tab")
(x-rebind-keysym "Tab_backward"	nil	"\e*Shift_Tab")
(x-rebind-keysym "Tab"		"ctl"	"\e*Ctl_Tab")
;;(x-rebind-keysym "Tab"  	"meta"	"\e*Meta_Tab")

(x-rebind-keysym "Execute"	"none"	"\e*Enter")		; Actually, enter
(x-rebind-keysym "Print"	nil	"\e*Shift_enter")
(x-rebind-keysym "Execute"	"ctl"	"\e*Ctl_Enter")
(x-rebind-keysym "Execute"	"meta"	"\e*Meta_Enter")

(x-rebind-keysym "Break"	"none"	"\e*Break")
(x-rebind-keysym "Reset"	nil	"\e*Shift_Break")
(x-rebind-keysym "Break"	"ctl"	"\e*Ctl_Break")
(x-rebind-keysym "Break"	"meta"	"\e*Meta_Break")

(x-rebind-keysym "Cancel"	"none"	"\e*Cancel")
(x-rebind-keysym "Cancel"	"shift"	"\e*Shift_Cancel")
(x-rebind-keysym "Cancel"	"ctl"	"\e*Ctl_Cancel")
(x-rebind-keysym "Cancel"	"meta"	"\e*Meta_Cancel")

(x-rebind-keysym "System"	"none"	"\e*System")
(x-rebind-keysym "User"		nil	"\e*Shift_System")
(x-rebind-keysym "System"	"ctl"	"\e*Ctl_System")
(x-rebind-keysym "System"	"meta"	"\e*Meta_System")

(x-rebind-keysym "Clear_line"	"none"	"\e*Clear_line")
(x-rebind-keysym "Clear_line"	"shift"	"\e*Shift_Clear_line")
(x-rebind-keysym "Clear_line"	"ctl"	"\e*Ctl_Clear_line")
(x-rebind-keysym "Clear_line"	"meta"	"\e*Meta_Clear_line")

(x-rebind-keysym "Clear"	"none"	"\e*Clear_display")		; ***
(x-rebind-keysym "Clear"	"shift"	"\e*Shift_Clear_display")	; ***
(x-rebind-keysym "Clear"	"ctl"	"\e*Ctl_Clear_display")		; ***
(x-rebind-keysym "Clear"	"meta"	"\e*Meta_Clear_display")	; ***

(x-rebind-keysym "Insert_line"	"none"	"\e*Insert_line")
(x-rebind-keysym "Insert_line"	"shift"	"\e*Shift_Insert_line")
(x-rebind-keysym "Insert_line"	"ctl"	"\e*Ctl_Insert_line")
(x-rebind-keysym "Insert_line"	"meta"	"\e*Meta_Insert_line")

(x-rebind-keysym "Delete_line"	"none"	"\e*Delete_line")
(x-rebind-keysym "Delete_line"	"shift"	"\e*Shift_Delete_line")
(x-rebind-keysym "Delete_line"	"ctl"	"\e*Ctl_Delete_line")
(x-rebind-keysym "Delete_line"	"meta"	"\e*Meta_Delete_line")

(x-rebind-keysym "Insert_char"	"none"	"\e*Insert_char")
(x-rebind-keysym "Insert_char"	"shift"	"\e*Shift_Insert_char")
(x-rebind-keysym "Insert_char"	"ctl"	"\e*Ctl_Insert_char")
(x-rebind-keysym "Insert_char"	"meta"	"\e*Meta_Insert_char")

(x-rebind-keysym "Delete_char"	"none"	"\e*Delete_char")
(x-rebind-keysym "Delete_char"	"shift"	"\e*Shift_Delete_char")
(x-rebind-keysym "Delete_char"	"ctl"	"\e*Ctl_Delete_char")
(x-rebind-keysym "Delete_char"	"meta"	"\e*Meta_Delete_char")

(x-rebind-keysym "Home"		"none"	"\e*Home")
(x-rebind-keysym "Home"		"shift"	"\e*Shift_Home")
(x-rebind-keysym "Home"		"ctl"	"\e*Ctl_Home")
(x-rebind-keysym "Home"		"meta"	"\e*Meta_Home")


(x-rebind-keysym "KP_F1"	"none"	"\e*KP_F1")
(x-rebind-keysym "KP_F1"	"shift"	"\e*Shift_KP_F1")
(x-rebind-keysym "KP_F1"	"ctl"	"\e*Ctl_KP_F1")
(x-rebind-keysym "KP_F1"	"meta"	"\e*Meta_KP_F1")

(x-rebind-keysym "KP_F2"	"none"	"\e*KP_F2")
(x-rebind-keysym "KP_F2"	"shift"	"\e*Shift_KP_F2")
(x-rebind-keysym "KP_F2"	"ctl"	"\e*Ctl_KP_F2")
(x-rebind-keysym "KP_F2"	"meta"	"\e*Meta_KP_F2")

(x-rebind-keysym "KP_F3"	"none"	"\e*KP_F3")
(x-rebind-keysym "KP_F3"	"shift"	"\e*Shift_KP_F3")
(x-rebind-keysym "KP_F3"	"ctl"	"\e*Ctl_KP_F3")
(x-rebind-keysym "KP_F3"	"meta"	"\e*Meta_KP_F3")

(x-rebind-keysym "KP_F4"	"none"	"\e*KP_F4")
(x-rebind-keysym "KP_F4"	"shift"	"\e*Shift_KP_F4")
(x-rebind-keysym "KP_F4"	"ctl"	"\e*Ctl_KP_F4")
(x-rebind-keysym "KP_F4"	"meta"	"\e*Meta_KP_F4")

(x-rebind-keysym "KP_Multiply"	"none"	"*")
(x-rebind-keysym "KP_Multiply"	"shift"	"*")
(x-rebind-keysym "KP_Multiply"	"ctl"	"*")
(x-rebind-keysym "KP_Multiply"	"meta"	"*")

(x-rebind-keysym "KP_Divide"	"none"	"/")
(x-rebind-keysym "KP_Divide"	"shift"	"/")
(x-rebind-keysym "KP_Divide"	"ctl"	"/")
(x-rebind-keysym "KP_Divide"	"meta"	"/")

(x-rebind-keysym "KP_Add"	"none"	"+")
(x-rebind-keysym "KP_Add"	"shift"	"+")
(x-rebind-keysym "KP_Add"	"ctl"	"+")
(x-rebind-keysym "KP_Add"	"meta"	"+")

(x-rebind-keysym "KP_Subtract"	"none"	"-")
(x-rebind-keysym "KP_Subtract"	"shift"	"-")
(x-rebind-keysym "KP_Subtract"	"ctl"	"-")
(x-rebind-keysym "KP_Subtract"	"meta"	"-")

(x-rebind-keysym "KP_7"		"none"	"7")
(x-rebind-keysym "KP_7"		"shift"	"7")
(x-rebind-keysym "KP_7"		"ctl"	"7")
(x-rebind-keysym "KP_7"		"meta"	"7")

(x-rebind-keysym "KP_8"		"none"	"8")
(x-rebind-keysym "KP_8"		"shift"	"8")
(x-rebind-keysym "KP_8"		"ctl"	"8")
(x-rebind-keysym "KP_8"		"meta"	"8")

(x-rebind-keysym "KP_9"		"none"	"9")
(x-rebind-keysym "KP_9"		"shift"	"9")
(x-rebind-keysym "KP_9"		"ctl"	"9")
(x-rebind-keysym "KP_9"		"meta"	"9")

(x-rebind-keysym "KP_Enter"	"none"	"\n")
(x-rebind-keysym "KP_Enter"	"shift"	"\n")
(x-rebind-keysym "KP_Enter"	"ctl"	"\n")
(x-rebind-keysym "KP_Enter"	"meta"	"\n")

(x-rebind-keysym "KP_4"		"none"	"4")
(x-rebind-keysym "KP_4"		"shift"	"4")
(x-rebind-keysym "KP_4"		"ctl"	"4")
(x-rebind-keysym "KP_4"		"meta"	"4")

(x-rebind-keysym "KP_5"		"none"	"5")
(x-rebind-keysym "KP_5"		"shift"	"5")
(x-rebind-keysym "KP_5"		"ctl"	"5")
(x-rebind-keysym "KP_5"		"meta"	"5")

(x-rebind-keysym "KP_6"		"none"	"6")
(x-rebind-keysym "KP_6"		"shift"	"6")
(x-rebind-keysym "KP_6"		"ctl"	"6")
(x-rebind-keysym "KP_6"		"meta"	"6")

(x-rebind-keysym "KP_Separator"	"none"	",")
(x-rebind-keysym "KP_Separator"	"shift"	",")
(x-rebind-keysym "KP_Separator"	"ctl"	",")
(x-rebind-keysym "KP_Separator"	"meta"	",")

(x-rebind-keysym "KP_1"		"none"	"1")
(x-rebind-keysym "KP_1"		"shift"	"1")
(x-rebind-keysym "KP_1"		"ctl"	"1")
(x-rebind-keysym "KP_1"		"meta"	"1")

(x-rebind-keysym "KP_2"		"none"	"2")
(x-rebind-keysym "KP_2"		"shift"	"2")
(x-rebind-keysym "KP_2"		"ctl"	"2")
(x-rebind-keysym "KP_2"		"meta"	"2")

(x-rebind-keysym "KP_3"		"none"	"3")
(x-rebind-keysym "KP_3"		"shift"	"3")
(x-rebind-keysym "KP_3"		"ctl"	"3")
(x-rebind-keysym "KP_3"		"meta"	"3")

(x-rebind-keysym "KP_Tab"	"none"	"\t")
(x-rebind-keysym "KP_Tab_backward" nil	"\t")
(x-rebind-keysym "KP_Tab"	"ctl"	"\t")
(x-rebind-keysym "KP_Tab"	"meta"	"\t")

(x-rebind-keysym "KP_0"		"none"	"0")
(x-rebind-keysym "KP_0"		"shift"	"0")
(x-rebind-keysym "KP_0"		"ctl"	"0")
(x-rebind-keysym "KP_0"		"meta"	"0")

(x-rebind-keysym "KP_Decimal"	"none"	".")
(x-rebind-keysym "KP_Decimal"	"shift"	".")
(x-rebind-keysym "KP_Decimal"	"ctl"	".")
(x-rebind-keysym "KP_Decimal"	"meta"	".")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Simple non-standard functions to support key bindings	     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun delete-this-line ()
  "Delete current line, save as kill"
	(interactive)
	(let (curcol dot1)
	(setq curcol (current-column))
	(forward-line 0)
	(setq dot1 (dot))
	(forward-line 1)
	(kill-region dot1 (dot))
	(move-to-column curcol)))

(defun clear-this-line ()
  "Delete contents of current line, save as kill"
	(interactive)
	(forward-line 0)
	(kill-line))

(defun clear-entire-buffer ()
  "Delete contents of entire buffer, save as kill"
	(interactive)
	(mark-whole-buffer)
	(kill-region 1 (region-end)))

(defun switch-to-prev-buffer ()
  "Switch to previous buffer:  Like switch-to-buffer, but without interaction"
	(interactive)
	(switch-to-buffer (other-buffer (current-buffer))))

(defun start-end-kbd-macro ()
  "Start/stop capturing keystrokes for keyboard macro"
	(interactive)
	(if defining-kbd-macro
		(end-kbd-macro)
		(start-kbd-macro nil)))


( defvar grep-arg nil "Default arg for RE-search" )
( defun grep-arg ()
  (if (memq last-command '(research-forward research-backward)) grep-arg
    (let* ((command (car command-history))
	   (command-name (symbol-name (car command)))
	   (search-arg (car (cdr command)))
	   (search-command 
	    (and command-name (string-match "search" command-name)))
	   )
      (if (and search-command (stringp search-arg)) (setq grep-arg search-arg)
	(setq search-command this-command 
	      grep-arg (read-string "REsearch: " grep-arg)
	      this-command search-command)
	grep-arg)))
)

( defun research-forward ()
  "Repeat RE search forward."
  ( interactive )
  ( re-search-forward ( grep-arg ) )
)

( defun research-backward ()
  "Repeat RE search backward."
  ( interactive )
  ( re-search-backward ( grep-arg ) )
)

;;
;; stupid idea....
;;
;;(defun keys ()
;;  "Describe default HP key functions"
;;  (interactive)
;;  (view-file "/usr/local/gemacs-18.54/etc/HP_keys.text"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Map specific escape sequences	   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-unset-key "\233")		; Unmap meta-escape

;;;
;;;	Function key row along top of main cluster
;;;
(global-set-key	"\e*Break"		'compile)		; Break
(global-set-key	"\e*Shift_Break"	'grep)			; Shift Break (Reset)
(global-set-key	"\e*Cancel"		'next-error) 		; Stop
(global-set-key	"\e*Shift_Cancel"	'kill-compilation)	; Shift Stop

(global-set-key	"\e*F1"			'start-end-kbd-macro)	; f1
(global-set-key	"\e*F2"			'call-last-kbd-macro)	; f2
(global-set-key	"\e*F3"			'bury-buffer)		; f3
(global-set-key	"\e*F4"			'switch-to-prev-buffer)	; f4
(global-set-key	"\e*Menu"		'list-buffers)		; Menu
(global-set-key	"\e*System"		'dired)			; System
(global-set-key	"\e*User"		'rmail)			; User
(global-set-key	"\e*F5"			'research-forward)	; f5
(global-set-key	"\e*F6"			'research-backward)	; f6
(global-set-key "\e*F7"			'replace-regexp)	; f7
(global-set-key "\e*F8"			'query-replace-regexp)	; f8

(global-set-key	"\e*Clear_line"		'clear-this-line)	; Clear line
(global-set-key	"\e*Clear_display"	'clear-entire-buffer)	; Clear display


;;;
;;;	Special purpose keys in main key cluster
;;;
(global-set-key	"\e*Enter"		'find-file)		; Enter
(global-set-key	"\e*Print"		'insert-file)		; Print

(global-set-key	"\e*Insert_line"	'open-line)		; Insert line
(global-set-key	"\e*Delete_line"	'delete-this-line)	; Delete line

(global-set-key	"\e*Insert_char"	'overwrite-mode)	; Insert char
(global-set-key	"\e*Delete_char"	'delete-char)		; Delete char

(global-set-key	"\e*Home"		'beginning-of-buffer)	; Home
(global-set-key	"\e*Shift_Home"		'end-of-buffer)		;
(global-set-key	"\e*Meta_Home"		'end-of-buffer)		;
(global-set-key	"\233*Meta_Home"	'end-of-buffer)		;
(global-set-key	"\e*Prior"		'switch-to-prev-buffer)	; Prev

(global-set-key	"\e*Select"		'newline-and-indent)	; Select
(global-set-key	"\e*Next"		'other-window)		; Next


(global-set-key "\e*Up"			'previous-line)		; Up    arrow
(global-set-key "\e*Shift_Up"		'scroll-down)
(global-set-key "\e*Ctl_Up"		'previous-line)
(global-set-key "\e*Meta_Up"		'scroll-down)
(global-set-key "\233*Meta_Up"		'scroll-down)

(global-set-key "\e*Left"		'backward-char)		; Left  arrow
(global-set-key "\e*Shift_Left"		'backward-word)
(global-set-key "\e*Ctl_Left"		'backward-char)
(global-set-key "\e*Meta_Left"		'backward-word)
(global-set-key "\233*Meta_Left"	'backward-word)

(global-set-key "\e*Down"		'next-line)		; Down  arrow
(global-set-key "\e*Shift_Down"		'scroll-up)
(global-set-key "\e*Ctl_Down"		'next-line)
(global-set-key "\e*Meta_Down"		'scroll-up)
(global-set-key "\233*Meta_Down"	'scroll-up)

(global-set-key "\e*Right"		'forward-char)		; Right arrow
(global-set-key "\e*Shift_Right"	'forward-word)
(global-set-key "\e*Ctl_Right"		'forward-char)
(global-set-key "\e*Meta_Right"		'forward-word)
(global-set-key "\233*Meta_Right"	'forward-word)



;;;
;;;	Keypad cluster
;;;

(global-set-key	"\e*KP_F1"		'apropos)		; Keypad F1
(global-set-key	"\e*KP_F2"		'describe-key)		; Keypad F2
(global-set-key	"\e*KP_F3"		'describe-bindings)	; Keypad F3
(global-set-key	"\e*KP_F4"		'describe-function)	; Keypad F4


(global-set-key	"\e*KP_Multiply"	'text-mode)
(global-set-key	"\e*KP_Divide"		'indented-text-mode)
(global-set-key	"\e*KP_Add"		'outline-mode)
(global-set-key	"\e*KP_Subtract"	'fundamental-mode)

(global-set-key	"\e*KP_7"		'c-mode)
(global-set-key	"\e*KP_8"		'lisp-mode)
(global-set-key	"\e*KP_9"		'emacs-lisp-mode)
(global-set-key	"\e*KP_Enter"		'lisp-interaction-mode)

(global-set-key	"\e*KP_4"		'latex-mode)
(global-set-key	"\e*KP_5"		'plain-tex-mode)
(global-set-key	"\e*KP_6"		'scribe-mode)
(global-set-key	"\e*KP_Separator"	'nroff-mode)

(global-set-key	"\e*KP_1"		'copy-region-as-kill)
(global-set-key	"\e*KP_2"		'kill-region)
(global-set-key	"\e*KP_3"		'yank)
(global-set-key	"\e*KP_Tab"		'goto-line)		; Keypad Tab
(global-set-key	"\e*KP_Shift_Tab"	'what-line)		; Shift KP Tab

(global-set-key	"\e*KP_0"		'undo)			; Keypad 0
(global-set-key	"\e*KP_Decimal"		'set-mark-command)
