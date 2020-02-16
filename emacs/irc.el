Article 6707 of comp.emacs:
Path: kodak!rochester!bbn!usc!bloom-beacon!tut.cis.ohio-state.edu!rpi!rpi.edu!tale
From: tale@pawl.rpi.edu (David C Lawrence)
Newsgroups: comp.emacs,gnu.emacs
Subject: An Interface to the Internet Relay Chat
Message-ID: <TALE.89Jun23143155@imagine.pawl.rpi.edu>
Date: 23 Jun 89 18:31:55 GMT
Sender: usenet@rpi.edu
Reply-To: tale@pawl.rpi.edu
Lines: 1393
Xref: kodak comp.emacs:6707 gnu.emacs:1227

The following is an interface to IRC for use in GNU Emacs.  The
Internet Relay Chat conferencing system was recently posted to
alt.sources and is rumoured to be slated for posting in
comp.sources.unix.  This is version 1.0Gimel of this interface; if you
don't grok that, think of it as "beta test".

This is the third such Emacs interface that I know about.  My
implementation does things considerably differently from the other two
but I think it is most in the spirit of Emacs (one buffer, one task)
and has the most features.  One thing it lacks at the moment is an
aliasing mechanism but I intend to add that soon.  If you happen to
get hold of the other two I would suggest a naming scheme somthing
like irc-tale.el, irc-lars.el and irc-mike.el, but use whatever makes
the most sense to you.

To use: first and foremost you need an IRC server that will accept
your connexions.  This will usually be a machine in your organization
which is is running ircd.  Edit the variable irc-server in the first
few lines of the code to be the hostname of that machine.  You might
also need to change irc-port to be the port ircd is waiting on.  It is
usually 6667.  I didn't use the service name as the default because I
have noticed that many machines haven't put it in /etc/services.

Assuming you call it irc.el, you can make it autoloadable by putting
it in a directory in your load-path and then using somehting akin to:
   (autoload 'irc "irc" "An interface to the Internet Relay Chat" t)
in your .emacs.  Byte-compile for speed, although I've not noticed any
sorts of delays when running un-byte-compiled on this Sun 3/280.

M-x irc starts everything rolling.  I would suggest reading irc-news
(C-c n) as the first step to using the interface.  One last note: the
key bindings provided are not really something to which I am dearly
attached; suggestions for changes/additions are welcome.

Dave
-- 
;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; irc.el --- A user interface for the Internet Relay Chat
;; Author          : David C Lawrence           <tale@pawl.rpi.edu>
;; Created On      : Wed Jun 14 22:22:57 1989
;; Last Modified By: David C Lawrence
;; Last Modified On: Thu Jun 22 23:55:01 1989
;; Update Count    : 1
;; Status          : Seemingly stable.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Copyright (C) 1989  David C Lawrence

;;  This program is free software; you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation; either version 1, or (at your option)
;;  any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program; if not, write to the Free Software
;;  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; Comments and/or bug reports about this interface should be directed to:
;;     Dave Lawrence          <tale@{pawl,itsgw}.rpi.edu>
;;     76 1/2 13th Street     +1 518 273 5385
;;     Troy NY 12180

;; Defined variables
(provide 'irc)

(defvar irc-server "your.server.here"
  "*The server to use to establish a connexion to the IRC-net.")

(defvar irc-port 6667
  "*The port on which the IRC server responds.")

(defvar irc-oops "Oops ... ignore that."
  "*The text to send when using /OOPS.")

(defvar irc-nick (user-login-name)
  "*The nickname with which to enter IRC.")

(defvar irc-signals '((private t) (invite t) (wall t) (public) (join) (user))
  "Events in IRC that should get signals; this is a assoc list which is
maintained by the /SIGNAL command.")

(defvar irc-ignores nil
  "A list of users whose events will be ignored.  Maintain with /IGNORE.")

(defvar irc-notifies '(join)
  "Events in IRC that should get a notification message; currently only
a \"join\" event is supported in the /NOTIFY command.")

(defvar irc-confirm t
  "*If non-nil, provide confirmation for messages sent.")

(defvar irc-expand-colon t
  "*Non-nil means to expand a colon or semi-colon at the beginning of a
line to be the last explicit sendlist.")

(defvar irc-spacebar-pages t
  "*When this variable is non-nil, the following keys are in effect when
point is in the output region.

SPC      scroll-forward    DEL           scroll-backward
TAB      previous-line     LFD or RET    next-line")

(defvar irc-maximum-size 20480
  "*Maximum size that the *Irc* buffer can attain, in bytes.  The default
value of 20k represents an average of about 512 lines, or roughly 22 screens
on a standard VDT.")

(defvar irc-mode-hook nil
  "*Hook to run after starting irc-mode but before connecting to the server.")

(defvar irc-pop-on-signal 4
  "*This variable is a number whose reciprocal is the portion of the screen
to use when popping up the IRC window if only one window is visible.
A value of 1 causes the window to appear full-screen, 2 makes the window
1/2 the screen, 3 is 1/3, etc ...
If nil, don't pop up the *IRC* buffer when a signal is received.")

(defvar irc-max-history 40
  "*The maximum number of messages retained by irc-mode.  Commands are
not retained in the history list.")

(defvar irc-hist nil
  "A list of sent strings retained by irc-mode.")
(make-variable-buffer-local 'irc-hist)

(defvar irc-default-to "*;"
  "The default recipient of a message if no : or ; is provided.
\"*\" means the current channel, no matter what it is.")
(make-variable-buffer-local 'irc-default-to)

(defvar irc-mode-map nil "The keymap which irc-mode uses.")

(defconst irc-version "IRC-mode Version 1.0Gimel"
  "Current version of irc.el")

(defconst irc-command-alist
  '(("HELP" . "help")
    ("KILL" . "kill")
    ("OPER" . "oper")
    ("LINKS" . "links")
    ("QUOTE" . "quote")
    ("USERS" . "users")
    ("TOPIC" . "topic")
    ("NICKNAME" . "nick")
    ("AWAY" . "away") ("HERE" . "here")
    ("LIST" . "list") ("WHAT" . "list")
    ("SEND" . "send") ("QUERY" . "send")
    ("WALL" . "wall") ("MSG" . "privmsg")
    ("WHO" . "who") ("LONGWHO" . "whois")
    ("REDIRECT" . "redirect") ("OOPS" . "oops")
    ("INVITE" . "invite") ("SUMMON" . "summon")
    ("IGNORE" . "ignore") ("UNIGNORE" . "unignore")
    ("BYE" . "quit") ("QUIT" . "quit") ("STOP" . "quit")
    ("INFO" . "info") ("ADMIN" . "admin") ("TIME" . "time")
    ("JOIN" . "join") ("CHANNEL" . "join") ("LEAVE" . "leave")
    ("SIGNAL" . "signal") ("NOTIFY" . "notify") ("CONFIRM" . "confirm")))

;; varmapcar and keymap
(defmacro varmapcar (func list &optional arg-pos &rest args)
  "Apply mapcar to a function that takes more than one argument.
Calling form is (varmapcar func list &optional arg-pos &rest args).
'func' and 'list' are the same as in mapcar; just provide these two and
varmapcar will behave like mapcar.  If 'arg-pos' is provided then 'args'
must be too.  'args' are the other arguments to pass to the function.
'arg-pos' is the location to splice in the argument provided by the mapping;
it is zero based."
  (list 'mapcar 
        (list 'function 
              (list 'lambda '(arg)
                    (append (list (eval func))
                            (if args
                                (if (= (length args) arg-pos)
                                    (append args '(arg))
                                  (let (arglist)
                                    (while args
                                      (if (= (length arglist) arg-pos)
                                          (setq arglist 
                                                (append arglist '(arg) args)
                                                args nil)
                                        (setq arglist
                                              (append arglist
                                                      (list (car args)))
                                              args (cdr args))))
                                    arglist))
                              '(arg)))))
        list))

(or irc-mode-map
    (progn
      (setq irc-mode-map (make-keymap))
      (define-key irc-mode-map "\C-j" 'irc-send-input)
      (define-key irc-mode-map "\C-m" 'irc-send-input)
      (define-key irc-mode-map "\C-i"      'irc-tab)
      (define-key irc-mode-map "\C-c\C-h"  'irc-execute-help)
      (define-key irc-mode-map "\C-c\C-j"  'irc-execute-join)
      (define-key irc-mode-map "\C-c\C-l"  'irc-execute-list)
      (define-key irc-mode-map "\C-c\C-m"  'irc-history-menu)
      (define-key irc-mode-map "\C-c\C-n"  'irc-hist-next)
      (define-key irc-mode-map "\C-c\C-o"  'irc-execute-oops)
      (define-key irc-mode-map "\C-c\C-p"  'irc-hist-prev)
      (define-key irc-mode-map "\C-c\C-q"  'irc-execute-leave)
      (define-key irc-mode-map "\C-c\C-r"  'irc-execute-redirect)
      (define-key irc-mode-map "\C-c\C-s"  'irc-execute-send)
      (define-key irc-mode-map "\C-c\C-u"  'irc-kill-input)
      (define-key irc-mode-map "\C-c\C-w"  'irc-execute-who)
      (define-key irc-mode-map "\C-c " 'irc-pong)
      (define-key irc-mode-map "\C-ck" 'irc-execute-quit)
      (define-key irc-mode-map "\C-cn" 'irc-news)
      (define-key irc-mode-map "\C-cp" 'irc-yank-prev-command)
      (define-key irc-mode-map "\C-cv" 'irc-version)
      (define-key irc-mode-map "\C-?"  'irc-del-backward-char)
      (varmapcar 'define-key (where-is-internal 'self-insert-command nil nil)
                 1 irc-mode-map 'irc-self-insert)))

;; filters and sentinels
(defun irc-filter (proc str)
  "Filtering procedure for IRC server messages."
  (let ((ibuf (process-buffer proc)) bell imtp np now win size)
    (save-excursion
      (set-buffer ibuf)
      (irc-truncate-buffer irc-maximum-size)
      (setq imtp (- (point) (setq now (goto-char irc-mark)))
            irc-scratch (concat irc-scratch str))
      (while (string-match "\n" irc-scratch)
        (setq irc-scratch (irc-parse-server-msg irc-scratch)
              bell (cdr irc-scratch)
              irc-scratch (car irc-scratch))
        (if (not bell) ()
          (ding 'no-terminate)
          (minibuffer-message " [Bell in %s]" (buffer-name ibuf))))
      (setq np (+ (if (< imtp 0) now irc-mark) imtp)))
    (set-buffer-modified-p (buffer-modified-p))
    (if (setq win (get-buffer-window ibuf)) (set-window-point win np)
      (save-excursion (set-buffer ibuf) (goto-char np)))
    (if (not (and irc-pop-on-signal bell (not win))) ()
      (setq win (selected-window))
      (if (/= (count-windows 'no-mini) 1) (display-buffer ibuf)
        (select-window (next-window win 'no-mini))
        (setq size (- (screen-height) (/ (screen-height) irc-pop-on-signal)))
        (split-window (selected-window) size)
        (display-buffer ibuf)
        (select-window win)))))

(defun irc-parse-server-msg (str)
  "Take the first line from STR and convert it into a message for the user.
It returns a dotted-pair whose car is the remainder of STR after the first
newline and whose cdr is either t or nil, indicating whether a bell was
issued."
  (let ((loc 0) (line (substring str 0 (string-match "\n" str))))
    (while (string-match "%" line loc)
      (setq line (concat (substring line 0 (match-end 0)) "%"
                         (substring line (match-end 0)))
            loc (1+ (match-end 0))))
    (cons
     (substring str (1+ (string-match "\n" str)))
     (cond
      ((string-match "^:\\S +\\s +CHANNEL" line) (irc-parse-channel line))
      ((string-match "^:\\S +\\s +INVITE" line) (irc-parse-invite line))
      ((string-match "^:\\S +\\s +MSG" line) (irc-parse-public line))
      ((string-match "^:\\S +\\s +NICK" line) (irc-parse-nick line))
      ((string-match "^:\\S +\\s +WALL" line) (irc-parse-wall line))
      ((string-match "^:\\S +\\s +QUIT" line) (irc-parse-quit line))
      ((string-match "^\\(:\\S *\\s +\\)?PRIVMSG" line)
       (irc-parse-private line))
      ((string-match "^PING" line) (irc-pong))
      ((string-match "^ERROR" line) (irc-parse-error line))
      ((string-match "^NOTICE" line) (irc-parse-notice line))
      ((string-match "^WHOREPLY" line) (irc-parse-whoreply line))
      ((string-match "^LINREPLY" line) (irc-parse-linreply line))
      (t (insert str "\n") nil)))))

(defun irc-parse-channel (str)
  "Examine a CHANNEL message and show it if requested.  Returns t if a bell
was generated for the event, nil otherwise."
  (let ((user (substring str 1 (string-match "\\s CHANNEL " str)))
        (channel (string-to-int (substring str (match-end 0)))))
    (irc-maintain-list 'irc-wholist user 'add)
    (if (string= user irc-nick)
        (progn
          (if (/= channel 0)
              (irc-insert "You are now a member of channel %d." channel)
            (irc-insert "You have left channel %d."
                        (get 'irc-channel 'o-chan)))
          nil)
      (if (or (member-general user irc-ignores 'string=)
              (not (memq 'join irc-notifies))) ()
        (if (/= channel 0)
            (irc-insert "*** %s has joined channel %d ***" user channel)
          (irc-insert "*** %s has left channel %d ***"
                      user irc-channel))
        (irc-signal user 'join)))))

(defun irc-parse-invite (str)
  "Examine an INVITE message and display it.  Returns t if a bell was generated
for the event, nil otherwise."
  (let ((user (substring str 1 (string-match "\\s +INVITE " str)))
        (to (substring str (match-end 0)
                       (string-match "\\s +" str (match-end 0))))
        (channel (substring str (match-end 0))))
    (irc-maintain-list 'irc-wholist user 'add)
    (if (member-general user irc-ignores 'string=) ()
      (irc-insert "*** %s invites %s to join channel %s ***" user
                  (if (string= to irc-nick) "you" to) channel)
      (irc-signal user 'invite))))

(defun irc-parse-public (str)
  "Examine a MSG message and display it.  Returns t if a bell was generated for
the event, nil otherwise."
  (let ((user (substring str 1 (string-match "\\s MSG :" str)))
        (msg (substring str (match-end 0))))
    (irc-maintain-list 'irc-wholist user 'add)
    (if (member-general user irc-ignores 'string=) ()
      (irc-insert "\n -> From %s to %d:" user irc-channel)
      (irc-insert-message msg)
      (irc-signal user 'public))))

(defun irc-parse-private (str)
  "Examine a PRIVMSG message and display it.  Returns t if a bell was generated
for the event, nil otherwise."
  ;; This is really gross because it kludges in the fact that PRIVMSG can
  ;; be used to send notification of a change of channel topic.  Actually,
  ;; topic changes are handled poorly all-around by the servers because
  ;; only the person who changed the topic gets notification.
  ;; Also have to kludge in the fact that TIME to a remote host gives back
  ;; a PRIVMSG with no sended but with a leading :.  ARGHGHGHG!!
  (let (from to msg)
    (if (string-match "^:\\S +\\s +PRIVMSG\\s +" str)
        (setq from (substring str 1 (string-match "\\s +PRIVMSG\\s +" str))
              to (substring str (match-end 0)
                            (string-match "\\s +:" str (match-end 0))))
      (setq from nil
            to (substring str 9 (string-match "\\s :" str))))
    (setq msg (substring str (match-end 0)))
    (if (not from) (progn (irc-insert msg) nil)
      (irc-maintain-list 'irc-wholist from 'add)
      (if (member-general from irc-ignores 'string=) ()
        (irc-insert "\n >> Private message from %s:" from)
        (or (string= to irc-nick)
            (and (= (string-to-int to) irc-channel) (not (string= "0" to)))
            (irc-insert " (apparently to %s)" to))
        (irc-insert-message msg)
        (irc-signal from 'private)))))

(defun irc-parse-quit (str)
  "Notify of a users departure from IRC."
  (let ((user (substring str 1 (string-match "\\s +QUIT" str))))
    (irc-maintain-list 'irc-wholist user 'remove)
    (if (member-general user irc-ignores 'string=) ()
      (irc-insert "*** %s has left IRC ***" user)
      (irc-signal user 'join))))

(defun irc-parse-wall (str)
  "Show a WALL message.  WALL messages will always be displayed even if
the sender is /ignore'd."
  (let ((user (substring str 1 (string-match "\\s +WALL\\s +:" str)))
        (msg (substring str (match-end 0))))
    (irc-maintain-list 'irc-wholist user 'add)
    (irc-insert "\n ## Message from %s to everyone:" user)
    (irc-insert-message msg)
    (irc-signal user 'wall)))

(defun irc-parse-nick (str)
  "Note a change of nickname."
  ;; hey!!  someone changed the server, I think.  I could of sworn it
  ;; used to reply when you changed your own name!!  @*$%##@!!
  ;; --found the problem.  it only sends the message if you are on a channel
  (let ((old (substring str 1 (string-match "\\s NICK " str)))
        (new (substring str (match-end 0))))
    (irc-maintain-list 'irc-wholist old 'remove)
    (irc-maintain-list 'irc-wholist new 'add)
;"    (if (string= old irc-nick)
;        (progn (setq irc-nick new)
;               (put 'irc-nick 'o-nick old)
;               (irc-insert "You will now be known as %s." new) nil)"
    (if (member-general old irc-ignores 'string=)
        (progn (irc-maintain-list 'irc-ignores old 'remove)
               (irc-maintain-list 'irc-ignores new 'add))
      (irc-insert "*** %s is now known as %s ***" old new)
      (irc-signal old 'user))))

(defun irc-parse-error (str)
  "Insert a server error message."
  (string-match "\\s +:" str)
  (if (string-match "Nickname\\s +\\S *\\s +\\(already\\|not\\s +chan\\)"
                    str)
      (setq irc-nick (or (get 'irc-nick 'o-nick)
                         "NO NAME YET (/NICK to set one)")))
  (irc-insert (substring str (match-end 0))))

(defun irc-parse-notice (str)
  "Insert a NOTICE message."
  (if (string-match "Error: No such nickname (" str)
      (irc-maintain-list 'irc-wholist
                         (substring str (match-end 0) (string-match ")$" str))
                         'remove))
  (string-match "\\s +:" str)
  (irc-insert (substring str (match-end 0))))

(defun irc-parse-whoreply (str)
  "Format the reply lines of WHO."
  (string-match "^WHOREPLY\\s +" str)
  (setq str (substring str (match-end 0)))
  (let (split)
    (while (not (string-match "^:" str))
      (setq split (cons (substring str 0 (string-match "\\(\\s +\\|$\\)" str))
                        split)
            str (substring str (match-end 0))))
    (setq split (cons str split))
    (or (string= (nth 1 split) "S")
        (irc-maintain-list 'irc-wholist (nth 2 split) 'add))
    (irc-insert (concat
                 (if (member-general (nth 2 split) irc-ignores 'string=) "#"
                   (if (string= "G" (nth 1 split)) "-" " "))
                 (nth 2 split)
                 (make-string (- 10 (length (nth 2 split))) 32)
                 (format "%3s " (if (= (string-to-int (nth 6 split)) 0)
                                    (if (string= "*" (nth 6 split)) "Chn" "")
                                  (nth 6 split)))
                 (nth 5 split) "@" (nth 4 split) " ("
                 (substring (car split) 1) ")"))))

(defun irc-parse-linreply (str)
  "Format the reply lines of LINKS."
  (string-match "^LINREPLY\\s +" str)
  (irc-insert "Server: %s (%s)"
              (substring str (match-end 0)
                         (string-match "\\s +" str (match-end 0)))
              (substring str (match-end 0))))

(defun irc-insert-message (msg)
  "Format the body of a message by breaking it into 75 character lines."
  (let (line)
    (while (> (length msg) 75)
      (setq line (substring msg 0 76) msg (substring msg 76))
      (if (string-match "\\s \\S *$" line)
          (setq msg (concat (substring line (1+ (match-beginning 0))) msg)
                line (substring line 0 (match-beginning 0))))
      (irc-insert (concat " - " line)))
    (irc-insert (concat " - " msg))))

(defun irc-pong ()
  "Send out a 'Yep, I be here' message."
  (interactive) (irc-send (concat "PONG " (system-name))) nil)

(defun irc-insert (format &rest args)
  "Insert-before-markers the string created by FORMAT with substituted ARGS."
  (string-match "" format)
  (let ((str (apply 'format format args)) line)
    (while (> (length str) 79)
      (setq line (substring str 0 80) str (substring str 80))
      (if (string-match "\\s \\S *$" line)
          (setq str (concat (substring line (1+ (match-beginning 0))) str)
                line (substring line 0 (match-beginning 0))))
      (insert-before-markers (concat line "\n")))
    (insert-before-markers (concat str "\n"))))

(defun irc-subst-comma (str newsep)
  "Take the last comma and space from STR and replace it with NEWSEP, the
new seperator.  A space will be added before NEWSEP."
  (if (string-match ", [^,]*$" str)
      (concat (substring str 0 (match-beginning 0)) " " newsep
              (substring str (1+ (match-beginning 0))))
    str))

;; simple key functions
(defun irc-self-insert (arg)
  "Input region: self-insert-command.
Output region: if irc-spacebar-pages and space typed, scroll-up; otherwise
move to end of input region and insert character."
  (interactive "p")
  (let ((expand-colon
         (and (or (= last-input-char ?:) (= last-input-char ?\;))
              (string-match
               "^\\s *$" (buffer-substring irc-mark
                                           (if (irc-input-region-p) (point)
                                             (point-max)))))))
    (if (irc-input-region-p)
        (if (not (and irc-expand-colon expand-colon))
            (self-insert-command arg)
          (delete-region irc-mark (point))
          (insert irc-last-explicit)
          (if (> arg 1) (insert (make-string (1- arg) last-input-char))))
      (if (and irc-spacebar-pages (= last-input-char 32))
          (condition-case EOB (scroll-up nil)
            (end-of-buffer (goto-char (point-max))))
        (goto-char (point-max))
        (if (not (and irc-expand-colon expand-colon))
            (self-insert-command arg)
          (delete-region irc-mark (point))
          (insert irc-last-explicit)
          (if (> arg 1) (insert (make-string (1- arg) last-input-char))))))))

(defun irc-del-backward-char (arg)
  "Input region: delete-backward-char, restricted to input region.
Output region: if irc-spacebar-pages, scroll-down; otherwise do nothing."
  (interactive "p")
  (if (> (point) irc-mark)
      (let ((cmtp (- (point) irc-mark)))
	(if (> arg cmtp) (setq arg cmtp))
	(delete-backward-char arg))
    (if (and irc-spacebar-pages (not (irc-input-region-p))) (scroll-down nil)
      (ding))))

(defun irc-tab ()
  "Input region: tab-to-tab-stop.
Output region: previous-line if irc-spacebar-pages; do nothing otherwise."
  (interactive)
  (if (irc-input-region-p) (tab-to-tab-stop)
    (if irc-spacebar-pages (previous-line 1) (ding))))

(defun irc-input-region-p ()
  "Return t if point >= irc-mark, nil otherwise."
  (if (>= (point) irc-mark) t))

;; top-level -- entry and mode
(defun irc ()
  "Enter the Internet Relay Chat conferencing system."
  (interactive)
  (let ((buffer (generate-new-buffer "*IRC*")) (proc-name "irc") rproc)
    (switch-to-buffer buffer)
    (erase-buffer)
    (insert "IRC-mode for GNU Emacs -- bugs to tale@pawl.rpi.edu."
            "  C-c n for news.\n\n")
    (irc-mode)
    (condition-case NOT-IRCED
        (progn
          (setq rproc (open-network-stream proc-name buffer
                                           irc-server irc-port))
          (set-process-filter rproc 'irc-filter)
          (irc-send (format "USER %s %s %s %s" (user-login-name)
                            (system-name) irc-server (user-full-name)))
          (irc-send (concat "NICK " irc-nick))
          (setq irc-away nil irc-channel 0))
      (error (irc-insert "Sorry ... couldn't connect to %s at port %d."
                         irc-server irc-port)))))

(defun irc-mode ()
  "To understand some documentation given with irc-mode variables and
functions, output region is defined as everything before the irc-mark.
irc-mark is a marker kept by irc-mode to know where to insert new text
from IRC.  Text in the output region cannot be modified by the most common
methods of typing a self-inserting character or pressing delete.

The input region is everything which follows irc-mark.  It is what
gets processed by irc-mode when you type LFD or RET.  If irc-spacebar-pages
is non-nil, the following keys are in effect when the cursor is in the
output region:

SPC             scroll-forward       DEL     scroll-backward
LFD or RET      next-line            TAB     previous-line

Local keys:
\\{irc-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'irc-mode mode-name "IRC")
  (make-local-variable 'irc-away)
  (make-local-variable 'irc-channel)
  (make-local-variable 'irc-wholist)
  (set (make-local-variable 'irc-hist-index) -1)
  (set (make-local-variable 'irc-scratch) "")
  (set (make-local-variable 'irc-last-command) "")
  (set (make-local-variable 'irc-last-explicit) "*;")
  (set (make-local-variable 'blink-matching-paren) nil)
  (set (make-local-variable 'scroll-step) 1)
  (set (make-local-variable 'mode-line-format)
       (list (purecopy "--- %14b") 'global-mode-string
             (purecopy "  %[(") 'mode-name 'minor-mode-alist
             (purecopy ")%]---") 'irc-nick 'irc-away (purecopy "-%-")))
  (set-marker (set (make-local-variable 'irc-mark) (make-marker)) (point-max))
  (buffer-enable-undo)
  (use-local-map irc-mode-map)
  (run-hooks 'irc-mode-hook))

;; sending
(defun irc-send-input ()
  "Input region: process the input region for IRC.
Output region: if irc-spacebar-pages, next-line; otherwise do nothing."
  (interactive)
  (if (not (irc-input-region-p)) (if irc-spacebar-pages (next-line 1) (ding))
    (delete-region (goto-char (point-max))
                   (if (re-search-backward "[^ \t\n]" irc-mark t)
                       (1+ (point)) (point)))
    (delete-region (goto-char irc-mark)
                   (progn (re-search-forward "\\s *") (point)))
    (setq irc-hist-index -1)
    (let ((irc-to-send (buffer-substring irc-mark (point-max))) irc-input ass)
      (if (string= "" irc-to-send) (message "(nothing sent to the irc-server")
        (while (string< "" irc-to-send)
          (setq irc-input
                (substring irc-to-send 0 (string-match "\n\\|$" irc-to-send))
                irc-to-send (substring irc-to-send (match-end 0)))
          (re-search-forward (regexp-quote irc-input))
          (if (looking-at "\n") (forward-char 1) (insert "\n"))
          (set-marker irc-mark (point))
          (cond
           ((string-match "^/" irc-input)
            (if (< (length irc-input) 250)
                (irc-execute-command (setq irc-last-command
                                           (substring irc-input 1)))
              (ding) (message "IRC commands can't exceed 250 characters.")))
           ((string< "" irc-input)
            (setq ass (irc-find-to irc-input 'explicit))
            (if (and ass (string-match "^[^:;]" irc-input))
                (setq irc-last-explicit (irc-find-to irc-input)))
            (if (> (length irc-input) 250)              ;; grossness
                (let ((send (substring irc-input 0 251))
                      (keep (substring irc-input 251)))
                  (if (= (aref keep 0) 32) (setq keep (substring keep 1))
                    (if (string-match "[ \t][^ \t]*$" send
                                      (if ass (length (irc-find-to send))))
                        (setq keep (concat
                                    (substring send (1+ (match-beginning 0)))
                                    keep)
                              send (substring send 0 (match-beginning 0)))))
                  (setq irc-input (concat send " >>")
                        irc-to-send (concat (if ass ";") keep irc-to-send))
                  (re-search-backward (regexp-quote send))
                  (re-search-forward  (regexp-quote send))
                  (insert " >>\n" (if ass ";"))
                  (delete-char 1) (backward-char 1)))
            (irc-add-to-hist
             (irc-execute-msg (concat (if (not ass) irc-default-to)
                                      irc-input)))))))
      (set-marker irc-mark (goto-char (point-max))))))

(defun irc-send (str)
  "Send STR to the IRC process.  CR-LFD pair is appended automatically."
  (send-string (get-buffer-process (current-buffer)) (concat str "\r\n"))
  str)


;; /commands
(defun irc-execute-command (str)
  "Execute the \"/\" command of STR.  STR should not begin with a slash.
Most commands are passed through, but oops, resend, join, quit and send are
all handed specially."
  (let* ((case-fold-search t) (errormsg "Ambiguous command '/%s'.  Could be ")
         (command (substring str 0 (string-match "\\(\\s +\\|$\\)" str)))
         (text (substring str (match-end 0)))
         (matches
          (apply 'append
                 (mapcar (function
                          (lambda (dp)
                            (if (string-match (concat "^" command) (car dp))
                                (list dp)))) irc-command-alist))))
    (if (not matches)
        (irc-insert "Unknown command '/%s'.  Type /HELP for help." str)
      (if (= (length matches) 1)
          (funcall (intern-soft (concat "irc-execute-" (cdr (car matches))))
                   text)
        (irc-insert
         (irc-subst-comma
          (concat errormsg
                  (mapconcat (function (lambda (arg) (concat "/" (car arg))))
                             matches ", ")
                  ".") "or") str)))))

(fset 'irc-execute-quote 'irc-send)

(defun irc-execute-who (&optional channel)
  "Get a list of the users on IRC; with optional string CHANNEL just users
on that channel are shown.  The resulting listing uses '-' to represent
users who are away and '#' to represent users whom you are ignoring.  Users
who are not on any channel, or who are on a channel >1000, do not have
anything displayed under the Chn column."
  (interactive)
  (if (or (not channel) (= (string-to-int channel) 0)) (setq irc-wholist nil))
  (irc-send (concat "WHO " channel)))

(defun irc-execute-list (&optional cruft)
  "Get a list of the discussions that are on IRC."
  (interactive) (irc-send "LIST"))

(defun irc-execute-links (&optional cruft)
  "List the names of the servers with which your server is communicating."
  (interactive) (irc-send "LINKS"))

(defun irc-execute-admin (&optional cruft) ; what an evil thought
  "Get information about your server's IRC administrator."
  (interactive) (irc-send "ADMIN"))

(defun irc-execute-time (&optional host)
  "Get the current time on your IRC server or on optional HOST."
  (interactive) (irc-send (concat "TIME " host)))

(defun irc-execute-join (&optional disc)
  "Join a channel on IRC."
  (interactive)
  (if (or (not disc) (string= disc ""))
      (setq disc (read-string "Channel to join? ")))
  (irc-send (concat "CHANNEL " (int-to-string (string-to-int disc))))
  (put 'irc-channel 'o-chan irc-channel)
  (setq irc-channel (string-to-int disc)))

(defun irc-execute-nick (&optional newname)
  "Change your nickname."
  (interactive)
  (while (or (not newname) (not (string-match "^[A-Za-z0-9]+$" newname)))
    (setq newname (read-string "New nickname? ")))
  (if (> (length newname) 9) (setq newname (substring newname 0 9)))
  (irc-insert "You will now be known as \"%s\"." newname)
  (put 'irc-nick 'o-nick irc-nick)
  (setq irc-nick newname)
  (irc-send (concat "NICK " newname)))

(defun irc-execute-quit (&optional text)
  "Exit IRC."
  (interactive)
  (if (and text (string< "" text))
      (irc-insert "/QUIT takes no arguments.")
    (irc-send "QUIT")))

(defun irc-execute-privmsg (str)
  "Send a private message."
  (irc-add-to-hist
   (irc-execute-msg (concat
                     (setq irc-last-explicit
                           (concat (substring
                                    str 0 (string-match "\\s +" str)) ";"))
                     (substring str (match-end 0))))))

(defun irc-execute-msg (str)
  "Send a public or private message.  Returns its argument STR."
  (let (tolist (orig str) icw confirm)
    (if (string-match "^[;:]" str)
        (setq str (concat irc-last-explicit (substring str 1)))
      (if (not (irc-find-to str 'explicit))
          (if irc-default-to (setq str (concat irc-default-to str))
            (irc-insert "You have no default sendlist."))))
    (if (irc-find-to str 'explicit)
        (setq icw (irc-find-to str)
              tolist (irc-burst-comma (substring icw 0 (1- (length icw))))
              str (irc-find-message str)))
    (setq
     confirm
     (delq
      nil
      (mapcar (function
               (lambda (to)
                 (if (not (zerop (string-to-int to)))
                     (if (= (string-to-int to) irc-channel)
                         (progn (irc-send (concat "MSG " str)) to)
                       (irc-insert "You are not on channel %d."
                                   (string-to-int to)) nil)
                   (setq icw (irc-check-list irc-wholist to))
                   (cond
                    ((string= to "*")
                     (if (zerop irc-channel)
                         (progn (irc-insert "You are not on any channel.") nil)
                       (irc-send (concat "MSG " str))
                       (int-to-string irc-channel)))
                    ((string= to "0")
                     (irc-insert "You can't send to channel 0.") nil)
                    ((= (length icw) 1)
                     (irc-send (concat "PRIVMSG " (car icw) " " str))
                     (car icw))
                    ((not icw)
                     (irc-insert "No names found to match \"%s\"." to) nil)
                    (t
                     (irc-insert "Ambiguous recipient \"%s\"; could be %s."
                                 to (irc-subst-comma
                                     (mapconcat 'eval icw ", ") "or")) nil)))))
              tolist)))
    (if (and confirm irc-confirm)
        (irc-insert "(message sent to %s)"
                    (irc-subst-comma (mapconcat 'eval confirm ", ") "and"))
      (if (not confirm) (irc-insert "(message not sent)")))
    orig))

(defun irc-execute-oops (&optional newto)	; one of my favourites. 
  "Send irc-oops to recipient(s) of last message and resend message to
specified recipient."
  (interactive)
  (irc-execute-msg (concat (irc-find-to (car irc-hist)) irc-oops))
  (irc-execute-redirect newto))

(defun irc-execute-redirect (&optional newto)
  "Send last message to new recipient(s)."
  (interactive)
  (if (or (string= newto "") (not newto))
      (setq newto
            (read-string
             (format "New recipient(s)? %s"
                     (if (not irc-default-to) ""
                       (concat " [RET for "
                               (substring irc-default-to 0
                                          (1- (length irc-default-to)))
                               " ]"))))))
  (setq newto (if (string= "" newto) irc-default-to (concat newto ";"))
        irc-last-explicit newto)
  (irc-add-to-hist
   (irc-execute-msg (concat newto (irc-find-message (car irc-hist))))))

(defun irc-execute-send (&optional slist)
  "Set the default sendlist for IRC messages.  Just \"-\" means use no
default sendlist.  A null-string doesn't do anything to the default list.
Each item specified is checked to see whether you can send there; ambiguous
references to users are not allowed nor are channels which you are not on.
\"*\" is always allowed.  If no item in the new list can be set then the
sendlist is not changed."
  (interactive "sDefault recipient(s) for messages? ")
  (let (matches)
    (if (string= "-" slist) (setq irc-default-to nil)
      (setq matches
            (delq nil
                  (mapcar
                   (function
                    (lambda (arg)
                      (setq matches (irc-check-list irc-wholist arg))
                      (cond
                       ((string= arg "*") arg)
                       ((string= arg "0")
                        (irc-insert "You can't send to channel 0.") nil)
                       ((not (zerop (string-to-int arg)))
                        (if (= (string-to-int arg) irc-channel) arg
                          (irc-insert "You are not on channel %s." arg) nil))
                       ((= (length matches) 1) (car matches))
                       ((eq matches nil)
                        (irc-insert "No names found to match \"%s\"." arg) nil)
                       (t
                        (irc-insert "Ambiguous recipient \"%s\"; could be %s."
                                    to (irc-subst-comma
                                        (mapconcat 'eval matches ", ") "or"))
                        nil)))) (irc-burst-comma slist))))
      (if matches
          (setq irc-default-to (concat (mapconcat 'eval matches ",") ";"))
        (or (string= "" slist)
            (irc-insert "(no matches -- sendlist not changed)"))))
    (if (not irc-default-to) (irc-insert "Your default sendlist is disabled.")
      (irc-insert
       "You are sending to %s."
       (irc-subst-comma
        (mapconcat 'eval
                   (irc-burst-comma
                    (substring irc-default-to 0
                               (1- (length irc-default-to)))) ", ") "and")))))

(defun irc-execute-leave (&optional cruft)
  "Leave your current channel and join no other."
  (interactive) (put 'irc-channel 'o-chan irc-channel) (irc-send "CHANNEL 0"))

(defun irc-execute-notify (&optional notify)
  "Set the list for events to notify you about with a message.
Events are added with +event or simply event; they are removed with -event.
+ adds all supported events and - removes all supported events.
Events currently supported by /notify: join."
  (interactive "sNotify for events: ")
  (let ((recog '(join)) (str notify) sym)
    (while (string< "" notify)
      (setq str (substring notify 0 (string-match "\\(\\s +\\|$\\)" notify))
            notify (substring notify (match-end 0)))
      (cond
       ((string= "-" str) (setq irc-notifies nil))
       ((string= "+" str) (setq irc-notifies recog))
       ((= (aref str 0) 45)
        (if (memq (setq sym (car (read-from-string (substring str 1)))) recog)
            (setq irc-notifies (delq sym irc-notifies))
          (irc-insert "Notify: Unknown argument '%s'." sym)))
       (t
        (if (memq (setq sym
                        (car (read-from-string
                              (if (= (aref str 0) 43) (substring str 1) str))))
                  recog)
            (if (not (memq sym irc-notifies))
                (setq irc-notifies (cons sym irc-notifies)))
          (irc-insert "Notify: Unknown argument '%s'." sym)))))
    (if irc-notifies
        (irc-insert "Notification is currently enabled for %s."
                    (irc-subst-comma (mapconcat 'prin1-to-string irc-notifies
                                                ", ") "and"))
      (irc-insert "Notification is currently disabled."))))

(defun irc-execute-confirm (&optional str)
  "Turn on message confirmation with + or off with -."
  (interactive "sSet confimation on (+) or off (-)? ")
  (cond ((string= str "+") (setq irc-confirm t))
        ((string= str "-") (setq irc-confirm nil))
        ((string< "" str)
         (irc-insert "Confirm: unknown argument \"%s\"." str)))
  (irc-insert "Message confirmation is %s." (if irc-confirm "on" "off")))

(defun irc-execute-here (&optional cruft)
  "Mark yourself as present (ie, not \"away\") on IRC."
  (interactive) (irc-send "AWAY") (setq irc-away ""))

(defun irc-execute-away (&optional text)
  "Mark yourself as away, giving TEXT to people who send you messages."
  (interactive "sReason for being away: ")
  (irc-send (concat "AWAY " text))
  (setq irc-away (concat " [" text "]")))

(defun irc-execute-whois (&optional user)
  "Get a longer description (longer than /who) of who a user is."
  (interactive)
  (let (match)
    (while (and (string< "" user)
                (/= (length (setq match (irc-check-list irc-wholist user))) 1))
      (setq user (read-string "Who is who? ")))
    (if (string< "" user)
        (irc-send (concat "WHOIS " (car match))))))

(defun irc-execute-topic (&optional topic)
  "Make TOPIC the topic blurb for the current channel."
  (interactive)
  (if (= irc-channel 0)
      (irc-insert "You aren't on any channel.")
    (if (interactive-p)
        (setq topic
              (read-string (format "Topic for channel %d? " irc-channel))))
    (irc-send (concat "TOPIC " topic))))

(defun irc-execute-oper (oper &optional passwd) ;; for crimes against humanity
  "(Attempt to) Become an IRC operator.  Can take the name of the operator
and the password as arguments.  If the password is not provided it will
be read (and blanked) in the minibuffer."
  (interactive "sOperator name? ")
  (if (and (string-match "\\s +" oper) (or (not passwd) (string= passwd "")))
      (setq passwd (substring oper (match-end 0))
            oper (substring oper (match-beginning 0))))
  (while (not (string-match "^[A-Za-z0-9]*$" oper))
    (setq oper (read-string "Operator name? ")))
  (if (string= "" oper) ()
    (if (or (not passwd) (string= passwd ""))
        (setq passwd
              (irc-read-passwd (format "Password for operator %s? " oper))))
    (irc-send (concat "OPER " oper " " passwd))))

(defun irc-execute-summon (&optional user)
  "Summon a user not on IRC to join IRC.  The argument provided may either be
a user name on the local machine or user@host.  'host' in this case must be a
host which the IRC-net recognizes as running the IRC daemon and the user must
be signed on at the time you attempt the request."
  (interactive)
  (if (or (not user) (string= "" user))
      (setq user (read-string "User to summon to IRC? ")))
  (if (string< "" user) (irc-send (concat "SUMMON " user))))

(defun irc-execute-users (host) ;; this is entirely too violent
  "Get a list of the users signed on to your local IRC-server.  With
argument HOST, list the users signed on there.  HOST must be known to the
IRC-net as running the IRC daemon."
  (interactive "sList users on which host? [RET for localhost] ")
  (irc-send (concat "USERS " host)))

(defun irc-execute-info (&optional cruft)
  "Show some information about the programmer of IRC."
  (interactive) (irc-send "INFO"))

(defun irc-execute-kill (&optional user)
  "Remove a user from IRC.  Only works for IRC Operators."
  (interactive "sNuke which user? ")
  (let (match)
    (while (and (string< "" user)
                (/= (length (setq match (irc-check-list irc-wholist user))) 1))
      (setq user (read-string "Nuke which user? ")))
    (if (string< "" user)
        (irc-send (concat "KILL " (car match))))))

(defun irc-execute-invite (&optional user)
  "Ask a user on IRC to join the channel which you are on."
  (interactive)
  (let (match)
    (while (and (string< "" user)
                (/= (length (setq match (irc-check-list irc-wholist user))) 1))
      (setq user (read-string (format "Invite whom to channel %d? "
                                      irc-channel))))
    (if (string< "" user)
        (irc-send (concat "INVITE " (car match))))))

(defun irc-execute-ignore (&optional user)
  "Ignore another user on IRC.  Any events by this person (except for WALL)
are not displayed.  IRC-mode will track the ignored user across nickname
changes if it notices the change.  To undo this command, use /UNIGNORE."
  (interactive)
  (let (match)
    (while (and (string< "" user)
                (/= (length (setq match (irc-check-list irc-wholist user))) 1))
      (setq user (read-string "Ignore whom? ")))
    (if (string= "" user) ()
      (irc-insert "You are now ignoring %s." (car match))
      (irc-maintain-list 'irc-ignores (car match) 'add))))

(defun irc-execute-unignore (&optional user)
  "Stop ignoring a user."
  (interactive)
  (let (match)
    (while (and (string< "" user)
                (/= (length (setq match (irc-check-list irc-wholist user))) 1))
      (setq user (read-string "Stop ignoring whom? ")))
    (if (string= "" user) ()
      (irc-insert "You are no longer ignoring %s." (car match))
      (irc-maintain-list 'irc-ignores (car match) 'remove))))

(defun irc-execute-wall (&optional msg)
  "Send a message to everyone on IRC.  This can only be done by IRC operators."
  (interactive)
  (if (or (not msg) (string= "" msg))
      (setq msg (read-string "Message for everyone: ")))
  (if (string< "" msg) (irc-send (concat "WALL " msg))))

(defun irc-execute-signal (signals)
  "Set the events which will get signals (aks bells or dings) when they
occur.  Events supported are:

  private -- private messages      join   -- channel changing events
  public  -- public messages       invite -- invitation events
  wall    -- broadcast messages

Syntax is \"/SIGNAL [+ | on | - | off | [+]event [on] | -event [off]]\".
Without any arguments /SIGNAL simply prints a message about what signals
are currently enabled.  With event, +event or event on, turn on all
signalling for that event.  With -event or event off, remove all signals for
that event.  /SIGNAL + or /SIGNAL - adds or removes all signals respectively.
Prefixes of + or - have highest priority, making an expression like
\"+join off\" turn the signal on.  Trailing garbage is ignored if sense
can be made out of the first one or two arguments."
  (interactive "sSet signal: ")
  (let (event lead trail on off (recog '(private public wall invite join)))
    (string-match "^\\s *" signals)
    (setq signals (substring signals (match-end 0)
                             (string-match "\\s *$" signals)))
    (cond
     ((string= "" signals)
      (setq on (delq nil
                     (mapcar
                      (function
                       (lambda (arg)
                         (if (eq (nth 1 (assoc arg irc-signals)) t)
                             arg))) recog))
            off (delq nil
                      (mapcar
                       (function
                        (lambda (arg)
                          (if (eq (nth 1 (assoc arg irc-signals)) nil)
                              arg))) recog)))
      (if (and (not on))
          (irc-insert "All signalling is currently disabled.")
        (irc-insert (concat
                     (concat "Signalling is enabled for "
                             (irc-subst-comma
                              (mapconcat 'prin1-to-string on ", ") "and")
                             ".  ")
                     (if off
                         (concat "Signalling is disabled for "
                                 (irc-subst-comma
                                  (mapconcat 'prin1-to-string off ", ") "and")
                                 "."))))))
     ((or (string= "-" signals) (string= "off" (downcase signals)))
      (setq irc-signals (mapcar 'list recog))
      (irc-insert "All signalling has been disabled."))
     ((or (string= "+" signals) (string= "on" (downcase signals)))
      (setq irc-signals (mapcar (function (lambda (arg) (list arg t))) recog))
      (irc-insert "Signalling has been enabled for all events."))
     (t
      (setq lead (memq (aref signals 0) '(?+ ?-))
            event (substring signals (if lead 1 0)
                             (string-match "\\s +\\|$" signals))
            trail (substring signals (match-end 0)
                             (string-match "\\s +\\|$" signals (match-end 0)))
            event
            (car
             (delq nil
                   (mapcar
                    (function (lambda (arg)
                                (if (string-match
                                     (concat "^\\s *" (if (string= "" event)
                                                          trail event))
                                     (prin1-to-string arg)) arg))) recog)))
            off (if lead (= (aref signals 0) ?-)
                  (string= (downcase trail) "off"))
            on (if lead (not off)
                 (member-general (downcase trail) '("" "on") 'string=)))
      (if (or (not event) (not (or on off)))
          (irc-insert "Signal: unparsable arguments: %s" signals)
        (if off (setcdr (assoc event irc-signals) nil)
          (setcdr (assoc event irc-signals) '(t)))
        (irc-insert "Signalling has been %s for %s."
                    (if off "disabled" "enabled") event))))))

(defun irc-execute-help (topic)
  "Get the documentation for a given COMMAND.  With no command, list the
available commands as possible help topics."
  (interactive "sHelp for what command? ")
  (string-match "^\\s */?" topic)
  (setq topic (substring topic (match-end 0)
                         (string-match "\\s +\\|$" topic (match-end 0))))
  (if (string= topic "")
      (let (str (topics (mapcar 'car irc-command-alist)))
        (setq str "Help is available for the following IRC-mode commands:\n")
        (while topics
          (setq str
                (concat str
                        (format "\n%14s%14s%14s%14s%14s"
                                (nth 0 topics) (nth 1 topics) (nth 2 topics)
                                (nth 3 topics) (nth 4 topics)))
                topics (nthcdr 5 topics)))
        (with-output-to-temp-buffer "*Help*" (princ str)))
    (let ((matches
           (apply 'append
                  (mapcar (function
                           (lambda (dp)
                             (if (string-match (concat "^" topic) (car dp))
                                 (list dp)))) irc-command-alist))))
      (if (not matches)
          (irc-insert "Sorry, no help is available for %s." topic)
        (if (= (length matches) 1)
            (with-output-to-temp-buffer "*Help*"
              (princ
               (concat "IRC-mode command: /" (car (car matches)) "\n\n"
                       (documentation
                        (intern-soft
                         (concat "irc-execute-" (cdr (car matches))))))))
          (irc-insert
           (irc-subst-comma
            (concat "Ambiguous help topic; could be "
                    (mapconcat 'car matches ", ") ".") "or")))))))

;; miscellaneous
(defun irc-truncate-buffer (size)
  "Limit current buffer to SIZE bytes.  As many entire lines are stripped
from the beginning of the buffer as necessary to get it within bounds."
  (if (> (buffer-size) size)
      (if (save-excursion (goto-char (- (point-max) size))
                          (re-search-forward "\n" nil t))
          (delete-region 1 (match-end 0))
        (message "Warning: buffer exceeding max size.  Couldn't truncate."))))

(defun irc-read-passwd (&optional prompt)
    "Allow user to type a string without it showing.  Returns string.
If optional PROMPT non-nil, use it as the prompt string in the mini-buffer.
Not meant to be called by the user explicitly."
  (let ((passwd "") (echo-keystrokes 0) char)
    (if prompt (message prompt))
    (while (not (or (= (setq char (read-char)) 13) (= char 10)))
      (if (or (= char 8) (= char 127))
          (if (> (length passwd) 0)
              (setq passwd (substring passwd 0 (1- (length passwd)))))
        (setq passwd (concat passwd (char-to-string char))))
      (if prompt (message (concat prompt (make-string (length passwd) ?*)))))
    (if prompt (message ""))
    passwd))

;; member-general by Bard Bloom <bard@theory.lcs.mit.com>
(defun member-general (x l comparison)
  "Is X a member of L under COMPARISON?"
  (let ((not-found t))
    (while (and l not-found)
      (setq not-found (not (funcall comparison x (car l)))
            l         (cdr-safe l)))
    (not not-found)))

(defun irc-signal (user event)
  "Return t if a ding should be issued for a USER/EVENT pair.
Currently only the event part of things is supported by /SIGNAL."
  (let ((signal (cdr (assoc event irc-signals))))
    (or (memq t signal) (member-general user signal 'string=)
        (member-general user (cdr (assoc 'user irc-signals)) 'string=))))

(defun irc-check-list (list item)
  "See if LIST has string ITEM.  Returns a list of possible matches.  The list
returned is based on the following precedence rules:  if there is an exact
match, it is returned.  If there are any strings in the list whose beginning
match the item, they are returned.  If that fails, strings which have the item
match anywhere are returned.  As a last resort, nil is returned."
  (let (return)
    (if (setq return
              (delq nil
                    (mapcar (function
                             (lambda (arg)
                               (if (string-match (concat "^" item "$") arg)
                                   arg))) list))) return
      (if (setq return
                (delq nil
                      (mapcar (function
                               (lambda (arg)
                                 (if (string-match (concat "^" item) arg)
                                     arg))) list))) return
        (delq nil
              (mapcar (function
                       (lambda (arg)
                         (if (string-match (concat "." item) arg) arg)))
                      list))))))

(defun irc-maintain-list (list item func)
  "Maintain a LIST of strings by adding or removing string ITEM.
Second argument FUNC should be 'add or t or to make sure the user is in
the list or 'remove or nil to make sure user is out of the list."
  (cond
   ((memq func '(add t))
    (or (member-general item (eval list) 'string=)
        (set list (cons item (eval list)))))
   ((memq func '(remove nil))
    (set list
         (delq nil (mapcar (function (lambda (arg)
                                       (if (string= item arg) nil arg)))
                           (eval list)))))))

(defun irc-burst-comma (str &optional and-spaces)
  "Take a comma-separated STR and turn it into a list of its elements.
It bursts on spaces or comma-space pairs."
  (let (list sub (beg 0))
    (string-match "" str)
    (while (string-match "\\(,+\\|\\s +\\|,+\\s +\\)" str beg)
      (if (not (string= (setq sub (substring str beg (match-beginning 0))) ""))
          (setq list (cons sub list)))
      (setq beg (match-end 0)))
    (if (/= (length str) beg) (cons (substring str beg) list) list)))

;; wish i could remember who I got this from; I had to patch it to work
;; with the minibuffer correctly but it is mostly untouched.
(defun walk-windows (proc &optional no-mini)
  "Applies PROC to each visible window (after selecting it, for convenience).
Optional arg NO-MINI non-nil means don't apply PROC to the minibuffer
even if it is active."
  (let* ((real-start (selected-window))
          (start (next-window real-start no-mini))
          (current start) done)
     (while (not done)
       (select-window current)
       (funcall proc)
       (setq current (next-window current no-mini))
       (setq done (eq current start)))
     (select-window real-start)))

(defun count-windows (&optional no-mini)
   "Returns the number of visible windows.
Optional arg NO-MINI non-nil means don't count the minibuffer
even if it is active."
   (let ((count 0))
     (walk-windows (function (lambda () (setq count (1+ count)))) no-mini)
     count))

;; swiped from minibuf.el, but made exclusive to * Minibuf-n*.
(defun minibuffer-message (format &rest args)
  "Print a temporary message at the end of the Minibuffer.
After 2 seconds or when a key is typed, erase it."
  (if (zerop (minibuffer-depth)) (apply 'message format args)
    (let (p)
      (save-excursion
        (set-buffer (concat " *Minibuf-" (1- (minibuffer-depth)) "*"))
        (unwind-protect
            (progn
              (setq p (goto-char (point-max)))
              (insert (apply 'format format args))
              (sit-for 2))
          (delete-region p (point-max)))))))

(defun irc-find-to (str &optional explicit)
  "Find the part of STRING that Irc will interpret as the sendlist.
If no explicit list is found, irc-default-to is returned.
The string returned is either : or ; terminated.

If optional EXPLICIT is non-nil, then return t if a sendlist was explicitly
specified, nil if the sendlist was implicit."
  (let ((matched (string-match "^[A-Za-z0-9-,*]*[;:]" str)))
    (if matched (if explicit t (substring str 0 (match-end 0)))
      (if explicit nil irc-default-to))))

(defun irc-find-message (string)
  "Find the message that Irc will see if STR were sent.  For messages
sent with explicit lists, this is everything following the colon or
semi-colon.  For everything else, it is just the string."
  (substring string (length (irc-find-to string))))

;; moving through the history
(defun irc-add-to-hist (str)
  "Put STRing at the head of the irc-hist list."
  (if (string-match "^[:;]" str)
      (setq str
            (concat irc-last-explicit (substring str 1 (length str)))))
  (setq irc-hist (append (list str) irc-hist))
  (and (> (length irc-hist) irc-max-history)
       (setq irc-hist (reverse (cdr (reverse irc-hist))))))

(defun irc-yank-prev-command ()
  "Put the last IRC /command in the input-region."
  (interactive)
  (delete-region irc-mark (goto-char (point-max)))
  (insert "/" irc-last-command)
  (goto-char (1+ irc-mark)))

(defun irc-hist-prev (arg)
  "Select the previous message in the IRC history list.  ARG means
select that message out of the list (0 is the first)."
  (interactive "P")
  (let ((str (nth (or arg (1+ irc-hist-index)) irc-hist)))
    (if (not str)
        (message "No message %d in history." (or arg (1+ irc-hist-index)))
      (delete-region irc-mark (goto-char (point-max)))
      (insert str)
      (goto-char irc-mark)
      (setq irc-hist-index (or arg (1+ irc-hist-index))))))

(defun irc-hist-next (arg)
  "Select the next message in the IRC history list.  With prefix ARG
select that message out of the list (same as irc-hist-prev if
called with a prefix arg)."
  (interactive "P")
  (if arg (irc-hist-prev arg)
    (if (= irc-hist-index -1)
        (message "No next message in history.")
      (delete-region irc-mark (goto-char (point-max)))
      (insert (if (zerop irc-hist-index) ""
                (nth (1- irc-hist-index) irc-hist)))
      (setq irc-hist-index (1- irc-hist-index)))))

(defun irc-kill-input ()
  "Delete the input region and start out fresh.  This function is recommended
over any other way of killing the input-region interactively because it
also resets the index for the history list."
  (interactive)
  (delete-region irc-mark (goto-char (point-max)))
  (setq irc-hist-index -1))

(defun irc-history-menu ()
  "List the history of messages kept by irc-mode in another buffer."
  (interactive)
  (let ((pop-up-windows t) (hist irc-hist) (line 0))
    (save-excursion
      (set-buffer (get-buffer-create "*IRC History*"))
      (fundamental-mode)
      (erase-buffer)
      (while hist
        (insert (format "%2d: %s\n" line (car hist)))
        (setq hist (cdr hist))
        (setq line (1+ line)))
      (if (zerop line)
          (insert "No messages have been sent to IRC yet."))
      (set-buffer-modified-p nil)
      (goto-char (point-min)))
    (display-buffer "*IRC History*")))

;; stuff about irc-mode
(defun irc-version (&optional arg)
  "Print the current version of irc.el in the minibuffer.  With optional
ARG, insert it in the current buffer."
  (interactive "P")
  (if arg (insert irc-version) (princ irc-version)))

(defun irc-news ()
  "Shows news about latest changes to irc.el.  Even shows news about
old changes to irc.el -- what a wonderous function indeed."
  (interactive)
  (save-excursion
    (set-buffer (get-buffer-create "*IRC-mode News*"))
    (erase-buffer)
    (insert "Latest changes to Irc mode:

For functions, use C-h f to describe the function.  Variables use C-h v.

22 June 89 -- I would have to put everything in here for the first entry in
   order to have something to reference against \"Latest changes\".  I
   shan't do that.  This first entry shall just say that this is IRC-mode
   version 1.0Gimel, in its first distribution to testers.  Play with the
   /HELP command to see what it can offer you.  Send comments/bug reports
   to me (Dave Lawrence, <tale@pawl.rpi.edu>) and I will try to tend to 
   them as quickly as possible.

   One thing to note that I don't think is documented in any functions is
   is how sending works.  Basically, any line that you send which does not
   start with a / gets processed in the following way: if you have a string
   which is all alpha-numeric characters (with items seperated by commas)
   up to and including a colon or semi-colon, it is called your explicit
   sendlist.  The elements of that list will be processed for sending to the
   recipients you name.  Partial matching of names is also done.  If a line
   starts with a colon or semi-colon then that last used explicit sendlist
   is considered the sendlist for the message.  If no such prefix list exists
   then something called your implicit sendlist is used.  This list is set
   with /SEND (aka /QUERY).  In all of these lists the character \"*\" is
   recognized as meaning \"whatever channel I happen to be on when I am
   sending\".
   
   Look at some of the bindings in C-h m (describe-mode).  Since C-p is a
   normal movement command, scrolling through your message history is
   instead bound to C-c C-p (and C-c C-n for going the other way).  C-c C-u
   will always kill the current line of input no matter what your position.
   While a user-variable (irc-max-history) is available to determine how
   many messages should be retained in the history, only the last command
   is remembered for command-history.  It is available with C-c p.

   One last suggestion -- try M-x edit-options followed by a search through
   the options buffer for irc.  This should put you in an area where the
   available irc user-variables are all together, with their documentation
   strings and current values.

   I hope you enjoy my implementation of an IRC client.")
    (goto-char (point-min)))
  (display-buffer "*IRC-mode News*"))


