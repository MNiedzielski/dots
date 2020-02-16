; -*-Emacs-Lisp-*-
;;; GNU Emacs server support code
;;;
;;; This file is not part of GNU Emacs (yet).
;;; Copyright (c) 1989 Andrew P. Norman & FSF.
;;;
;;; Copying is permitted under those conditions described by the GNU
;;; Emacs General Public License.
;;;
;;; Author: Andy Norman (ange%anorman@hplabs.hp.com)
;;;
;;; Please mail bugs and suggestions to the author at the above address.
;;;

(defvar server-program "gnuserv"
  "*The program to use as the edit server")

(defvar server-process nil 
  "the current server process")

(defvar server-string ""
  "the last input string from the server")

(defvar current-client nil
  "the client we are currently talking to")

(defvar server-clients nil
  "List of current server clients.
Each element is (CLIENTID BUFFER...) where CLIENTID is an integer
that can be given to the server process to identify a client.
When a buffer is killed, it is removed from this list.")

(defvar server-buffer-clients nil
  "List of clientids for clients requesting editing of current buffer.")

(make-variable-buffer-local 'server-buffer-clients)
(setq-default server-buffer-clients nil)
(or (assq 'server-buffer-clients minor-mode-alist)
    (setq minor-mode-alist (cons '(server-buffer-clients " Server") minor-mode-alist)))

;; If a *server* buffer exists,
;; write STRING to it for logging purposes.
(defun server-log (string)
  (if (get-buffer "*server*")
      (save-excursion
	(set-buffer "*server*")
	(goto-char (point-max))
	(insert string)
	(or (bobp) (newline)))))


(defun server-sentinel (proc msg)
  (cond ((eq (process-status proc) 'exit)
	 (server-log (message "Server subprocess exited")))
	((eq (process-status proc) 'signal)
	 (server-log (message "Server subprocess killed")))))


(defun server-process-filter (proc string)
  "Process incoming requests for gnu emacs to do some actions."
  (setq server-string (concat server-string string))
  (if (string-match "\n$" server-string) ;wait till ends with a newline
      (progn
	(server-log server-string)
	(let ((header (read-from-string server-string)))
	  (setq current-client (car header))
	  (condition-case oops
	      (eval (car (read-from-string server-string (cdr header))))
	    (error (setq server-string "")
		   (server-write-to-client current-client oops)
		   (setq current-client nil))
	    (quit (setq server-string "")
		  (server-write-to-client current-client oops)
		  (setq current-client nil)
		  (signal 'quit nil)))
	  (setq server-string "")))))


(defun server-kill-outstanding-buffers ()
  "Zap all buffers that have clients waiting for them to be finished."
  (interactive)
  (while server-clients
    (let ((buffer (nth 1 (car server-clients)))) ;need to do this for all buffers
      (server-kill-buffer buffer))))	; destructively modifies server-clients


(defun server-start (&optional leave-dead)
  "Allow this Emacs process to be a server for client processes.
This starts a server communications subprocess through which
client \"editors\" can send editing commands to this Emacs job.

Prefix arg means just kill any existing server communications subprocess."
  (interactive "P")
  ;; kill it dead!
  (if server-process
      (progn
	(server-kill-outstanding-buffers)
	(set-process-sentinel server-process nil)
	(condition-case ()
	    (delete-process server-process)
	  (error nil))))
  ;; If we already had a server, clear out associated status.
  (if leave-dead
      nil
    (if server-process
	(server-log (message "Restarting server")))
    (setq server-string "")
    (setq current-client nil)
    (let ((process-connection-type t))
      (setq server-process (start-process "server" nil server-program)))
    (set-process-sentinel server-process 'server-sentinel)
    (set-process-filter server-process 'server-process-filter)
    (process-kill-without-query server-process)))


(defun server-write-to-client (client form)
  "Write the given form to the given client via the server process."
  (if (and client
	   (eq (process-status server-process) 'run))
      (let ((s (format "%s:%s\n" client form)))
	(send-string server-process s)
	(server-log s))))


(defun server-eval (form)
  "Evaluate form and return result to client."
  (server-write-to-client current-client (eval form)))

(defun server-eval-quickly (form)
  "Let client know that we've received the request, but eval the form
afterwards in order to not keep the client waiting."
  (server-write-to-client current-client nil)
  (setq current-client nil)
  (eval form))

(defun server-make-window-visible ()
  "Try to make this window even more visible."
  (if (and (boundp 'window-system)
	   (boundp 'window-system-version)
	   (eq window-system 'x)
	   (eq window-system-version 11)
	   (fboundp 'x-remap-window))
      (x-remap-window)))


(defun server-edit-file (lineno path)
  "Edit the given file for the client and save enough information such that
server-kill-buffer can let the client know when the buffer has been finished
with."
  (find-file path)
  (let ((old-clients (assq current-client server-clients))
	(buffer (current-buffer)))
    (goto-line lineno)
    (setq server-buffer-clients
	  (cons current-client server-buffer-clients))
    (if old-clients			;client already waiting for buffers?
	(nconc old-clients (list buffer)) ;yes -- append this one as well
      (setq server-clients		;nope -- make a new record
	    (cons (list current-client buffer)
		  server-clients)))
    (server-make-window-visible)))


(defun server-edit-file-quickly (lineno path)
  "Edit the given file for the client and goto the given line number. 
Note that unlike server-edit-file, no information is saved about clients
waiting for this buffer to be killed."
  (find-file path)
  (goto-line lineno)
  (server-make-window-visible))


(defun server-edit-file-quickly-done ()
  "Let the client know that emacs has received the preceeding requests
to edit file(s) quickly via server-edit-file-quickly."
  (server-write-to-client current-client nil))


(defun server-kill-buffer (buffer)
  "One arg, a string or a buffer.  Get rid of the specified buffer.
Note that this function has been enhanced to allow for remote editing
in the following way:

If the buffer is waited upon by one or more clients, and a client is
not waiting for other buffers to be killed, then the server is told to
tell that client that the buffer has been killed. The buffer is then
killed and another buffer is selected which is preferably one that has
a client waiting on it."
  (interactive "bKill buffer ")
  (setq buffer (get-buffer buffer))	; make sure it's a real buffer object
  (save-excursion
    (set-buffer buffer)
    (let ((old-clients server-clients))
      (real-kill-buffer buffer)		;try to kill it
      (if (buffer-name buffer)		;succeeded in killing?
	  nil 				;nope
	  (while old-clients
	    (let ((client (car old-clients)))
	      (delq buffer client)
	      (if (cdr client)		;pending buffers?
		  nil			;yep
		(server-write-to-client (car client) nil) ;nope, tell client
		(setq server-clients (delq client server-clients))))
	    (setq old-clients (cdr old-clients))))))
  (if (not (buffer-name buffer))	;try to select another client buffer
      (if server-clients
	  (switch-to-buffer (nth 1 (car server-clients))))))


(defun server-kill-all-local-variables ()
  "Eliminate all the buffer-local variable values of the current buffer.
This buffer will then see the default values of all variables.
NOTE: This function has been modified to ignore the variable 
server-buffer-clients."
  (let ((clients server-buffer-clients))
    (real-kill-all-local-variables)
    (if clients
	(setq server-buffer-clients clients))))


(or (fboundp 'real-kill-buffer)
  (fset 'real-kill-buffer (symbol-function 'kill-buffer)))

(fset 'kill-buffer 'server-kill-buffer)

(or (fboundp 'real-kill-all-local-variables)
    (fset 'real-kill-all-local-variables
	  (symbol-function 'kill-all-local-variables)))

(fset 'kill-all-local-variables 'server-kill-all-local-variables)
