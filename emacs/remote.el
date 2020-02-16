;; remote.el version 3.0
;;
;; Remote editing via rcp.
;; Original idea from Nick Tran
;; Total rewrite by Eric Raible (raible@orville.nas.nasa.gov)
;;
;; This file is distributed under the same terms as GNU Emacs.
;;

(provide 'remote)

(defvar default-remote-host "acadia:"
 "The host to use for remote file operations when none other is appropriate.")

(defvar track-default-remote-host t
  "Controls whether  default-remote-host  changes.
When nil,  default-remote-host  keep its value, otherwise it will always
have the value of the last remote host successfully read.")

(defvar remote-directory-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map " "  'remote-directory-complete)
    (define-key map "\t" 'remote-directory-complete)
    (define-key map "?"  'remote-directory-complete)
    (define-key map "\e" 'remote-directory-set-host)
    map)
  "Map used by read-remote-file-name-with-completion.")

(make-variable-buffer-local 'buffer-remote-file-name)
(set-default 'buffer-remote-file-name "")
(make-variable-buffer-local 'remote-editing)

(defvar remote-shell
  (cond ((eq system-type 'hpux) "remsh")
	(t "rsh"))
  "Name of the command to invoke a remote shell.")

(defvar rcp "rcp"
  "Name of the command to do a remote copy")

(if (not (assoc 'remote-editing minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(remote-editing " Remote") minor-mode-alist)))

(defun remote-editing (arg)
  "Toggle remote-editing mode.
With arg, turn on remote editing mode iff arg is positive, otherwise toggle.

In remote editing mode, the normal bindings for find-file,
find-file-read-only, find-alternate-file, save-buffer, write-file,
and insert-file are changed to operate on a remote system by default.

When remote editing, a prefix arg allows local file operations.  When not
remote editing, a prefix arg allows remote file operations.

It is assumed that .rhosts files are set up properly on both machines."
  (interactive "P")
  (setq remote-editing
	(if (null arg) (not remote-editing)
	  (> (prefix-numeric-value arg) 0)))
  (set-buffer-modified-p (buffer-modified-p))) ;No-op, but updates mode line.

;;;
;;; Macro used as front-end to normal file operation key bindings to decide
;;; between local and remote editing modes.
;;;
;;; Automatically constructs doc string and includes prefix arg hacking
;;; to temporarily toggle sense of remote-editing.
;;;

;;; defaultlocal idea by wjc@ho5cad.att.com (Bill Carpenter)
(defmacro def-local-or-remote (binding name remote defaultlocal)
  (if (eq (key-binding binding) name)	; already done - avoid infinite loop
      nil
    (let* ((local (or (key-binding binding) defaultlocal))
	   (r (symbol-name remote))
	   (l (symbol-name local)))
      (list 'progn
	    (list 'global-set-key binding (list 'quote name))
	    (list 'defun name '(arg)
		  (concat "Call either " r " or " l ".
If remote-editing (which see), call " r ",
else call " l ".

See also the documentation for " r "
and " l ".")
		  '(interactive "P")
		  (list 'call-interactively
			(list 'if '(xor remote-editing arg)
			      (list 'quote remote)
			      (list 'quote local))))))))

(def-local-or-remote "\C-x\C-f"	find-local-or-remote-file
  find-remote-file		find-file)

(def-local-or-remote "\C-x\C-r"	find-local-or-remote-file-read-only
  find-remote-file-read-only	find-file-read-only)

(def-local-or-remote "\C-x\C-v"	find-alternate-local-or-remote-file
  find-alternate-remote-file	find-alternate-file)

(def-local-or-remote "\C-x\C-s"	save-local-or-remote-buffer
  save-remote-buffer		save-buffer)

(def-local-or-remote "\C-x\C-w"	write-local-or-remote-file
  write-remote-file		write-file)

(def-local-or-remote "\C-xi"	insert-local-or-remote-file
  insert-remote-file		insert-file)

(defun find-remote-file (host file)
  "Edit remote file HOST:FILE (using rcp).
This command is similiar to find-file, but uses rcp to read the file from
a remote machine.  Also see remote-editing."
  (interactive (read-remote-file-name "Find remote file"))
  (let ((f-or-b (get-remote-file-or-buffer host file "retrieve"))
	local-file)
    (if f-or-b
	(if (bufferp f-or-b)
	    (switch-to-buffer f-or-b)
	  (setq local-file f-or-b)
	  (let ((buf (generate-new-buffer
		      (concat host (file-name-nondirectory file)))))
	    (switch-to-buffer buf)
	    (if (not (file-exists-p local-file))
		(message "(New remote file)")
	      (insert-file-contents local-file)
	      (set-buffer-modified-p nil)
	      (delete-file local-file))
	    ;; dynamic binding for normal-mode
	    (let ((buffer-file-name (concat host file)))
	      (normal-mode)
	      (remote-editing 1)
	      (setq buffer-remote-file-name buffer-file-name
		    buffer-offer-save t)))))))

(defun find-remote-file-read-only ()
  "Edit remote file FILENAME, but mark buffer as read-only.
Also see find-remote-file and remote-editing."
  (interactive)
  (call-interactively 'find-remote-file)
  (setq buffer-read-only t))

(defun find-alternate-remote-file ()
  "Find alternate file using rcp.
This command is similiar to find-alternate-file, but uses rcp to fetch
the file from a remote machine.  Also see remote-editing."
  (interactive)
  (and (buffer-modified-p)
       (not buffer-read-only)
       (not (yes-or-no-p (format "Buffer %s is modified; kill anyway? "
				 (buffer-name))))
       (error "Aborted"))
  (let ((obuf (current-buffer))
	(oname (buffer-name)))
    (rename-buffer " **lose**")
    (unwind-protect
	(apply 'find-remote-file
	       (read-remote-file-name "Find remote alternate file"))
      (if (eq obuf (current-buffer))
	  (rename-buffer oname)
	(kill-buffer obuf)))))

(defun save-remote-buffer ()
  "Save a file using rcp.
This command is similiar to save-buffer, but uses rcp to write the file back
to a remote machine.  Also see remote-editing."
  (interactive)
  (if (buffer-modified-p)
      (if (zerop (length buffer-remote-file-name))
	  (call-interactively 'write-remote-file)
	(do-write-remote-file buffer-remote-file-name))
    (message "(No changes need to be saved)")))

(defun write-remote-file (host file)
  "Write a file HOST:FILE using rcp.
This command is similiar to write-file, but uses rcp to write the file back
to a remote machine.  Also see remote-editing."
  (interactive (read-remote-file-name "Write remote file" 'no-file-ok))
  (do-write-remote-file (concat host file)))

(defun insert-remote-file (host file)
  "Insert a remote file HOST:FILE using rcp.
This command is similiar to insert-file, but uses rcp to read the file from
a remote machine.  Also see remote-editing."
  (interactive (read-remote-file-name "Insert remote file"))
  (let ((f-or-b (get-remote-file-or-buffer host file "insert")))
    (if f-or-b
	(if (bufferp f-or-b)
	    (insert-buffer f-or-b)
	  (insert-file f-or-b)
	  (delete-file f-or-b)))))

;;;
;;; Internal routines
;;;

(defun do-write-remote-file (file)
  (let* ((temp (concat "/tmp/" (buffer-name)))
	 (output (save-excursion
		   (prog1 (set-buffer (get-buffer-create "*Rcp Output*"))
		     (erase-buffer))))
	 (cursor-in-echo-area t)
	 time)
    ;; write-file doesn't quite do it.
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) temp nil 'no-message))
    (message "Sending %s..." file)
    (if (setq time (remote-process-wait
		    (start-process "rcp" output rcp temp file)))
	(progn
	  (if remote-editing
	      (let ((new-name
		     (concat (remote-filename-host-part file)
			     (file-name-nondirectory
			      (remote-filename-file-part file)))))
		(or (get-buffer new-name) (rename-buffer new-name))
		(set-buffer-modified-p nil)))
	  (setq buffer-remote-file-name file)
	  (message "%d bytes in %d seconds" (buffer-size) time)
	  (delete-file temp))
      (remote-rcp-error output buffer-remote-file-name "update"))))

(defun get-remote-file-or-buffer (host file message)
  "Return a remote file as either a buffer or a file.
If the file HOST:FILE already has been read in, return the buffer
that contains it; otherwise try and rcp the file to the local machine.
If successful, return the local file name."
  (let ((remote (concat host file))
	(temp (concat "/tmp/" (file-name-nondirectory file))))
    (if (file-directory-p file)
	(progn
	  (message "Remote directory listing not yet implemented")
	  nil)
      (or (get-remote-buffer remote)	  ;; already exists
	  (let* ((output (save-excursion
			   (prog1 (set-buffer (get-buffer-create
					       "*Rcp Output*"))
			     (erase-buffer))))
		 (cursor-in-echo-area t)
		 time)
	    (message "Retrieving %s..." remote)
	    (if (setq time (remote-process-wait
			    (start-process "rcp" output rcp remote temp)))
		(progn
		  (message "%d bytes in %d seconds"
			   (nth 7 (file-attributes temp)) time)
		  temp)
	      (remote-rcp-error output remote message)))))))

(defun get-remote-buffer (name)
  (save-window-excursion
    (let ((buffers (buffer-list)) found)
      (while (and (not found) buffers)
	(set-buffer (car buffers))
	(if (string= name buffer-remote-file-name)
	    (setq found (car buffers)))
	(setq buffers (cdr buffers)))
      found)))

(defun read-remote-file-name (prompt &optional no-file-ok)
  "Read a remote file specification, and return list (host file).
Prompting with PROMPT, get a file name.  The initial host defaults
to a value is derived from buffer-remote-file-name, or if there is
none, then from the global default (default-remote-host).

The host can be changed by hitting an ESC."
  (let* ((host (or (remote-filename-host-part buffer-remote-file-name)
		   default-remote-host))
	 (file (file-name-directory
		(or (remote-filename-file-part buffer-remote-file-name) ""))))
    (setq host (strip-trailing-colons host))
    (let* ((res (read-remote-file-name-with-completion prompt host file))
	   (host (nth 0 res))
	   (file (nth 1 res)))
      (and track-default-remote-host
	   (setq default-remote-host host))
      (list host
	    (if (or (null file) (string= file (file-name-directory file)))
		(concat file
			(or (if (not (string= buffer-remote-file-name ""))
				(file-name-nondirectory
				 (remote-filename-file-part
				  buffer-remote-file-name)))
			    (remote-filename-file-part (buffer-name))
			    (buffer-name)))
	      file)))))

(defun read-remote-file-name-with-completion (prompt host file)
  (let ((cursor-in-echo-area t))
    (message "Hit esc to change the default host")
    (sit-for 1))
  (while (string= ""
		  (setq file
			(read-from-minibuffer
			 (concat prompt " (on " host "): ")
			 file remote-directory-map))))
  (list (concat host ":")
	(if (last-char-is file ?*)
	    (substring file 0 -1)
	  file)))

(defvar remote-dir-info nil
  "Holds the completion table information for read-remote-file-name.
Don't change this.")

(defun remote-directory-set-host ()
  "Internal routine for read-remote-file-name.  Don't call."
  (interactive)
  (let* ((enable-recursive-minibuffers t)
	 (new (read-string (format "New host (default %s): " host))))
    (if (string= new "")
	()
      (setq host (strip-trailing-colons new)
					; New host =>
	    remote-dir-info ())		;  must invalidate remote-dir-info,
      (erase-buffer)			;  current file name,
      (exit-minibuffer))))		;  and try again

(defun remote-directory-complete ()
  "Internal routine for read-remote-file-name.  Don't call.
Note that this routine uses the dynamic binding of host."
  (interactive)
  (let ((file (buffer-substring (point-min) (point-max))))
    (if (string-match "^~" file)
	(progn
	  (erase-buffer)
	  (setq file (remote-expand-~ file))
	  (insert file)))
    (if (or (null remote-dir-info)		       ; no table
	    (not (equal (nth 0 remote-dir-info) host)) ; out-of-date host
	    (not (eq 0 (string-match		       ; out-of-date file
			(regexp-quote (nth 1 remote-dir-info)) file)))
	    (string-match			       ; a new directory
	     "/" (substring file (length (nth 1 remote-dir-info))))
	    )
	(setq remote-dir-info
	      (list host file (remote-directory-list host file))))
    (let* ((minibuffer-completion-table
	    (mapcar 'list 
		    (all-completions file (nth 2 remote-dir-info))))
	   (length (length minibuffer-completion-table)))
      (cond ((= length 0)
	     (let ((minibuffer-completion-table '(())))
	       (minibuffer-complete)))
	    ((= length 1)
	     (minibuffer-complete))
	    (t (minibuffer-completion-help)
	       (minibuffer-complete))))))

(defun remote-expand-~ (what)
  (let ((proc (start-process "expand" () remote-shell host "echo" what))
	(output)
	(cursor-in-echo-area t))
    (message (format "Querying %s to expand %s" host what))
    (set-process-filter proc
			'(lambda (proc string)
			   (setq output string)))
    (accept-process-output proc)
    (substring output 0 -1)))		; remove newline

(defun remote-directory-list (host file)
  (let* ((cursor-in-echo-area t)
	 (process)
	 (host (substring host 0 (string-match ":" host)))
	 (remote-directory-output ""))	; dynamic binding for filter
    (message "Remote directory listing of %s:%s (be patient)" host file)
    ;;
    ;; System dependent - we want a listing that gives all completions, but
    ;; leaves a slash ("/") at the end of directories.
    ;;
    (setq file (concat file "*"))
    (setq process (start-process
		   "Remote Directory Listing"
		   nil
		   remote-shell
		   host
		   "/bin/ls"
		   "-dF"
		   file))
    (set-process-filter process 'remote-directory-filter)
    (if (remote-process-wait process)
	(or (parse-remote-output remote-directory-output) (list nil))
      (error "Couldn't get a remote listing of %s:%s" host file))))

(defun remote-directory-filter (process output)
  ;; remote-directory-output dynamically bound in remote-directory-list
  (setq remote-directory-output (concat remote-directory-output output)))

(defun parse-remote-output (output)
  (let ((case-fold-search t)
	(result ()))
    (if (or (string-match "no match" output)
	    (string-match "not found" output)
	    (string-match "no such file or directory" output))
	()
      (while (string-match "\012" output)
	;; cons the output into a list -
	(setq result (cons (list (substring output 0 (match-beginning 0)))
			   result)
	      output (substring output (match-end 0)))))
    result))

(defun last-char-is (string char)
  "True if last character of STRING is CHAR."
  (and (> (length string) 0)
       (= (aref string (1- (length string)))
	  char)))

(defun strip-trailing-colons (host)
  (while (last-char-is host ?:)
    (setq host (substring host 0 -1)))
  host)

(defun remote-filename-host-part (name)
  (if (string-match ".+:" name)
      (substring name 0 (match-end 0))))

(defun remote-filename-file-part (name)
  (if (string-match ".+:\\(.+\\)" name)
      (substring name (match-beginning 1) (match-end 1))))

(defun xor (a b)
  (eq (null a) (not (null b))))

(defun remote-process-wait (proc)
  (condition-case ignore
      (let ((time 0))
	(while (eq (process-status proc) 'run)
	  (setq time (1+ time))
	  (sleep-for 1))
	(if (and (eq (process-status proc) 'exit)
		 (eq (process-exit-status proc) 0))
	    time
	  nil))
    (quit (progn (kill-process proc)
		 nil))))

(defun remote-rcp-error (buffer file-name message)
  (save-window-excursion
    (switch-to-buffer buffer)
    (delete-other-windows)
    (goto-char 1)
    (insert (format "Unable to %s %s\n\n" message file-name))
    (goto-char (point-max))
    (message "Hit any character to continue")
    (read-char)
    (bury-buffer buffer)))


