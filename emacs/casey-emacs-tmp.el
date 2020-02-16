;;;; Time-stamp: <2002-07-23 14:17:56 crabbkw>
;;;;
;;;; a dot emacs file
;;;;
(defmacro when-emacs-version (vers &rest body)
  `(when (equal emacs-major-version ,vers)
    ,@body))

(setq default-buffer-file-coding-system 'undecided-unix)

(setq load-path ; For great justice, add ~/elisp to our load-path.
      (append (list (expand-file-name "~/elisp")
                    (expand-file-name "~/elisp/speedbar")
                    (expand-file-name "~/elisp/semantic")
                    (expand-file-name "~/elisp/eieio")
                    (expand-file-name "~/elisp/elib")
                    (expand-file-name "~/elisp/csharp")
                    (expand-file-name "~/elisp/psgml"))
              load-path))

;;;
;;; Primary functionality group
;;;
(setq inhibit-startup-message t)    ; no stupid messages about who did what
;(pc-bindings-mode)                 ; turn on good key mode.
;(delete-selection-mode)            ; let me delete what I have selected easily

(setq scroll-step 1)           ; Scroll by 1 line damnit!
(setq scroll-conservatively 1) ; I really mean it!
(setq scroll-up-aggressively .00000001) ; why does it have to be so hard to scroll by one?
(setq scroll-down-aggressively .00000001) ; grrr

(setq insert-default-directory nil) ; no CWD in minibuffer
(line-number-mode 1)                ; Line number please
(column-number-mode 1)              ; Column number please
(show-paren-mode t)                 ; Show me the parens.
(temp-buffer-resize-mode t)         ; Size the tempbuffer according to contents.
(resize-minibuffer-mode)            ; Size the minibuffer according to contents.
(setq display-time-day-and-date t)
(display-time)

;;; buffer switching stuff
(when-emacs-version 21
                    (progn
                      (iswitchb-default-keybindings) ; Show me my completions please
                      (iswitchb-mode 1)))            ; Show me my completions please
(setq iswitchb-case nil) ; completions are case sensitive.


(when-emacs-version 21
                    ;; Handle ANSI color sequences nicely.
                    (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
                    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))




;; File Manipulation stuff

(setq require-final-newline t) ; Force emacs to add a newline to the end of the file if there isn't one there.
(setq-default indent-tabs-mode nil) ; changes tabs into spaces.
(setq next-line-add-newlines nil)   ; don't let me add newlines at the EOF
(add-hook 'write-file-hooks 'time-stamp) ; Add time stamps

;; Auto-fill stuff
;(setq-default auto-fill-function 'do-auto-fill) ; Make everything auto-fill by default.
;(setq-default fill-column 128)                  ; set it to wrap at 128 characters.
;(setq-default auto-fill-function 'c-null)



;; Remove the annoying redundancy. When I type `y', I mean `y'.
(fset 'yes-or-no-p 'y-or-n-p)


;;;
;;; Backup files in one spot
;;;
(setq backup-directory-alist nil)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/elisp/eback"))
            backup-directory-alist))


;;;
;;; GNUServ stuff
;;;
;(require 'gnuserv)
;(if (equal 'w32 window-system)
;    (progn
;      (gnuserv-start)
;      (setq gnuserv-frame (selected-frame))))

(when (and (equal window-system 'w32)
           (equal emacs-major-version 21))
  (defun fix-cursor ()
    "Fix the Emacs 21 cursor issue"
    (interactive)
    (set-cursor-color "medium turquoise"))
(add-hook 'emacs-startup-hook 'fix-cursor))


;;;
;;; Keybindings
;;;
(defun c-time-stamp () ; output a timestamp
  (interactive)
    (progn
      (insert (time-stamp-dd-mon-yy))
      (insert " ")
      (insert (time-stamp-hh:mm:ss))
      (insert " ")))
;;; Functions which let me switch between wrap and truncate quickly
(defun c-wrap ()
  (interactive)
  (setq truncate-lines nil))

(defun c-truncate ()
  (interactive)
  (setq truncate-lines 't))

(defun c-null ()
  (interactive))

(defun c-insert-xhtml-heading ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    (insert "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n")
    (insert "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\n")))


(defun fix-stupid-backspace-key-issue ()
  "Fix this dumb terminal. Ugh."
  (interactive)
  (unless window-system ; No need to do this in them. :)
    ;; The following two lines are from the `Keyboard Translations' page
    ;; of the Emacs manual.
    ;; Translate `C-h' to <DEL>.
    (keyboard-translate ?\C-h ?\C-?)
    (message "Backspace has been fixed.")))

(global-set-key "\C-cd" 'c-time-stamp) ;bind C-c d to output timestamp
(global-set-key "\C-[;" 'goto-line) ; Give me a goto-line keyboard shortcut
(global-set-key "\C-c1" 'c-truncate) ; truncating is sometimes good
(global-set-key "\C-c2" 'c-wrap) ; wrapping is often good
(global-set-key (kbd "<f9>") 'fix-stupid-backspace-key-issue)
(global-set-key "\C-x\C-k" 'kill-buffer)
(kbd "tab")

;; GNU Emacs 21 Trailing Whitespace setup
(when (and (featurep 'emacs)
           (>= emacs-major-version 21))
  (setq-default show-trailing-whitespace t)

  (defun turn-off-show-trailing-whitespace ()
    "Turn on the highlighting of trailing whitespace in this buffer."
    (interactive)
    (setq show-trailing-whitespace nil))

  (defun turn-on-show-trailing-whitespace ()
    "Turn off the highlighting of trailing whitespace in this buffer."
    (interactive)
    (setq show-trailing-whitespace t))

  (defun toggle-show-trailing-whitespace ()
    "Toggle the highlighting of trailing whitespace in this buffer."
    (interactive)
    (setq show-trailing-whitespace
          (not show-trailing-whitespace)))

  (let ((mode-hooks '(Buffer-menu-mode-hook custom-mode-hook
                                            term-mode-hook Info-mode-hook
                                            comint-mode-hook buffer-menu-mode-hook
                                            apropos-mode-hook tooltip-show-hook
                                            gnus-article-mode-hook mail-mode-hook
                                            gnus-summary-mode-hook message-mode-hook
                                            gnus-group-mode-hook eshell-mode-hook
                                            w3-mode-hook)))

    (mapcar '(lambda (mode-hook)
               (add-hook mode-hook 'turn-off-show-trailing-whitespace))
            mode-hooks)))




;;;
;;; Crap I want here, but not enabled.
;;;
                                        ;(pc-selection-mode)
                                        ;(transient-mark-mode 0)
                                        ;(icomplete-mode)


;;;;
;;;;
;;;; Begin Mode Specific stuff
;;;;
;;;;

(require 'htmlize) ; give me the ability to gen html files

;;;
;;; Eshell stuff
;;;
(when (fboundp 'eshell)

  ;; This and rmb are my two vices. rmb is defined as an eshell alias.
  (defalias 'eshell/lo #'eshell/exit)

  (defalias 'eshell/logout #'save-buffers-kill-emacs)

  ;; I'm obsessive/compulsive with clear.
  (defun eshell/clear ()
    "\"Clear the screen,\" as it were."
    (recenter 0))

  ;; Obviously, this is TRT.
  (defun eshell/emacs (&rest args)
    "Open a file in emacs. Some habits die hard."
    (if (null args)
        (bury-buffer)
      (mapcar #'find-file args)))

  (defalias 'eshell/emacsclient #'eshell/emacs)
  ; (add-to-list 'eshell-visual-commands "CLI.py")
  ;; custom prompt
  ;(defun eshell-prompt-function () (concat (eshell-user-name) "@" (getenv "HOST") ":" default-directory "> "))
  ;(setq eshell-prompt-regexp (concat (eshell-user-name) "@" (getenv "HOST") ":" "\\([^>]>\\"))
  )
;;; End eshell stuff

;;; Japanse Stuff
(set-language-environment "Japanese") ; Set default alternate language
(setq quail-japanese-use-double-n t) ; nn = mn please
(setq enable-kinsoku t) ; Autofill properly
;;; End Japanese Stuff

;;;
;;; Python Mode stuff.
;;;
(autoload 'python-mode "python-mode" "Python editing mode." t)
(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (cons '("python" . python-mode)
            interpreter-mode-alist))
(add-hook 'python-mode-hook
          '(lambda ()
             (imenu-add-to-menubar "Jump")))
;;; End Python Mode Stuff


;;;
;;; Trailing Whitespace
;;;
(when-emacs-version 20
  (require 'ted-whitespace)
  (highlight-trailing-whitespace 'c-mode) ; no trailing whitespace in C
  (highlight-trailing-whitespace 'python-mode)
  (highlight-trailing-whitespace 'java-mode)
  (highlight-trailing-whitespace 'emacs-lisp-mode))
(when-emacs-version 21
  (setq-default show-trailing-whitespace t))
;;; End Trailing Whitespace


;;;
;;; Casey-HTML Stuff
;;;
;(require 'casey-html) ; Load the mode
;(setq-default casey-html-file-name "readme.html")
;(setq-default casey-html-target-directory (expand-file-name "."))
;(add-hook 'casey-html-mode-hook
;          '(lambda ()
;             (setq casey-html-target-directory default-directory)))
;(setq casey-html-before-text "<html>\n<body>\n<pre>\n") ;; define the html that you want to go before stuff.
;(setq casey-html-after-text "</pre>\n</body>\n</html>\n") ;; same, but after stuff.
;;; End Casey-HTML Stuff


;;;
;;; CDiv Stuff
;;;
(require 'cdiv) ; load the mode
(setq auto-mode-alist
      (cons '("\\.cdiv$" . cdiv-mode) auto-mode-alist))
;;; End CDiv Stuff


;;;
;;; Gnus Stuff
;;;
(setq gnus-select-method '(nntp "newshost.cs.rose-hulman.edu"))
(setq gnus-check-new-newsgroups nil)
(setq gnus-save-newsrc-file nil)
(setq gnus-save-killed-list nil)
(setq gnus-startup-file "~/elisp/newsrc")
(setq gnus-dribble-directory "~/elisp")
;;; End Gnus Stuff


;;;
;;; Visual Basic Mode
;;;
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic Mode" t)
(setq auto-mode-alist (append '(
                                ("\\.cls$" . visual-basic-mode)
                                ("\\.asp$" . visual-basic-mode)
                                ("\\.wsc$" . visual-basic-mode)
                                ("\\.vbs$" . visual-basic-mode)
                                ("\\.bas$" . visual-basic-mode)) auto-mode-alist))
;;; End Visual Basic Stuff

;;;
;;; Common Tabbing Code
;;;
(setq dabbrev-case-replace nil)
(setq dabbrev-case-fold-search nil)
(defun c-tab-indent-or-complete ()
  (interactive)
  (if (and
       (equal (current-column)
              (tab-test))
       (not (is-right-of-whitespace)))
      (dabbrev-completion)))

(defun tab-test ()
  (interactive)
  (c-indent-command)
  (current-column))

(defun is-right-of-whitespace ()
  (interactive)
  (if (or
       (equal "\t" (char-to-string (char-before)))
       (equal " "  (char-to-string (char-before)))
       (equal "\n" (char-to-string (char-before)))
       (equal "\l" (char-to-string (char-before))))
      't
    nil))

;;;
;;; Java Mode
;;;
(defun my-java-mode-hook ()
  (progn
    (turn-on-font-lock)
    (auto-fill-mode)
    (define-key java-mode-map "\t" 'c-tab-indent-or-complete)
    (setq fill-column 127)))

(add-hook 'cshar-mode-hook 'my-java-mode-hook)

;;;
;;; CSharp Mode
;;;
(setq auto-mode-alist
      (cons '("\\.cs$" . csharp-mode) auto-mode-alist))
(autoload 'csharp-mode "cc-mode")

(defun my-csharp-mode-hook ()
  (progn
   (turn-on-font-lock)
   (auto-fill-mode)
   (define-key csharp-mode-map "\t" 'c-tab-indent-or-complete)
   (setq fill-column 127)))

(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)
;;; End CSharp Stuff


;;;
;;; PSGML stuff
;;;
(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)
(add-to-list 'auto-mode-alist '("\\.xml\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.sgml\\'" . sgml-mode))
(setq sgml-catalog-files (list "catalog" (expand-file-name "~/elisp/xhtml1.soc")))

(require 'psgml)
;; set some psgml variables
(setq sgml-auto-activate-dtd t)
(setq sgml-omittag-transparent t)
(setq sgml-balanced-tag-edit t)
(setq sgml-auto-insert-required-elements t)
(setq sgml-live-element-indicator t)
(setq sgml-indent-step 2)
(setq-default sgml-indent-data 2)
(define-key sgml-mode-map "\C-ch" 'c-insert-xhtml-heading)
(define-key sgml-mode-map "\C-m"   'reindent-then-newline-and-indent)
;; create faces to assign to markup categories
(make-face 'sgml-comment-face)
(make-face 'sgml-start-tag-face)
(make-face 'sgml-end-tag-face)
(make-face 'sgml-entity-face)
(make-face 'sgml-doctype-face) ; DOCTYPE data
(make-face 'sgml-ignored-face) ; data ignored by PSGML
(make-face 'sgml-ms-start-face) ; marked sections start
(make-face 'sgml-ms-end-face) ; end of marked section
(make-face 'sgml-pi-face) ; processing instructions
(make-face 'sgml-sgml-face) ; the SGML declaration
(make-face 'sgml-shortref-face) ; short references

;; view a list of available colors with the emacs-lisp command:
;;
;; list-colors-display
;;
;; please assign your own groovy colors, because these are pretty bad
(set-face-foreground 'sgml-comment-face "White")
(set-face-background 'sgml-comment-face "#755050")

(set-face-foreground 'sgml-start-tag-face "#EE7070")
(set-face-foreground 'sgml-end-tag-face "#EE7070")
(set-face-foreground 'sgml-entity-face "#70EE70")
(set-face-foreground 'sgml-doctype-face "#70EE70")
(set-face-foreground 'sgml-ignored-face "cornflowerblue")
(set-face-foreground 'sgml-ms-start-face "coral")
(set-face-foreground 'sgml-ms-end-face "coral")
(set-face-foreground 'sgml-pi-face "coral")
(set-face-foreground 'sgml-sgml-face "coral")
(set-face-foreground 'sgml-shortref-face "coral")

;; assign faces to markup categories
(setq sgml-markup-faces '  (
 (comment . sgml-comment-face)
  (start-tag . sgml-start-tag-face)
  (end-tag . sgml-end-tag-face)
  (entity . sgml-entity-face)
  (doctype . sgml-doctype-face)
  (ignored . sgml-ignored-face)
  (ms-start . sgml-ms-start-face)
  (ms-end . sgml-ms-end-face)
  (pi . sgml-pi-face)
  (sgml . sgml-sgml-face)
  (shortref . sgml-shortref-face)
  ))

;; tell PSGML to pay attention to face settings
(setq sgml-set-face t)
(setq auto-mode-alist
      (append '(("\\.xml$" . xml-mode)) auto-mode-alist ))
;;; End PSGML Stuff



(if window-system ;;; Win32 or X11 or Visual display et al...
    (progn
      (if (equal 'w32 window-system) ;;; Win32 only stuff
          (progn
            (set-default-font  ; Set our font
             "-*-Lucida Console-normal-r-*-*-14-120-96-96-c-*-iso8859-1")
            (when-emacs-version 21 (fix-cursor))))

      (when-emacs-version 20 ; Emacs 20 GUI Specific Stuff
        (toggle-global-lazy-font-lock-mode)     ; Fun highlight method
        (setq lazy-lock-defer-time 0.05)        ; Color FAST!
        (setq lazy-lock-fontify-after-idle nil)
        (require 'color-theme)                  ; Load color-theme.el
        (color-theme-airog)                     ; Load my color theme
        (add-hook 'after-make-frame-functions   ; Load my color theme when
                  '(lambda (bob)                ; creating a new frame
                     (color-theme-airog))))     ;

      ;;; Fun for the whole GUI
      (scroll-bar-mode nil)                ; Turn off the scroll bar
      (setq-default mouse-yank-at-point t) ; Yank at the point, not at cursor
      (setq search-highlight t)            ; Highlight current search result.
      ;; (mouse-avoidance-mode 'exile) ; Kill the damn mouse cursor.
      ) ;;; end GUI stuff
  (progn ;;; Console only stuff
    (menu-bar-mode nil))) ; Gimme the extra row damnit

(when-emacs-version 21
  (if (not (equal 'mac window-system))
      (progn
        (require 'emacs-21-colorstuff)
        (setq default-frame-alist '(
                                    (foreground-color . "#c0c0c0")
                                    (background-color . "black")
                                    (mouse-color . "cyan")
                                    (cursor-color . "#FFFFFF")
                                    (border-color . "black")
                                    (background-mode . dark)
                                    (menu-bar-lines . 0)
                                    (tool-bar-lines . 0)
                                    (width . 132)
                                    (height . 50)))
        (setq initial-frame-alist default-frame-alist)
        (global-font-lock-mode 1)
        (interactive)
        (set-cursor-color "#FFFFFF"))))

(when-emacs-version 21
  (if (equal 'mac window-system)
      (progn
        (set-background-color "black")
        (set-foreground-color "white")
        (set-cursor-color "cyan")
        (set-frame-width frame-initial-frame 132)
        (set-frame-height frame-initial-frame 50)
        (global-font-lock-mode 1))))
(if (equal 'w32 window-system)
    (set-frame-position (car (frames-on-display-list)) 55 0))

(global-set-key "\C-z"  'c-null)     ; Ctrl-z bad, mkay?

