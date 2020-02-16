;;;
;;; .emacs
;;;
;;; Wed 12 October 1988 Mark Niedzielski (niedziel@copper)
;;; Tue 2 November 2004 Mark Niedzielski (min@savantis)
;;; Fri 11 April   2014 Mark Niedzielski (mniedzielski@paypal)
;;; Wed 11 March   2015 Mark Niedzielski (mniedzielski@nasuni)
;;; Mon 9 July     2018 Mark Niedzielski (min@gamalon)
;;;

;
; Note: recall this is lisp, 't' is true, and 'nil' is false
;

( setq load-path (cons "~/emacs" load-path) )
;( setq load-path
;       ( append (list (expand-file-name "/home/min/emacs/")
;		       (expand-file-name "~/.xemacs")
;              load-path)
;	)
;)

( defvar using-server             nil )	  ; if you want to use emacsclient
( defvar using-abbrevs            nil )	  ; if you have a file of abbreviations
( defvar using-unix                 t )   ; enable server and extended controls
( defvar using-sun                nil )   ;
( defvar using-osx                  t )   ;
( defvar sun-esc-bracket          nil )	  ; for sun function keys
( defvar display-time-interval      5 )	  ; update rate for time on mode line
( defvar c-tabs-should-be-tabs      t )	  ; is <TAB> a tab, or an indent line
( defvar extra-click-wait         300 )

( let ((gc-cons-threshold 1000000))

;; Install package repositories:
( require 'package )
( package-initialize )
( add-to-list 'package-archives '("gnu" .       "http://elpa.gnu.org/packages/"       ) )
( add-to-list 'package-archives '("melpa" .     "http://melpa.org/packages/"          ) )
( add-to-list 'package-archives '("melpa" .     "http://melpa.milkbox.net/packages/"  ) )
;( add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") ) ; Defunct?
( package-refresh-contents )

; ( load "newtime"		nil t )
  ( if (eq window-system 'x)
    ( let nil
      ( load "blink-paren"	nil t )
    )
  )
  ( if (eq using-unix t)
    ( let nil
      ( load "server"		nil t )
    )
  )
  ( if (eq using-sun t)
    ( let nil
      ( load "term/sun"	        nil t )
      ( load "sunkeys"	        nil t )
      ( load "sunX-keys"	nil t )
      ( load "mouse-bits"       nil t )
      ( load "x-sb-mouse"	nil t )
      ( load "xsbm-userfuns"	nil t )
    )
  )
  ( load "scroll-in-place.el"	nil t )
  ( load "smooth-scroll.el/smooth-scroll.el" nil t )
  ( load "ebuff-menu"		nil t )
  ( load "echistory"		nil t )
  ( load "man"			nil t )
  ( load "saveconf"		nil t )
  ( load "ispell"		nil t )
  ( load "longlines.el"         nil t )      ; Manually wrap long lines
  ( load "mic-paren.el"         nil t )      ; Advanced Paren Matching
  ( load "cc-mode"		nil t )
  ( load ".emacs_init"		nil t )
  ( load "desktop"		nil t )
; ( load "cplus-md"		nil t )
; ( load "c-mode"		nil t )
; ( load "thing"		nil t )
; ( load "positions"		nil t )
; ( load "pos-help"		nil t )
; ( load "cmacexp"		nil t )
; ( load "custom.el"		nil t )      ; XEmacs layout variables
; ( load "hhm-config.el"        nil t )      ; HTML Helper Mode configuration

  ( garbage-collect )
)

; check if the packages is installed; if not, install it.
( mapc
 ( lambda (package)
   ( or ( package-installed-p package )
	( package-install package)
   )
 )
   '(     ace-jump-mode
	  json-mode flymake-json
	  ;ssh-config-mode
	  flymake
	  ;puppetfile-mode
	  puppet-mode flymake-puppet
	  ;flymake-jshint helm-flymake
	  ;flyspell-lazy
          gist ; https://github.com/defunkt/gist.el
	  yaml-mode
	  web-mode
	  ;elpy
	  ;yasnippet
	  ;project-explorer
	  ;rinari ruby-mode robe enh-ruby-mode flymake-ruby
	  ;git-gutter+ magit git-gutter-fringe
	  ;;magithub
	  ;terraform-mode
          ;mediawiki
    )
)

;; magithub configuration (github integration)
;; https://github.com/vermiculus/magithub/blob/master/magithub.org
;(magithub-feature-autoinject t)
;(setq magithub-clone-default-directory "~/src/celect")

;; set keys for Apple keyboard, for emacs in OS X
;(setq mac-command-modifier 'meta)             ; make cmd key do Meta
(setq mac-option-modifier 'super)             ; make opt key do Super
;(setq mac-control-modifier 'control)          ; make Control key do Control
;(setq ns-function-modifier 'hyper)            ; make Fn key do Hyper

; Associate keycodes to logical keys.  Move to .emacs_<platform>init.el.
;( when ( >= emacs-major-version 23 )
  ( define-key input-decode-map          "\e[1;5A" [C-up]      )
  ( define-key input-decode-map          "\e[1;5B" [C-down]    )
  ( define-key input-decode-map          "\e[1;5C" [C-right]   )
  ( define-key input-decode-map          "\e[1;5D" [C-left]    )
  ( define-key input-decode-map          "\e[1;2A" [S-up]      )
  ( define-key input-decode-map          "\e[1;2B" [S-down]    )
  ( define-key input-decode-map          "\e[1;2C" [S-right]   )
  ( define-key input-decode-map          "\e[1;2D" [S-left]    )
  ( define-key input-decode-map          "\e[1;7A" [O-up]      )
  ( define-key input-decode-map          "\e[1;7B" [O-down]    )
  ( define-key input-decode-map          "\e[1;7C" [O-right]   )
  ( define-key input-decode-map          "\e[1;7D" [O-left]    )
;)

; Keymap function assignments.  These will move to .emacs_<platform>init.el.
( define-key global-map [f1]             'shrink-window               )
( define-key global-map [27 f1]          'shrink-window-horizontally  )
( define-key global-map [f2]             'enlarge-window              )
( define-key global-map [27 f2]          'enlarge-window-horizontally )
( define-key global-map [f3]             'isearch-forward             )
( define-key global-map [27 f3]          'isearch-forward-regexp      )
( define-key global-map [f4]             'query-replace               )
( define-key global-map [27 f4]          'replace-regexp              )
( define-key global-map [f5]             'replace-string              )
( define-key global-map [f6]             'goto-line                   )
( define-key global-map [27 f6]          'regexp-builder              )

( define-key global-map [f7]             'beginning-of-buffer         )
( define-key global-map [f8]             'electric-buffer-list        )
( define-key global-map [f9]             'end-of-buffer               )
( define-key global-map [f10]            'comment-dwim                )

( define-key global-map [f11]            'split-window-below          )
( define-key global-map [f12]            'delete-other-windows-vertically )

( define-key global-map [f13]            'backward-paragraph          )
( define-key global-map [f14]            'beginning-of-buffer         )
( define-key global-map [27 f14]         'end-of-buffer               ) ; ESC f14
( define-key global-map [f15]            'forward-paragraph           )

( define-key global-map [f16]            'research-backward           ) ; shift-f5
( define-key global-map [27 f16]         'isearch-backward            )
( define-key global-map [f17]            'research-forward            ) ; shift-f6
( define-key global-map [27 f17]         'isearch-forward             )
( define-key global-map [f18]            'describe-key                )
( define-key global-map [f19]            'describe-function           ) ; shift-f7

( define-key global-map [f20]            'scroll-down-command         ) ; shift-f8
( define-key global-map [f21]            'scroll-up-command           ) ; shift-f9
( define-key global-map [f22]            'backward-paragraph          ) ; shift-f10
;( define-key global-map [f23]            'beginning-of-buffer         ) ; shift-f11
( define-key global-map [f23]            'end-of-buffer               ) ; shift-f14 on full keyboard
( define-key global-map [f24]            'forward-paragraph           ) ; shift-f12

( define-key global-map [S-up]           'backward-to-indentation     )
( define-key global-map [S-down]         'forward-to-indentation      )
( define-key global-map [C-up]           'backward-paragraph          )
( define-key global-map [C-down]         'forward-paragraph           )
( define-key global-map [O-up]           'scroll-up-in-place          )
( define-key global-map [O-down]         'scroll-down-in-place        )

( define-key global-map [S-right]        'forward-word                )
( define-key global-map [S-left]         'backward-word               )
( define-key global-map [C-left]         'previous-multiframe-window  )
( define-key global-map [C-right]        'next-multiframe-window      )
;( define-key global-map (kbd "C-[")      'python-indent-shift-left    )
;( define-key global-map (kbd "C-]")      'python-indent-shift-right   )

( define-key global-map [27 f22]         'scroll-down                 ) ; 27 == ESC
( define-key global-map [27 f24]         'scroll-up                   )
( define-key global-map [prior]          'scroll-down                 )
( define-key global-map [next]           'scroll-up                   )

( define-key global-map [O-left]         'previous-buffer             )
( define-key global-map [O-right]        'next-buffer                 )

( define-key global-map [27 deletechar]  'kill-word                   ) ; ESC DEL
;( define-key global-map [control-delete] 'delete-backwards            )
;( define-key global-map [pause]          'shell                       )
;( define-key global-map [control-pause]  'long-lines                  )

( define-key global-map [M-O x]           'replace-string             )
( define-key global-map [M-O o]           'replace-regexp             )
( define-key global-map [M-O j]           'replace-regexp             ) 

;; ACE-JUMP: http://www.emacswiki.org/emacs/AceJump
;; --------------------
;; Set mark
;; C-SPC
;; Jump to word
;; C-c SPC <First_letter_of_word> <LETTER>
;; Jump Back to mark
;; C-U C-SPC
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; Helm
;(require 'helm-config)
;(require 'helm-flymake)
;(global-set-key (kbd "C-c h") 'helm-mini)
;(helm-mode 1)

;; Improve flyspell responsiveness
;(require 'flyspell-lazy)
;(flyspell-lazy-mode 0)
;(flyspell-mode 1)

; Puppetfile
;(require 'puppetfile-mode)
;(add-to-list 'auto-mode-alist '("Puppetfile\\'" . puppetfile-mode))

;; Puppet
;(add-to-list 'auto-mode-alist '("pp$" . puppet-mode))
;; (add-hook 'puppet-mode 'flymake-puppet-load)
(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))
(require 'flymake-puppet)
(add-hook 'puppet-mode-hook (lambda () (flymake-puppet-load)))

;; JSON
(require 'flymake-json)
(add-hook 'json-mode-hook   (lambda () (flymake-json-load)))

;; Jinja2
(add-to-list 'auto-mode-alist '("\\.jinja\\'" . web-mode))

;; Flymake
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-error ((((class color)) (:underline "red"))))
 '(flymake-warning ((((class color)) (:underline "yellow"))))
 '(font-lock-keyword-face ((t (:foreground "magenta" :weight normal))))
 '(font-lock-string-face ((t (:foreground "blue"))))
 '(font-lock-type-face ((t (:foreground "green" :weight semi-bold))))
 '(font-lock-variable-face ((t (:foreground "cyan" :weight normal))))
 '(font-lock-variable-name-face ((t (:foreground "cyan" :weight normal))))
 '(region ((t (:background "white")))))
(require 'flymake-cursor)

;; ;; Terraform Mode
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(mediawiki-site-alist
;;    (quote
;;     (("Wikipedia" "https://en.wikipedia.org/w/" "username" "password" nil "Main Page")
;;      ;("Nasuni" "http://wiki.nasuni.net/" "Mniedzielski" "Nasuni313" nil "NOC"))))
;;  '(package-selected-packages
;;    (quote
;;     ;(terraform-mode terraform mediawiki gist flymake-json yaml-mode puppetfile-mode puppet-mode magit json-mode git-gutter-fringe git-gutter+ flymake-puppet ace-jump-mode)))
;;     (terraform-mode terraform mediawiki gist flymake-json yaml-mode puppetfile-mode puppet-mode json-mode git-gutter-fringe git-gutter+ flymake-puppet ace-jump-mode)))
;;  '(terraform-indent-level 4))


;; ( load "cloudformation-mode/cloudformation-mode.el" nil t )
;; (require 'cloudformation-mode)
;; (add-to-list 'auto-mode-alist '("\\.cf$" . cloudoformation-mode))
;; (add-to-list 'auto-mode-alist '("\\.cloudformation$" . cloudoformation-mode))
;; (add-to-list
;; 'magic-mode-alist
;;  '("\\(.\\|\n\\)*AWSTemplateFormatVersion" . cloudformation-mode))

; https://github.com/nonsequitur/git-gutter-plus
;( global-git-gutter+-mode t )

( setq column-number-mode                 t    )
;( paren-set-mode                         'sexp )
( setq show-paren-delay                   0    )
( show-paren-mode )
( setq show-paren-style                   'parenthesis) ; highlight just brackets
;( setq show-paren-style                   'expression) ; highlight entire bracket expression

;; Show offscreen parens
(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
        "If the matching paren is offscreen, show the matching line in the echo area.
        Has no effect if the character before point is not of the syntax class ')'."
	(interactive)
	(let* ((cb (char-before (point)))
	       (matching-text (and cb
				   (char-equal (char-syntax cb) ?\) )
				   (blink-matching-open))))
	          (when matching-text (message matching-text))))

;; Emacs gurus don't need no stinking scroll bars
( when (fboundp 'toggle-scroll-bar )
  ( toggle-scroll-bar -1 )
)

( linum-mode                              -1 )

;; Explicitly show the end of a buffer
( set-default 'indicate-empty-lines       t  )

( if using-server ( server-start ) )

( setq display-time-day-and-date         nil ) ; show day, date and time
( setq display-time-mail-file            t )   ; don't check mail
( setq display-time-24hr-format          t )   ; in 24hr format
( display-time )

( if using-abbrevs ( read-abbrev-file nil t ) )

( setenv "SLACK_TOKEN"                   "xoxb-14929910341-Xfug3Kv9p9R0CY78LtI4vj5B" )
(defvar slackcat-bin  "slackcat"         "Command to invoke slackcat.")
(defvar slackcat-args "-c noc"           "Default arguments to pass to slackcat.")

(defun slackcat (&optional b e)
  "Upload contents of region to slack chat."
  (interactive "r")
  (let ((args (read-from-minibuffer "slackcat args: " slackcat-args)))
        (shell-command-on-region b e (format "%s %s" slackcat-bin args))))

;; Backup files in one spot
(setq backup-directory-alist             nil )
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/backups"))
            backup-directory-alist))

( setq make-backup-files                 nil ) ; this ought to be interactive
( setq desktop-save-mode                 t   ) ; 
( setq desktop-path                      '("~/.emacs.d/") ) ;
( setq auto-save-and-recover-context     t   ) ; to save context upon exit
( setq save-buffer-context               t   ) ; to save context of all buffers
;;
( recover-context )
;( desktop-recover )

;; ( split-window-horizontally )                  ; Start with a split view

;( ecb-activate )                               ; enable Emacs Code Browser by default
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (visible-mark web-mode elpy yaml-mode terraform-mode puppet-mode magithub json-mode git-gutter-fringe git-gutter+ gist flymake-puppet flymake-json ace-jump-mode))))
