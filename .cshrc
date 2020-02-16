#
# .cshrc
#
# Mon July 18 10:08:32 1988  Mark Niedzielski (niedziel@snowmass)
# Wed Dec  11 14:37:12 1991  mark niedzielski (min@iclil)
# Mon Apr  27 10:28:13 2015  Mark Niedzielski (min@wayback)
#

limit coredumpsize	0m
limit descriptors	256
#umask 007
umask 022

#
# filec, fignore, and nobeep all relate to csh file name completion
# 'man csh'
#
set	filec
set	fignore		= ( .o .out )
set	nobeep
#unset	savehist

#
# identify where we are
#
set 	hostname	= `uname -n`
set 	tty		= `tty`
set	arch		= `uname -m`
if ( ($arch == 'sun4c') || ($arch == 'sun4m') ) then
	set arch = sun4
endif

set	ostype		= `uname -s`
set	osversion	= `uname -r`
set	osrev		= `uname -r | sed -n 's/^\([^\.]*\).*/\1/p'`
setenv	OSREV		$osrev

if ( ($osrev == '4') && ($arch == 'sun4') ) then
	set mach = `mach`
else
	set mach = `uname -p`
endif

#
# be notified upon completion of background jobs
#
set	notify

#
# so that ^D doesn't kick us out of a csh
#
set	ignoreeof

#setenv	LANG			en_US
setenv	NOREBIND		true

setenv	LESS			cCdeMqsw
setenv	MORE			'-c'
setenv	PAGER			less
#setenv	PRINTER			lp2
#setenv	TERMCAP			~/.termcap
setenv	PS1			'oslo > '
setenv	PS2			' ? '
#setenv	NNTPSERVER		newsserver.questra
#setenv	JAVA_HOME		/usr/java
#setenv	CLASSPATH		.:$JAVA_HOME/lib:$HOME/src/java/classes:/local/lib

#
# DATED: XWindows
#setenv	OPENWINHOME		/usr/openwin
setenv	INDEXDIR		$HOME/h
#setenv	XENVIRONMENT		/local/src/xrainbow/Xrainbow
#setenv	XAPPLRESDIR		$OPENWINHOME/lib/app-defaults
#setenv	XDVIFONTS		/packages/latex/cmfonts/%p/%p%d/%f.%d%p
#setenv	XFILESEARCHPATH	 	$OPENWINHOME/bin

#setenv  PYTHONPATH		.:/lib/python2.7/site-packages:/usr/local/lib/python2.7/site-packages:/usr/lib/python2.7:$HOME/src
##setenv  PYTHONPATH		.:/lib/python2.7/site-packages:/usr/local/lib/python2.7/site-packages:/usr/lib/python2.7:$HOME/src:$HOME/src/nasuni
#setenv  SLACK_TOKEN             xoxb-14929910341-Xfug3Kv9p9R0CY78LtI4vj5B
#setenv  LIBRATO_USERID		noc@nasuni.com
#setenv  LIBRATO_API_TOKEN	d8412b51660e9035209898fd8d1e934e379a97bc57a867af7693344f9a87fd11

#
# DATED: environment variables dependent upon OS revision
#
#if ( ($osrev == '5') ) then
#	setenv	CCROOTDIR	/opt/SUNWspro
#	setenv	CCBINDIR	$CCROOTDIR/bin
#else
#	setenv	CCROOTDIR	/opt/lang
#	setenv	CCBINDIR	$CCROOTDIR
#endif

#setenv	LD_LIBRARY_PATH		$CCROOTDIR/lib:$OPENWINHOME/lib:/usr/lib:$JAVA_HOME/lib:/pkgs/X11R6/lib
#setenv	MANPATH			$CCROOTDIR/man:$OPENWINHOME/man:/usr/local/man:/usr/man

#
# everywhere...
#
set	path = (\
		~/bin\
		~/bin/ec\
#		/lib/python2.7/site-packages\
#		/usr/local/opt/python/libexec/bin\
#                ~/Library/Python/2.7/bin\
		/usr/local/bin\
                /usr/local/bin/bin\
		/usr/local/sbin\
		/bin\
		/sbin\
		/usr/bin\
		/usr/sbin\
		/opt/local/bin\
		/opt/local/sbin\
		/opt/puppetlabs/bin\
                /usr/local/opt/openssl/bin\
#                /Library/Ruby/Gems/2.0.0/gems/puppet-lint-2.0.2/bin\
#		/usr/X11R6/bin\
#		/Applications/Emacs.app/Contents/MacOS/bin\
		.\
	)

#
# the 'if($?prompt)' assures us that the following only
# happens in interactive shells. this gives 'rsh' a
# big performance boost.
#
if($?prompt == 0) then
	exit
endif

# Yes, reset hostname to an abbreviated version of that used by non-interactive shells.
set	hostname	= `uname -n | sed -e 's/\..*//'`
set	history		= 30000

#
# cdpath is a list of places to check for a cd
#
set	cdpath = (\
		~\
		/\
		~/src\
		/export\
		/opt\
		/var\
		..\
	)

#
# if we have a meta key, pass 8bit characters for meta characters in tcsh
#
#if ( "`echotc meta`" == "yes" ) then
#	stty pass8
#endif
#
#setenv	LC_CTYPE	iso_8859_1
#stty -parenb -istrip cs8

#setenv	EDITOR	/packages/gnu/bin.$arch/emacsclient
#setenv	VISUAL	/packages/gnu/bin.$arch/emacsclient
setenv	EDITOR	vi
setenv	VISUAL	vi
setenv	ESHELL	/bin/csh

if($?tcsh) then
	source ~/.common-completion.tcsh
	complete aws 'p/*/`aws_completer`/'
	# https://github.com/git/git/tree/master/contrib/completion
	source ~/.git-completion.tcsh

	# Make command completion (TAB key) cycle through all possible choices
	# (The default is to simply display a list of all choices when more than one
	# match is available.)
	bindkey "^I" complete-word-fwd
	bindkey "^H" i-search-back

	# Make command history (arrow keys) use history search operations
	# (The default is to simply display the next item in the history list.)
	bindkey -k down history-search-forward
	bindkey -k up history-search-backward

	set     autoexpand
	set	autolist	= ambiguous
	set	complete
	set	chase_symlinks
	set     color
	set	correct
	set	dextract
	#set     expand-glob
        set	histdup		= erase
	set	implicitcd	= verbose
	set	listjobs
	set	listlinks
	set	printexitvalue
	set	recexact
	set	savedirs
        set	savehist	= (${history} merge lock)
	set	watch		= (5 any any)

	unset	autologout
	bindkey -e
	source .aliases.tcsh
else
	source .aliases.csh
endif
source .aliases.common

#if("$tty" != '/dev/console') then
#	cd .
#	ihdr $hostname
#endif

if ($term == xterm || $term == xterms || $term == xterml || $term == xterm-color || $term == xterm-256color) then
	set prompt="%{\e]2\;%m:%/^g\e]1\;%m^g\r%}%m %h %# "
else
	set prompt	= "%c %h > "
endif

#
# correct the errors of silly terminal servers
#
#if(($term != 'sun') && ($term != 'sun-cmd') && ($term != 'xterm') && ($term != 'xterm-color') && ($term != 'xterm-256color')) then
#	set term = vt220
#	stty rows 24 columns 80
#endif

#eval $(docker-machine env default)
