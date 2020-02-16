#
# .login
#
# Wed December 28 15:38:32 1988  Mark Niedzielski (niedziel@copper)
# Thu March    22 15:52:26 1990  Mark Niedzielski (niedziel@earth)
# Thu October   7 09:46:06 1993  Mark Niedzielski (min@napa)
# Tue February 27 14:27:17 1996  Mark Niedzielski (min@sleestack)

if ( ! ${?DT} ) then
	clear

	echo " "
	/bin/date
	uptime
	echo " "

	set where = `tty`
	if ( $TERM == 'sun' && $where == '/dev/console' ) then

		if ( $osrev == '4' ) then
			stty dec
			click -n
		endif

		setenv	XDIR		$HOME/X
		setenv	XINITRC		$XDIR/.xinitrc
		setenv	HOSTNAME	$hostname

		#
		# do we have a host specific windowing init file to run?
		#
		if ( -r $XDIR/.$hostname ) then
			source $XDIR/.$hostname
		else
			source $XDIR/.default
		endif

		if ( -e ~/.exit ) then
			logout
		endif

	else

		set mail = (10 /usr/spool/mail/$USER)

		if ( -r /usr/spool/mail/$USER ) then
			echo " "
			from | tail -24
		endif

		if ( $osrev != '5' ) then
			biff y
		endif

	endif

else
	setenv	XDIR		$HOME/X
	setenv	XINITRC		$XDIR/.xinitrc
	setenv	HOSTNAME	$hostname
endif

