#
# .aliases.tcsh
#
# aliases for tcsh (obviously)
#
# Mon July  18 10:11:22 1988  Mark Niedzielski (niedziel@snowmass)
#

	# cmd executed before every prompt, see tcsh(1)
	#alias	precmd	'echo " " ; echo $HOST'
        alias precmd    'history -S'
        alias postcmd   'history -M'

	#alias	sethdr	'echo -n "]l${USER}@${HOST}    ${cwd} \ "'
	#alias	sethdr	'echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/~}\007"'
	alias	sethdr	'echo -n "]l${USER}@${HOST}    ${cwd} \ "'
	alias	clrhdr	'echo -n "]l \ "'
