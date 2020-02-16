#
# .aliases.csh
#
# aliases for csh and tcsh
#
# Wed Nov 30 1988  Mark Niedzielski (niedziel@copper)
# Thu Oct  7 1993  Mark Niedzielski v1.4  (min@napa)
#

	alias	sethdr	'set prompt="\\
`hostname`\\
`basename $cwd` \! > "; if ("${tty}" != /dev/console) echo -n "]l${user}@${hostname}: ${cwd}\"'
