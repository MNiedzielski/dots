  # directories
  complete cd 'C/*/d/'
  complete rmdir 'C/*/d/'
  complete lsd 'C/*/d/'

  # signal names
  # also note that the initial - can be created with the first completion
  # but without appending a space (note the extra slash with no
  # append character specified)
  complete kill 'c/-/S/' 'p/1/(-)//'

  # use available commands as arguments for which, where, and man
  complete which 'p/1/c/'
  complete where 'p/1/c/'
  complete man 'p/1/c/'

  # aliases
  complete alias 'p/1/a/'
  complete unalias 'p/1/a/'

  # variables
  complete unset 'p/1/s/'
  complete set 'p/1/s/'

  # environment variables
  complete unsetenv 'p/1/e/'
  complete setenv 'p/1/e/'
  #(kinda cool: complete first arg with an env variable, and add an =,
  # continue completion of first arg with a filename.  complete 2nd arg
  # with a command)
  complete env 'c/*=/f/' 'p/1/e/=/' 'p/2/c/'

  # limits
  complete limit 'p/1/l/'

  # key bindings
  complete bindkey 'C/*/b/'

  # groups
  complete chgrp 'p/1/g/'

  # users
  complete chown 'p/1/u/'


  # You can use complete to provide extensive help for complex commands
  # like find.  
  # Please check your version before using these completions, as some
  # differences may exist.
  complete find 'n/-name/f/' 'n/-newer/f/' 'n/-{,n}cpio/f/' \
       'n/-exec/c/' 'n/-ok/c/' 'n/-user/u/' 'n/-group/g/' \
       'n/-fstype/(nfs 4.2)/' 'n/-type/(b c d f l p s)/' \
       'c/-/(name newer cpio ncpio exec ok user group fstype type atime \
       ctime depth inum ls mtime nogroup nouser perm print prune \
       size xdev)/' \
       'p/*/d/'

  # set up cc to complete only with files ending in .c, .a, and .o
  complete cc 'p/*/f:*.[cao]/'

  # of course, this completes with all current completions
  complete uncomplete 'p/*/X/'

  # complex completion for ln
  # In all cases, if you start typing, it completes with a filename
  # But if you complete without typing anything you get this:
  #   first argument:           adds "-s"
  #   arguments that follow -s: reminds you of which argument is expected
  complete ln 'C/?/f/' 'p/1/(-s)/' 'n/-s/x:[first arg is path to original file]/' 'N/-s/x:[second arg is new link]/'

  # set a printer list, for use with all print related commands
  set printerlist=(hp1 hp2 color)
  complete lp 'c/-d/$printerlist/'
  complete lpstat 'p/1/$printerlist/'
  complete lpq 'c/-P/$printerlist/'
  complete lpr 'c/-P/$printerlist/'
  complete enscript 'c/-d/$printerlist/'

  # set a list of hosts, for use with rlogin
  set hostlist=(foo bar baz)
  complete rlogin 'p/1/$hostlist/'
  complete rsh 'p/1/$hostlist/' 'p/2/c/'
  complete ssh 'p/1/$hostlist/' 'p/2/c/'

  # rcp and scp allow arguments to be references to either local or remote
  # files.  It's impossible to complete remote files, but its useful to assume
  # that the remote file structure is similar to the local one.
  #
  # when you first start typing, it could be any of a username, hostname,
  # or filename.  But filename is probably the most useful case, so:
  #
  # complete arguments as regular filenames, with following exceptions
  # if "@" has been typed, complete with a hostname, and append a colon (:)
  # if ":" has been typed, complete with a filename relative to home directory
  # if ":/" has been typed, complete with a filename relative to root directory
  # 
  complete rcp "c,*:/,F:/," "c,*:,F:$HOME," 'c/*@/$hostlist/:/'
  complete scp "c,*:/,F:/," "c,*:,F:$HOME," 'c/*@/$hostlist/:/'
