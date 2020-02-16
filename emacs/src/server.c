# This is a shell archive.  Remove anything before this line,
# then unpack it by saving it in a file and typing "sh file".
#
# Wrapped by Andy Norman <ange@anorman> on Tue Jan 24 18:07:36 1989
#
# This archive contains:
#	Makefile	gnuclient.c	gnudoit.c	gnulib.c	
#	gnulib.h	gnuserv.1	gnuserv.c	gnuserv.el	
#	x11fns.diff	
#
# Error checking via wc(1) will be performed.

LANG=""; export LANG

echo x - Makefile
cat >Makefile <<'@EOF'
DEST=.
CFLAGS=-O

all:	${DEST}/gnuclient ${DEST}/gnuserv ${DEST}/gnudoit

${DEST}/gnuclient: gnulib.h gnulib.o gnuclient.c
	cc ${CFLAGS} gnuclient.c gnulib.o -o $@

${DEST}/gnuserv: gnulib.h gnulib.o gnuserv.c
	cc ${CFLAGS} gnuserv.c gnulib.o -o $@

${DEST}/gnudoit: gnulib.h gnulib.o gnudoit.c
	cc ${CFLAGS} gnudoit.c gnulib.o -o $@

gnulib.o: gnulib.h gnulib.c
	cc -c ${CFLAGS} gnulib.c
@EOF
set `wc -lwc <Makefile`
if test $1$2$3 != 1643389
then
	echo ERROR: wc results of Makefile are $* should be 16 43 389
fi

chmod 644 Makefile

echo x - gnuclient.c
cat >gnuclient.c <<'@EOF'
/* -*-C-*-
 Client code to allow remote editing of files by GNU Emacs.

 This file is not part of GNU Emacs (yet).
 Copyright (c) 1989 Andrew P. Norman

 Copying is permitted under those conditions described by the GNU
 Emacs General Public License.

 Author: Andy Norman (ange%anorman@hplabs.hp.com)

 Please mail bugs and suggestions to the author at the above address.
*/

#include "gnulib.h"

/*
  filename_expand -- try to convert the given filename into a fully-qualified
  		     pathname.
*/
void filename_expand(fullpath,remotepath,filename)
     char *fullpath;	/* returned full pathname */
     char *remotepath;	/* pathname to tack on the front */
     char *filename;	/* filename to expand */
{
  char cwd[MAXPATHLEN+1];
  int len;
  
  strcpy(fullpath,remotepath);

  if(filename[0] && filename[0] != '/') { /* relative filename */
    if ((getcwd(cwd,MAXPATHLEN)) == NULL) {
      perror(progname);
      fprintf(stderr,"%s: unable to get current working directory\n",progname);
      exit(1);
    }; /* if */
    
    strcat(fullpath,cwd);
    len = strlen(fullpath);
     
    if (len > 0 && fullpath[len-1] == '/') /* trailing slash already? */
      ;
    else
      strcat(fullpath,"/");	/* append trailing slash */
  }; /* if */

  strcat(fullpath,filename);

} /* filename_expand */


main(argc,argv)
     int argc;
     char *argv[];
{
  int starting_line = 1;	/* line to start editing at */
  char command[MAXPATHLEN+50];	/* emacs command buffer */
  char remotehost[HOSTNAMSZ];  	/* remote hostname */
  char remotepath[MAXPATHLEN+1]; /* remote pathname */
  char fullpath[MAXPATHLEN+1];	/* full pathname to file */
  char *ptr;			/* return from getenv */
  int qflg = 0;			/* quick edit, don't wait for user to finish */
  int hflg = 0;			/* hostname given on command line */
  int rflg = 0;			/* pathname given on command line */
  int errflg = 0;		/* option error */
  int c;			/* char from getopt */
  int s;			/* socket to server */
  u_short port = 0;		/* port to server */

  progname = argv[0];

  while ((c = getopt(argc, argv, "h:p:r:q")) != EOF)
    switch (c) {
    case 'q':
      qflg++;
      break;
    case 'h':
      strcpy(remotehost,optarg);
      hflg++;
      break;
    case 'r':
      strcpy(remotepath,optarg);
      rflg++;
      break;
    case 'p':
      port = atoi(optarg);
      break;
    case '?':
      errflg++;
    }; /* switch */

  if (errflg) {
    fprintf(stderr,"usage: %s [-q] [-h hostname] [-p port] [-r pathname] [[+line] path] ...\n",
	    progname);
    exit (1);
  }; /* if */

  if (!hflg) {
    if((ptr=getenv("GNU_HOST")) != NULL)
      strcpy(remotehost,ptr);
    else
      gethostname(remotehost,HOSTNAMSZ);	/* use this host by default */
  }; /* if */

  if(!rflg) {
    if((ptr=getenv("GNU_NODE")) != NULL)
      strcpy(remotepath,ptr);
    else
      remotepath[0] = '\0';	/* default is the empty path */
  }; /* if */

  if (port == 0 && (ptr=getenv("GNU_PORT")) != NULL)
    port = atoi(ptr);

  s = connect_to_server(remotehost,port);
  send_string(s,"(progn ");

  for (; optind < argc; optind++) {
    if (*argv[optind] == '+')
      starting_line = atoi(argv[optind]);
    else {
      filename_expand(fullpath,remotepath,argv[optind]);
      
      if (qflg)
	sprintf(command,"(server-edit-file-quickly %d \"%s\")",starting_line,fullpath);
      else
	sprintf(command,"(server-edit-file %d \"%s\")",starting_line,fullpath);
      
      send_string(s,command);
      starting_line = 1;
    }; /* else */
  }; /* for */

  if (qflg)
    send_string(s,"(server-edit-file-quickly-done))");
  else
    send_string(s,")");

  disconnect_from_server(s,FALSE);
  exit(0);

} /* main */

@EOF
set `wc -lwc <gnuclient.c`
if test $1$2$3 != 1434563668
then
	echo ERROR: wc results of gnuclient.c are $* should be 143 456 3668
fi

chmod 644 gnuclient.c

echo x - gnudoit.c
cat >gnudoit.c <<'@EOF'
/* -*-C-*-
 Client code to remotely evaluate lisp forms using GNU Emacs.

 This file is not part of GNU Emacs (yet).
 Copyright (c) 1989 Andrew P. Norman

 Copying is permitted under those conditions described by the GNU
 Emacs General Public License.

 Author: Andy Norman (ange%anorman@hplabs.hp.com)

 Please mail bugs and suggestions to the author at the above address.
*/

#include "gnulib.h"

main(argc,argv)
     int argc;
     char *argv[];
{
  int starting_line = 1;	/* line to start editing at */
  char remotehost[HOSTNAMSZ];	/* remote hostname */
  char *ptr;			/* return from getenv */
  int hflg = 0;			/* hostname given on command line */
  int qflg = 0;			/* don't wait around for gnu emacs to eval cmd */
  int errflg = 0;		/* option error */
  int c;			/* char from getopt */
  int s;			/* socket to server */
  u_short port = 0;		/* port number */

  progname = argv[0];

  while ((c = getopt(argc, argv, "qh:p:")) != EOF)
    switch (c) {
    case 'h':
      strcpy(remotehost,optarg);
      hflg++;
      break;
    case 'p':
      port = atoi(optarg);
      break;
    case 'q':
      qflg++;
      break;
    case '?':
      errflg++;
    }; /* switch */

  if (errflg) {
    fprintf(stderr,"usage: %s [-q] [-h hostname] [-p port] [sexpr]...\n",progname);
    exit (1);
  }; /* if */

  if (!hflg) {
    if((ptr=getenv("GNU_HOST")) != NULL)
      strcpy(remotehost,ptr);
    else
      gethostname(remotehost,HOSTNAMSZ); /* use this host by default */
  }; /* if */

  if (port == 0 && (ptr=getenv("GNU_PORT")) != NULL)
      port = atoi(ptr);

  s = connect_to_server(remotehost,port);

  if (qflg)
    send_string(s,"(server-eval-quickly (quote (progn ");
  else
    send_string(s,"(server-eval (quote (progn ");

  for (; optind < argc; optind++)
    send_string(s,argv[optind]);

  send_string(s,")))");

  disconnect_from_server(s,!qflg);
  exit(0);

} /* main */

@EOF
set `wc -lwc <gnudoit.c`
if test $1$2$3 != 802581893
then
	echo ERROR: wc results of gnudoit.c are $* should be 80 258 1893
fi

chmod 644 gnudoit.c

echo x - gnulib.c
cat >gnulib.c <<'@EOF'
/* -*-C-*-
 Common library code for the GNU Emacs server and client.

 This file is not part of GNU Emacs (yet).
 Copyright (c) 1989 Andrew P. Norman

 Copying is permitted under those conditions described by the GNU
 Emacs General Public License.

 Author: Andy Norman (ange%anorman@hplabs.hp.com)

 Please mail bugs and suggestions to the author at the above address.
*/

#include "gnulib.h"

char *progname = NULL;

/*
  send_string -- send string to socket.
*/
void send_string(s,msg)
     int s;
     char *msg;
{
  if (send(s,msg,strlen(msg),0) < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to send\n",progname);
    exit(1);
  }; /* if */ 
  
} /* send_string */


/*
  internet_addr -- return the internet addr of the hostname or
                   internet address passed. Return -1 on error.
*/
u_long internet_addr(host)
     char *host;
{
  struct hostent *hp;		/* pointer to host info for remote host */
  u_long host_addr;		/* host address */

  if ((host_addr = inet_addr(host)) != -1)
    return host_addr;
  else if ((hp = gethostbyname(host)) != NULL)
    return ((struct in_addr *)(hp->h_addr))->s_addr;
  else
    return -1;

} /* internet_addr */


/*
  connect_to_server -- establish connection with server process.
                       returns socket descriptor for server if successful.
*/
int connect_to_server(serverhost,port)
     char *serverhost;
     u_short port;
{
  int s;				/* connected socket descriptor */
  struct servent *sp;			/* pointer to service information */
  struct sockaddr_in peeraddr_in;	/* for peer socket address */

  /* clear out address structures */
  memset ((char *)&peeraddr_in, 0, sizeof(struct sockaddr_in));
  
  /* Set up the peer address to which we will connect. */
  peeraddr_in.sin_family = AF_INET;

  /* look up the server host's internet address */
  if ((peeraddr_in.sin_addr.s_addr = internet_addr(serverhost)) == -1) {
    fprintf(stderr,"%s: unable to find %s in /etc/hosts or from YP\n",
	    progname,serverhost);
    exit(1);
  }; /* if */

  if (port == 0) {
    if ((sp = getservbyname ("gnuserv","tcp")) == NULL) {
      fprintf(stderr,"%s: unable to find \"gnuserv\" in /etc/services or from YP\n",
	      progname);
      exit(1);
    }; /* if */
    
    peeraddr_in.sin_port = sp->s_port;
  } /* if */
  else
    peeraddr_in.sin_port = htons(port);

  /* Create the socket. */
  if ((s = socket (AF_INET,SOCK_STREAM, 0))== -1) {
    perror(progname);
    fprintf(stderr,"%s: unable to create socket\n",progname);
    exit(1);
  }; /* if */

  /* Try to connect to the remote server at the address
   * which was just built into peeraddr.
   */
  if (connect(s, &peeraddr_in, sizeof(struct sockaddr_in)) == -1) {
    perror(progname);
    fprintf(stderr, "%s: unable to connect to remote\n",progname);
    exit(1);
  }; /* if */

  return(s);

} /* connect_to_server */


/*
  disconnect_from_server -- inform the server that sending has finished, and wait for
                            its reply.
*/
void disconnect_from_server(s,echo)
     int s;
     int echo;
{
  char buffer[REPLYSIZ];
  int length;

  if (shutdown(s,1) == -1) {
    perror(progname);
    fprintf(stderr, "%s: unable to shutdown socket\n",progname);
    exit(1);
  }; /* if */

  while((length = recv(s,buffer,REPLYSIZ,0)) > 0) {
    buffer[length] = '\0';
    if (echo) printf("%s",buffer);
  }; /* while */
  
  if (echo) putchar('\n');

  if(length < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to read the reply from the server\n",progname);
    exit(1);
  }; /* if */

} /* disconnect_from_server */  
@EOF
set `wc -lwc <gnulib.c`
if test $1$2$3 != 1434683594
then
	echo ERROR: wc results of gnulib.c are $* should be 143 468 3594
fi

chmod 644 gnulib.c

echo x - gnulib.h
cat >gnulib.h <<'@EOF'
/* -*-C-*-
 Header file for the GNU Emacs server and client C code.

 This file is not part of GNU Emacs (yet).
 Copyright (c) 1989 Andrew P. Norman

 Copying is permitted under those conditions described by the GNU
 Emacs General Public License.

 Author: Andy Norman (ange%anorman@hplabs.hp.com)

 Please mail bugs and suggestions to the author at the above address.
*/

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/param.h>
#include <netinet/in.h>
#include <stdio.h>
#include <netdb.h>

extern char *getenv();
extern char *optarg;
extern int optind;
extern char *progname;
extern char *getcwd();

#define REPLYSIZ 300		/* max size of reply from server to client */
#define HOSTNAMSZ 255		/* max size of a hostname */
#define TABLE_SIZE 101		/* The number of entries in the hash table */
#define HASH(host) host		/* Rather simplistic hash function */

#define FALSE 0
#define TRUE 1
@EOF
set `wc -lwc <gnulib.h`
if test $1$2$3 != 34137901
then
	echo ERROR: wc results of gnulib.h are $* should be 34 137 901
fi

chmod 644 gnulib.h

echo x - gnuserv.1
cat >gnuserv.1 <<'@EOF'
.TH GNUSERV 1 "" "Server / Client enhancements for GNU Emacs"
.UC 4
.SH NAME
gnuserv, gnuclient, gnudoit \- Server / Client Enhancements for GNU Emacs
.SH SYNOPSIS
.B gnuclient
[-h hostname] [-q] [-p port] [-r pathname] [[+line] path] ...
.br
.B gnudoit 
[-h hostname] [-q] [-p port] [sexpr] ...
.br
.B gnuserv
.SH DESCRIPTION
\fBgnuclient\fP allows the user to request a running GNU Emacs process to
edit the named files or directories. 
.PP
\fBgnudoit\fP allows the user to request a running GNU Emacs process to
evaluate the given arguments inside a progn lisp form.
.PP
\fBgnuserv\fP is the server program that is set running by GNU Emacs to
handle all incoming and outgoing requests. It is not usually invoked directly, but is
started from GNU Emacs by the lisp form (server-start).
.PP
The options are:
.P
.TP 10
\fB-q\fP
This option means that both gnuclient and gnudoit will exit once connection has
been made with the GNU Emacs process. Normally gnuclient waits until all of the
files on the command line have been finished with (their buffers killed) by the
GNU Emacs process. Normally gnudoit
waits around for evaluation of its arguments by the GNU Emacs process, and prints
the results or error conditions.
.TP 10
\fB-h\fP hostname
This option specifies the host machine which should be running gnuserv. If this option
is not specified then the value of the environment variable
``GNU_HOST'' is used if set, otherwise the hostname of the machine running the
program is used.
Note that an internet address may be specified instead of a 
hostname which can speed up connections to the server by quite a factor,
especially if the client machine is running YP.
.TP 10
\fB-p\fP port
The service is called ``gnuserv'' in the services database, but a
different service port may be specified with this option.
The server must also be using the same service port. If this option is not
specified, but the value of the environment variable ``GNU_PORT'' is
specified, then it is used instead. Note that gnuserv doesn't use the command
line option since it is usually started from within GNU Emacs.
.TP 10
\fB-p\fP pathname
The pathname argument is needed such that the server knows how to specify the
root directory of a remote machine. \fBgnuclient\fP prepends this string to each
path argument given. For example, if you were trying to edit a file on a client
machine called otter, whose root directory
was accessible \fIfrom\fP the server machine via the path /net/otter, then this
argument should be set to '/net/otter'. If this option is omitted,
then the value is taken from the environment variable ``GNU_NODE'', if set, or
the empty string otherwise.
.TP 10
\fBpath\fP
This is the path of the file to be edited. If the file is a directory, then the
directory browser dired is usually invoked instead.
.TP 10
\fBsexpr\fP
This is part of a gnu emacs LISP expression to evaluate. All the arguments are
bundled together and wrapped in a progn form.
.PP
.SH SETUP
In order to use the programs, the file gnuserv.el can be copied into a
directory on your GNU Emacs load-path, and loaded into GNU Emacs by the lisp
form (load "gnuserv"). The server can then be initialized by the GNU Emacs
lisp form (server-start).
.PP
Additionally, the patch to src/x11fns.c can be applied to allow GNU Emacs to
de-iconify and raise its edit window upon request under certain X11 window
managers.
.SH EXAMPLE
.PP
.TP 10
gnudoit -q '(server-make-window-visible)' '(mh-smail)'
.TP 10
gnuclient -h otter -p /net/otter /tmp/*
.SH SECURITY
\fBgnuserv\fP admits a limited form of security at the machine level. By default only
connections from the host where the server is running will be allowed. All other
server connections will be rejected with a cryptic message (which is displayed
only by gnudoit). Alternatively, if the
variable ``GNU_SECURE'' can be found in gnuserv's environment, and it names a
readable filename, then this file is opened and assumed to be a list of
hosts, one per line, from which the server will allow requests. Note that a
host may be either a internet address, or a hostname. If this file contains a
lot of hostnames then the server may take quite a time to start up.
.SH KNOWN BUGS
.PP
If GNU Emacs attempts to send a string containing a newline character to
\fBgnuserv\fP, then \fBgnuserv\fP will die.
.SH FILES
.PP
.TP 8
.B /etc/services
.TP 8
.B ~/.emacs
\fIEmacs\fP customization file, see \fIemacs(1)\fP.
.SH AUTHOR.
Andy Norman (ange%anorman@hplabs.hp.com).
@EOF
set `wc -lwc <gnuserv.1`
if test $1$2$3 != 1077514476
then
	echo ERROR: wc results of gnuserv.1 are $* should be 107 751 4476
fi

chmod 644 gnuserv.1

echo x - gnuserv.c
cat >gnuserv.c <<'@EOF'
/* -*-C-*-
 Server code for handling requests from clients and forwarding them
 on to the GNU Emacs process.

 This file is not part of GNU Emacs (yet).
 Copyright (c) 1989 Andrew P. Norman

 Copying is permitted under those conditions described by the GNU
 Emacs General Public License.

 Author: Andy Norman (ange%anorman@hplabs.hp.com)

 Please mail bugs and suggestions to the author at the above address.
*/

#include "gnulib.h"

struct entry {
  u_long host_addr;
  struct entry *next;
};

struct entry *permitted_hosts[TABLE_SIZE];


/*
  permitted -- return whether a given host is allowed to connect to the server.
*/
int permitted(host_addr)
     u_long host_addr;
{
  int key;
  struct entry *entry;
  
  /* First find the hash key */
  key = HASH(host_addr) % TABLE_SIZE;
  
  /* Now check the chain for that hash key */
  for(entry=permitted_hosts[key]; entry != NULL; entry=entry->next)
    if (host_addr == entry->host_addr) 
      return(TRUE);

  return(FALSE);

} /* permitted */


/* 
  add_host -- add the given host to the list of permitted hosts, provided it isn't
              already there.
*/	
void add_host(host_addr)
     u_long host_addr;
{
  int key;
  struct entry *new_entry;
  
  if (!permitted(host_addr)) {
    if ((new_entry = (struct entry *) malloc(sizeof(struct entry))) == NULL) {
      fprintf(stderr,"%s: unable to malloc space for permitted host entry\n",
	      progname);
      exit(1);
    }; /* if */

    new_entry->host_addr = host_addr;
    key = HASH(host_addr) % TABLE_SIZE;
    new_entry->next = permitted_hosts[key];
    permitted_hosts[key] = new_entry;
  }; /* if */

} /* add_host */


void setup_table()
{
  FILE *host_file;
  char *file_name;
  char hostname[HOSTNAMSZ];
  u_long host_addr;
  int i;
  
  /* Make sure every entry is null */
  for (i=0; i<TABLE_SIZE; i++)
    permitted_hosts[i] = NULL;

  if (((file_name = getenv("GNU_SECURE")) != NULL &&
       (host_file = fopen(file_name,"r")) != NULL)) {
    while ((fscanf(host_file,"%s",hostname) != EOF)) 
      if ((host_addr = internet_addr(hostname)) != -1)
	add_host(host_addr);

    fclose(host_file);
  }
  else { /* file not available from environment so only allow this host */
    gethostname(hostname,HOSTNAMSZ);

    if ((host_addr = internet_addr(hostname)) == -1) {
      fprintf(stderr,"%s: unable to find %s in /etc/hosts or from YP", 
	      progname,hostname);
      exit(1);
    }; /* if */

    add_host(host_addr);
  }; /* else */

} /* setup_table */


/*
  init -- initialize server, returning a socket that can be listened on.
*/
int init()
{
  int ls;			/* socket descriptor */
  int chan;			/* unix channel number */
  struct servent *sp;		/* pointer to service information */
  struct sockaddr_in myaddr_in;	/* for local socket address */
  char *ptr;			/* ptr for getenv */

  setup_table();

  for(chan=3; chan < _NFILE; close(chan++)) /* close unwanted channels */
    ;

  /* clear out address structure */
  memset ((char *)&myaddr_in, 0, sizeof(struct sockaddr_in));
  
  /* Set up address structure for the listen socket. */
  myaddr_in.sin_family = AF_INET;
  myaddr_in.sin_addr.s_addr = INADDR_ANY;
  
  /* Find the information for the gnu server
   * in order to get the needed port number.
   */
  if ((ptr=getenv("GNU_PORT")) != NULL)
    myaddr_in.sin_port = htons(atoi(ptr));
  else {
    if ((sp = getservbyname ("gnuserv", "tcp")) == NULL) {
      fprintf(stderr,"%s: unable to get \"gnuserv\" from /etc/services or from YP\n",
	      progname);
      exit(1);
    }; /* if */
    
    myaddr_in.sin_port = sp->s_port;
  }; /* else */
  
  /* Create the listen socket. */
  if ((ls = socket (AF_INET, SOCK_STREAM, 0)) == -1) {
    perror(progname);
    fprintf(stderr,"%s: unable to create socket\n",progname);
    exit(1);
  }; /* if */
  
  /* Bind the listen address to the socket. */
  if (bind(ls, &myaddr_in, sizeof(struct sockaddr_in)) == -1) {
    perror(progname);
    fprintf(stderr,"%s: unable to bind socket\n",progname);
    exit(1);
  }; /* if */

  /* Initiate the listen on the socket so remote users
   * can connect. 
   */
  if (listen(ls,20) == -1) {
    perror(progname);
    fprintf(stderr,"%s: unable to listen\n",progname);
    exit(1);
  }; /* if */
  
/*  setpgrp(); */
  return(ls);
  
} /* init */


/*
  handle_request -- accept a request from a client and send the information
                    to stdout (the gnu process).
*/
void handle_request(ls)
int ls;
{
  char buf[BUFSIZ];
  int s;
  int len;
  int addrlen = sizeof(struct sockaddr_in);
  struct sockaddr_in peeraddr_in;	/* for peer socket address */

  memset ((char *)&peeraddr_in,0,sizeof(struct sockaddr_in));

  if ((s = accept(ls,&peeraddr_in,&addrlen)) == -1) {
    perror(progname);
    fprintf(stderr,"%s: unable to accept\n",progname);
    exit(1);
  }; /* if */

  /* Check that access is allowed - if not return crud to the client */
  if (!permitted(peeraddr_in.sin_addr.s_addr)) {
    send_string(s,"gnudoit: Connection refused\ngnudoit: unable to connect to remote");
    close(s);
    return;
  }; /* if */

  printf("%d ",s);
  
  while (len = recv(s,buf,BUFSIZ,0)) {
    if (len < 0) {
      perror(progname);
      fprintf(stderr,"%s: unable to recv\n",progname);
      exit(1);
    }; /* if */
	
    buf[len] = '\0';
    printf("%s",buf);
  }; /* while */
  
  printf("\n");

} /* handle_request */


/*
  handle_response -- accept a response from stdin (the gnu process) and pass the
                     information on to the relevant client.
*/
void handle_response()
{
  char buf[BUFSIZ];
  char response[BUFSIZ];
  int s;
  int len;

  if ((len = read(0,buf,BUFSIZ)) < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to read\n",progname);
    exit(1);
  }; /* if */
      
  /* parse the response from gnu */
  response[0] = '\0';
  sscanf(buf,"%d:%[^\n]\n", &s, response);
  
  /* Send a response back to the client. */
  send_string(s,response);
  close(s);

} /* handle_response */


main(argc,argv)
int argc;
char *argv[];
{
  int ls;

  progname = argv[0];
  ls = init();			/* init and get a socket to listen on */

  while (1) {
    int rmask = (1 << ls) + 1;
    
    if (select(ls + 1,&rmask,0,0,0) < 0) {
      perror(progname);
      fprintf(stderr,"%s: unable to select\n",progname);
      exit(1);
    }
    
    if (rmask & (1 << ls))	/* from socket (client process) */
      handle_request(ls);
    
    if (rmask & 1) 		/* from stdin (gnu process) */
      handle_response();

  }; /* while */

} /* main */

@EOF
set `wc -lwc <gnuserv.c`
if test $1$2$3 != 2778586509
then
	echo ERROR: wc results of gnuserv.c are $* should be 277 858 6509
fi

chmod 644 gnuserv.c

echo x - gnuserv.el
cat >gnuserv.el <<'@EOF'
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
@EOF
set `wc -lwc <gnuserv.el`
if test $1$2$3 != 2309297969
then
	echo ERROR: wc results of gnuserv.el are $* should be 230 929 7969
fi

chmod 644 gnuserv.el

echo x - x11fns.diff
cat >x11fns.diff <<'@EOF'
*** x11fns.c~	Mon Jan  9 17:30:02 1989
--- x11fns.c	Mon Jan  9 17:30:23 1989
***************
*** 827,832
  #endif				/* subprocesses */
  }
  
  syms_of_xfns ()
  {
    /* If not dumping, init_display ran before us, so don't override it.  */

--- 827,848 -----
  #endif				/* subprocesses */
  }
  
+ DEFUN ("x-remap-window", Fx_remap_window, Sx_remap_window,
+   0, 0, 0,
+   "Maps / raises the X window such that is now visible.")
+   ()
+ {
+ 
+   if (WindowMapped)
+     XRaiseWindow(XXdisplay,XXwindow);
+   else
+     XMapRaised(XXdisplay,XXwindow);
+ 
+   XFlush(XXdisplay);
+   return Qnil;
+ }
+ 
+ 
  syms_of_xfns ()
  {
    /* If not dumping, init_display ran before us, so don't override it.  */
***************
*** 876,881
    defsubr (&Sx_rebind_keys);
  #endif notdef
    defsubr (&Sx_debug);
  }
  
  #endif /* HAVE_X_WINDOWS */

--- 892,898 -----
    defsubr (&Sx_rebind_keys);
  #endif notdef
    defsubr (&Sx_debug);
+   defsubr (&Sx_remap_window);
  }
  
  #endif /* HAVE_X_WINDOWS */
@EOF
set `wc -lwc <x11fns.diff`
if test $1$2$3 != 511381005
then
	echo ERROR: wc results of x11fns.diff are $* should be 51 138 1005
fi

chmod 644 x11fns.diff

exit 0


