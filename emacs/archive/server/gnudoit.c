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

