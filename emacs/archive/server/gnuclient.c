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

