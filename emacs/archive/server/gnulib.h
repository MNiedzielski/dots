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
#define	_NFILE 32

#define FALSE 0
#define TRUE 1
