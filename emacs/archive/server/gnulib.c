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
