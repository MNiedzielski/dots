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

