DEST=.
CC=gcc
CFLAGS=-O

all:	${DEST}/gnuclient ${DEST}/gnuserv ${DEST}/gnudoit

${DEST}/gnuclient: gnulib.h gnulib.o gnuclient.c
	$(CC) ${CFLAGS} gnuclient.c gnulib.o -o $@

${DEST}/gnuserv: gnulib.h gnulib.o gnuserv.c
	$(CC) ${CFLAGS} gnuserv.c gnulib.o -o $@

${DEST}/gnudoit: gnulib.h gnulib.o gnudoit.c
	$(CC) ${CFLAGS} gnudoit.c gnulib.o -o $@

gnulib.o: gnulib.h gnulib.c
	$(CC) -c ${CFLAGS} gnulib.c
