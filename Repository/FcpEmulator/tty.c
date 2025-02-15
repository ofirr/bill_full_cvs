/* $Header: /baz/users/cvs-root/emulator/tty.c,v 1.1.1.1 1994/11/21 14:44:31 fcp Exp $ */
/*
**	tty.c - a device for reading from keyboard.
**
**	Avshalom Houri
**
*/

#include <errno.h>
#include <string.h>
#include <stdio.h>

#include <sys/termios.h>

#include "fcp.h"
#include "codes.h"
#include "global.h"
#include "macros.h"

static int Fd = -1;		/*  file desc. of stdin  */

static struct termios termios_orig;

int handle_tty();

int
tty(T)
     heapP T;
{
  register heapT V = *(T+1);
  register heapP P = T+1;

  deref(V, P);
  if (!IsStr(V)) {
    if (IsVar(V)) {
      sus_tbl_add(P);
    }
    return(False);
  }
  switch (Arity_of(*T)) {
  case 1:
    if (strcmp((char *) (P+2), "close") != 0) {
      return(False);
    }
    handle_tty(PauseC);
    return(reset_select_entry(ReadC, Fd));
  case 2:
    if (strcmp((char *) (P+2), "open") != 0) {
      return(False);
    }
    P = T+2;
    deref_ptr(P);
    if (!IsWrt(*P)) {
      if (IsRo(*P)) {
	sus_tbl_add(P);
      }
      return(False);
    }
    Fd = fileno(stdin);
/*    if (!isatty(Fd)) {
      return(False);
    }*/
    if (!(set_select_entry(ReadC, Fd, P, ((int (*)()) Null), handle_tty))) {
      return(False);
    }
    handle_tty(ResumeC);
    return(True);
  case 3:
    if (strcmp((char *) (P+2), "read") != 0) {
      return(False);
    }
    P = T+2;
    deref_ptr(P);
    if (!IsWrt(*P)) {
      if (IsRo(*P)) {
	sus_tbl_add(P);
      }
      return(False);
    }
    return(read_tty(P, (T+3)));
  default:
    return(False);
  }
}


read_tty(StreamEnd, NewStreamEnd)
     heapP StreamEnd, NewStreamEnd;
{
  static char Buf[BUFSIZ];
  static int  Cc = 0;
  register char   *Bp;

  if (ended_heap((HP+BUFSIZ))) {
    err_tbl_add(MACHINE, ErHPSPACE);
    return(False);
  }
  Cc = read(Fd, Buf, BUFSIZ);
  next_select_token(ReadC, Fd);
  if (Cc <= 0) {
    asgn(*StreamEnd, StreamEnd, Ref_Word(NewStreamEnd));
    return(True);
  }
  asgn(*StreamEnd, StreamEnd, Ref_Word(HP));
  Bp = Buf;
  while (Cc-- > 0) {
    *HP++ = Word( *Bp++, L_IntTag);
  }
  *HP++ = Ref_Word(NewStreamEnd);
  return(True);
}

handle_tty(Event)
     int Event;
{
  switch (Event) {
  case ResumeC:
    if (isatty(Fd) && foreground_p(Fd)) {
      {
	struct termios termios_buf;

	tcgetattr(Fd, &termios_buf);
	termios_orig = termios_buf;
	termios_buf.c_lflag &= ~ICANON;
	termios_buf.c_lflag &= ~ECHO;
	termios_buf.c_cc[VMIN] = 0;
	termios_buf.c_cc[VTIME] = 0;
	tcsetattr(Fd, TCSANOW, &termios_buf);
      }
    }
    break;
  case PauseC:
  case ExitC:
    if (isatty(Fd)) {
      tcsetattr(Fd, TCSANOW, &termios_orig);
    }
    break;
  }
}

foreground_p(terminal_fd)
     int	terminal_fd;
{
  return((tcgetpgrp(terminal_fd) == getpgrp(getpid())));
}
