/* $Header: /baz/users/cvs-root/emulator/link_static.c,v 1.2 1994/12/04 08:38:43 avshalom Exp $ */

#include	<stdio.h>
#include	"fcp.h"

/* Made by mkmk */
#include "link_static.h"

int
link_lookup(Name)
     char *Name;
{
  char *Cp;
  register int I;

  if ((Cp = ((char *) rindex(Name,'/'))) != NULL) {
    Name = Cp+1;
  }
#if	NUM_FUNCTIONS
  for (I = 0; I < NUM_FUNCTIONS; I++) {
    if (strcmp(Name, FuncName[I]) == 0) {
      break;
    }
  }
  return((I < NUM_FUNCTIONS) ? (I+1) : 0);
#else
  return(0);
#endif
}

int
link_execute(Index, Parm)
     int Index;
     int *Parm;
{
#if	NUM_FUNCTIONS
  if (Index <= NUM_FUNCTIONS) {
    if ((*Func[Index-1])(Parm)) {
      return(True);
    }
  }
#endif
  return(False);
}
