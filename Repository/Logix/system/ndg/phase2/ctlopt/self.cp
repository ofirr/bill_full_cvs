/* $Header: /baz/users/cvs-root/Source/system/ndg/phase2/ctlopt/self.cp,v 1.1.1.1 1993/12/31 10:42:25 fcp Exp $ */
-language(compound).
-export([ctlopts/3]).
-mode(trust).

procedure ctlopts(Ctl, CtlOpt, SC).

ctlopts(Ctl, CtlOpt, SC) :-
	ctlopt#ctlopt(Ctl, CtlOpt, SC).
