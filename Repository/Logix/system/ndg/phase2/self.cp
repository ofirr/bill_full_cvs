/* $Header: /baz/users/cvs-root/Source/system/ndg/phase2/self.cp,v 1.1.1.1 1993/12/31 10:42:16 fcp Exp $ */
-language(compound).
-export([ctls/5,ctlopts/3]).
-mode(trust).

procedure ctls(Dgs, Ctls, Iterate, Partial, SC).

ctls(Dgs, Ctls, Iterate, Partial, SC)
:-
	ctl_pe#ctls(Dgs, Ctls, Iterate, Partial, SC).
/*
	Partial = ms_imm(Pr) |
	ctl_pe_imm#ctls(Dgs, Ctls, Iterate, Pr, SC).
*/

procedure ctlopts(Ctls, CtlsOpt, SC).

ctlopts(Ctls, CtlsOpt, SC)
:-
	ctlopt#ctlopts(Ctls, CtlsOpt, SC).
