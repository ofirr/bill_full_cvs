/* $Header: /baz/users/cvs-root/Source/system/ndg/phase2/ctlopt/ctlopt.cp,v 1.1.1.1 1993/12/31 10:42:25 fcp Exp $ */
-language(compound).
-export([ctlopt/3]).
-mode(trust).

procedure ctlopt(Ctl, CtlOpt, SC).

ctlopt(CTLs, CTLsOPT, SC) :-

	CTLs ? {ProcId, Instructions} ,
	SC = {L, R} :
	CTLsOPT ! {ProcId, Instructions'}, SC' = {M1, R} | 
		ctlopt1#instructions(Instructions, Instructions1, {L,M}),
		peephole#peephole(Instructions1,Instructions',{M,M1}),
		ctlopt;

	CTLs = [] ,
	SC = {L,R} :
	CTLsOPT = [],
	L = R  |
		true.
