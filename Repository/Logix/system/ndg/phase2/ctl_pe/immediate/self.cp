/* $Header: /baz/users/cvs-root/Source/system/ndg/phase2/ctl_pe/immediate/self.cp,v 1.1.1.1 1993/12/31 10:42:23 fcp Exp $ */
-language([compound,typed]).
-mode(trust).
-export([immediate_activation/8, evaluate/6]).

procedure immediate_activation(Imm, Bodies, Ctl, Dic, BodiesO, Ch, SC, Done).

immediate_activation(Imm, Bodies, Ctl, Dic, BodiesO, Ch, SC, Done)
:-
	immediate_activation#immediate_activation(
			Imm, Bodies, Ctl, Dic, BodiesO, Ch, SC, Done).

procedure evaluate(Dg, Dic, Imm, StL, Ch, Done).

evaluate(Dg, Dic, Imm, StL, Ch, Done)
:-
	evaluate#eval(Dg, Dic, Imm, StL, Ch, Done).

