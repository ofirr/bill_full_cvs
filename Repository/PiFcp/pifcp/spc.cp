/*
Precompiler for Pi Calculus procedures - Stochastic Pi Calculus Phase.

Bill Silverman, February 1999.

Last update by		$Author: bill $
		       	$Date: 2000/02/23 11:47:49 $
Currently locked by 	$Locker:  $
			$Revision: 1.3 $
			$Source: /spring/users1/Bill/Repository/PiFcp/pifcp/servers.cp,v $

Copyright (C) 2000, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export(output/4).
-mode(interrupt).
-language(compound).

/*
** output/4
**
** Input:
**
**   In is a stream of  Type(Atom, RHSS, Action)
**
**      Type is one of {export, outer,
**			no_guard, none,
**			compare, logix,
**			receive, send, mixed}.
**
**      Atom is an Fcp atom of a compond procedure,
**
**        ProcedureName(<arguments>)
**
**      RHSS is the right-hand-side of the compond procedure.
**
**      Action is [] or the compound procedure
**
**        ProcedureName.Type<arguments'>)
**
**      where Type is in {send, mixed}.
**
**   Exports is a list of  String/Integer.
**
**   Delay is one of {none, stochastic}.
**
** Output:
**
**   Terms is a stream of compound procedures.
*/

output(In, Exports, Delay, Terms) :-

%    Delay =?= none,
    In ? _Type(Atom, RHSS, []) :
      Terms ! (Atom :- RHSS) |
	self;

%    Delay =?= none,
    In ? _Type(Atom, RHSS, Procedure), Procedure =\= [] :
      Terms ! (Atom :- RHSS),
      Terms' ! Procedure |
	self;

    In = [] :
      Exports = _,
      Delay = _,
      Terms = [].
