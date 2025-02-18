/*

Pre-compiler boot program transformer

Bill Silverman 1986

Last update by		$Author: fcp $
		       	$Date: 1993/12/31 10:42:00 $
Currently locked by 	$Locker:  $
			$Revision: 1.1.1.1 $
			$Source: /baz/users/cvs-root/Source/system/compile/control/run.cp,v $

Copyright (C) 1986, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([system/2]).
-mode(trust).
-language(compound).


List ::= [Any].

procedure system(Procedures, Procedures).

Procedures ::= [procedure(Any, [{boot(Any, Any), {List, List}, List} | List])].


system(SourceProcedures, BootedProcedures) :-
    SourceProcedures ? procedure(Id, Boots) :
      BootedProcedures = [procedure(Id, NewBoots) | SourceProcedures'] |
	boot_procedure(Boots, NewBoots).


boot_procedure(Boots, NewBoots) :-
    Boots ? Clauses :
      NewBoots ! NewClauses |
	boot_clause(Clauses, NewClauses),
	boot_procedure.
boot_procedure([], []^).

boot_clause(	{ {Boot, `In, Out},
			{Ask, Tell},
			Body
		},
		{ {Boot, `boot(in), Out},
			{Ask, [`boot(in) = [system | `In] | Tell]},
			Body
		}^
).
boot_clause(Clauses, Clauses^) :-
    otherwise |
	true.
