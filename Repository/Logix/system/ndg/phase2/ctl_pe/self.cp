/* $Header: /baz/users/cvs-root/Source/system/ndg/phase2/ctl_pe/self.cp,v 1.1.1.1 1993/12/31 10:42:21 fcp Exp $ */
-language([compound,typed]).
-export([ctls/5]).
-mode(trust).

Entry ::= {Areg,Vreg,Type,Value}.

Type ::= new ; load ; car ; addr ; sub_arg ; deref ;
	list ; nil ; string ; number ; integer ; real ; constant ; 
	tuple ; compound ; known ; unknown ; vector ; module ; var.

TypeCheck ::= list ; string ; number ; integer ; real ; constant ; tuple ;
		compound ; known ; unknown ; vector ; module ; var.

procedure ctls(Dgs, Ctls, Iterate, Pr, SC).

ctls(Dgs, Ctls, Iterate, Pr, SC)
:-
	ctls#ctls(Dgs, Ctls, Iterate, Pr, SC).
