/* $Header: /baz/users/cvs-root/Source/system/ndg/phase0/self.cp,v 1.1.1.1 1993/12/31 10:42:13 fcp Exp $ */
-language([compound]).
-scope(ndg).
-export([preproc/2,flow_analysis/2]).
-mode(trust).

procedure preproc(Procedures, Procs).

preproc(Procedures, Procs) :- preproc#preproc(Procedures, Procs).

procedure flow_analysis(Procs, Procs1).

flow_analysis(Procs, Procs1) :- flow_analysis#flow_analysis(Procs, Procs1).



