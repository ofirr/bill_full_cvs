/*
Circadian rhythm emulation

Aviv Regev

Last update by          $Author: bill $
                        $Date: 2004/05/31 07:01:52 $
Currently locked by     $Locker:  $
                        $Revision: 1.7 $
                        $Source: /home/bill/Repository/Aspic/spifcp/call.cp,v $

Copyright (C) 1999, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-language(spifcp).
-include(rates).

export(TAU).
public(t2a(Think2a) , t6a(Think6a)).


TAU ::=
	<<
	
	THINK(t2a) | THINK(t6a)  .

THINK(t) ::=
    	    t ! [] , THINK
	>>

.


