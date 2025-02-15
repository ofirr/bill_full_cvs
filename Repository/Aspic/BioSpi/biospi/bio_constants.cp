/*
Precompiler for Stochastic Pi Calculus procedures - symbol definitions.

Bill Silverman, December 2001.

Last update by          $Author: bill $
                        $Date: 2004/05/31 07:01:52 $
Currently locked by     $Locker:  $
                        $Revision: 1.7 $
                        $Source: /home/bill/Repository/Aspic/spifcp/call.cp,v $

Copyright (C) 1999, Weizmann Institute of Science - Rehovot, ISRAEL

*/

/* Characters */

COMMUNICATION_SUFFIX => 39. % Prime

/* Strings */

AMBIENT => "Ambient".
SCHEDULER_DOT => "Scheduler.".
SCHEDULER_DOT_PRIME => "Scheduler.'".
SERVICE => "Service".

/* Capabilities */

ACCEPT => "accept".
ENTER => "enter".

EXIT => "exit".
EXPEL => "expel".
LOCAL => "local".
MERGE => "merge".

/* Directions */

C2P => "c2p".
P2C => "p2c".
S2S => "s2s".

/* Variables */

BIO_AMBIENT => `"Ambient.".
BIO_CHOSEN => `"Chosen.".
BIO_MESSAGE => `"Message.".
BIO_NULL => `"_".
BIO_READY => `"Ready.".
BIO_SCHEDULER => `SCHEDULER_DOT.
BIO_SCHEDULER_PRIME => `SCHEDULER_DOT_PRIME.

