/* $Header: /baz/users/cvs-root/emulator/global.c,v 1.1.1.1 1994/11/21 14:44:28 fcp Exp $ */
/*
**  global.c  -  declaration and initialisation of global variables
**		 This is the one file which does NOT include global.h
**
**	Last update by 	     $Author: fcp $
**		       	     $Date: 1994/11/21 14:44:28 $
**	Currently locked by  $Locker:  $
**			     $Revision: 1.1.1.1 $
**			     $Source: /baz/users/cvs-root/emulator/global.c,v $
*/

#include "fcp.h"
#include "codes.h"

/*
**  Registers
*/

linkP	LinkBase;		/* Pointer to base of Link */
linkP	LinkEnd;		/* Pointer to end of Link */

heapP	HeapBase;		/* Pointer to base of Heap */
heapP	HeapEnd;		/* Pointer to end of Heap */
heapP   HeapStart;		/* Pointer to start of unfrozen Heap */
heapP	CurHeap;		/* Pointer to current Heap */
heapP	CurHeapEnd;		/* Pointer to the end of current heap */
heapP   CurHeapLimit;		/* Pointer to CurHeapEnd - Heap_TH */
heapP	OtherHeap;		/* Pointer to the other heap */
heapP	OtherHeapEnd;		/* Pointer to the end of other heap */

workerT	Worker;
workerP	W = &Worker;			/* Pointer to the worker */

/*
**  Data Structures
*/

heapP Constants[ConstSize];		/* Constants Table */

