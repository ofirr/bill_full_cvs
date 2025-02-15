/* $Header: /baz/users/cvs-root/emulator/global.h,v 1.2 1994/12/04 07:45:31 avshalom Exp $ */
/*
**  global.h  -  definition of global variables
**
**	Last update by 	     $Author: avshalom $
**		       	     $Date: 1994/12/04 07:45:31 $
**	Currently locked by  $Locker:  $
**			     $Revision: 1.2 $
**			     $Source: /baz/users/cvs-root/emulator/global.h,v $
*/


/*
**  Registers
*/

extern	linkP	LinkBase;	/* Pointer to base of Link */
extern	linkP	LinkEnd;	/* Pointer to end of Link */

extern	heapP	HeapBase;	/* Pointer to base of Heap */
extern	heapP	HeapEnd;	/* Pointer to end of Heap */
extern  heapP   HeapStart;      /* Pointer to start of unfrozen Heap */
extern	heapP	CurHeap;	/* Pointer to current Heap */
extern	heapP	CurHeapEnd;	/* Pointer to the end of current heap */
extern	heapP   CurHeapLimit;	/* Pointer to CurHeapEnd - Heap_TH */
extern	heapP	OtherHeap;	/* Pointer to the other heap */
extern	heapP	OtherHeapEnd;	/* Pointer to the end of other heap */

/*
** Data Structures
*/

extern	heapP	Constants[];    /* Constants Table */

extern	workerP	W;

/* utility functions */
extern	int	hash();
extern	realT	real_val();
extern	int	real_copy();
extern	int	invalid();
extern	int	is_shared();
extern	int	is_shared_we();
extern	int	is_shared_ro();
extern	heapP	new_list_element();
extern	int	non_empty_sus_list();
extern	heapP	produce_string();
extern	heapP	shared_info();
extern	int	var_info();
extern	int	waiting_to();
extern	heapP	wrt_of_ro();

#define	LP			(W->LP_W)

#define	HP			(W->HP_W)
#define	HB			(W->HB_W)

#define	QF			(W->QF_W)
#define	HQB			(W->HQB_W)
#define	QB			(W->QB_W)

#define	SQF			(W->SQF_W)
#define	SQB			(W->SQB_W)

#define	CP			(W->CP_W)
#define	HighQ			(W->HighQ_W)

#define	AsgnTrl			(W->AsgnTrl_W)
#define	AsgnTRP			(W->AsgnTRP_W)
#define	ChngTrl			(W->ChngTrl_W)
#define	ChngTRP			(W->ChngTRP_W)
#define	SusTbl			(W->SusTbl_W)
#define	STP			(W->STP_W)
#define	TempSus			(W->TempSus_W)

#ifdef	DEBUG
#define	PrevSTP			(W->PrevSTP_W)
#endif

#define	ErrTbl			(W->ErrTbl_W)
#define	ERRP			(W->ERRP_W)

#define	TimeSlice		(W->TimeSlice_W)
#define	TS			(W->TS_W)
#define	DevsTime		(W->DevsTime_W)

#define	Last_Redo_GC		(W->Last_Redo_GC_W)

#define	McnInP			(W->McnInP_W)
#define	McnOutP			(W->McnOutP_W)
#define	McnOutM			(W->McnOutM_W)

#define	DevsNo			(W->DevsNo_W)

#define	Nil			(W->Nil_W)
#define	SVRMarker		(W->SVRMarker_W)

#define	TimeDelta		(W->TimeDelta_W)

#define	KOutA			(W->KOutA_W)
#define	KOutB			(W->KOutB_W)
#define	KOutC			(W->KOutC_W)

#define	Creations		(W->Creations_W)
#define	Suspensions		(W->Suspensions_W)
#define	Activations		(W->Activations_W)
#define	Failures		(W->Failures_W)
#define	Losses			(W->Losses_W)
#define	Switches		(W->Switches_W)
#define	Reductions		(W->Reductions_W)
#define	Terminations		(W->Terminations_W)
#define	CpuTime			(W->CpuTime_W)
#define	GCTime			(W->GCTime_W)
#define	Collections		(W->Collections_W)
#define	FreedAverage		(W->FreedAverage_W)
#define	CopiedAverage		(W->CopiedAverage_W)
#define	GCMinFlt		(W->GCMinFlt_W)
#define	GCMajFlt		(W->GCMajFlt_W)

#ifdef	DEBUG
#define	Debug_Process		(W->Debug_Process_W)
#define	Debug_Clause		(W->Debug_Clause_W)
#define	Debug_Guard		(W->Debug_Guard_W)
#define	Debug_Outargs		(W->Debug_Outargs_W)
#define	Debug_Activation	(W->Debug_Activation_W)
#define	Debug			(W->Debug_W)
#define	Debug_Unset		(W->Debug_Unset_W)
#define	Debug_Buffered		(W->Debug_Buffered_W)
#endif

#define	X			(W->X_W)
#define	FLs			(W->FLs_W)
#define	StrOrder		(W->StrOrder_W)

