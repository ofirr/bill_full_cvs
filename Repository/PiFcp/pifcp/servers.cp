/*
Precompiler for Pi Calculus procedures - servers.

Bill Silverman, December 1999.

Last update by		$Author: bill $
		       	$Date: 2000/02/10 10:04:31 $
Currently locked by 	$Locker:  $
			$Revision: 1.15 $
			$Source: /spring/users1/Bill/Repository/PiFcp/pifcp.cp,v $

Copyright (C) 1999, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([serve_empty_scope/6, make_guard_receive/5]).
-mode(interrupt).
-language(compound).

/*
** serve_empty_scope/6+5
**
** Serve requests from first-level processes.  See serve_process_scope/6+6
**
** Input:
**
**   In is the request input stream:
**
**      error(Diagnostic) - copy Diagnostic to Errors stream.
**
**      lookup_functor(Functor, CallType, CallDefinition)
**
**          Lookup process Functor and return:
**               CallType in {none, outer, inner}
**               CallDefinition = {Name, Arity, ChannelNames,
**			OuterLHS, InnerLHS, Mode(SendRHS, ProcessRHS)}

**
**      process(PiLHS, Status, ProcessScope)
**
**          Analyze PiLHS (Left-hand-side) of process declaration and return
**               Status is one of  {single, double, nil} ;
**               ProcessScope - a stream to a process context handler.
**
**   GlobalList is a list of global channels.
**
**   Exported is a list of exported  Name  or  Name/Arity.
**
** Output:
**
**   Entries is a list of generated processes for first level procedure entry.
**
**   Errors is a list of error diagnostics.
**
** Local:
**
**   Progeny is a list of child ProcessDefinitions.
**
**   Refs, AddRef is a list of deferred lookup_functor/3 requests.
*/

serve_empty_scope(In, GlobalList, Exported, Exports, Entries, Errors) +
		(Progeny = [], Refs = AddRef?, AddRef,
		 Sums, Summed = Sums?) :-

    In ? error(Error) :
      Errors ! Error |
	self;

    In ? lookup_functor(Functor, CallType, CallDefinition) :
      AddRef ! lookup_functor(Functor, CallType, CallDefinition) |
	self;

    In ? process(PiLHS, Status, ProcessScope) |
	make_process_scope(PiLHS, ProcessScope, [], In'', In'?,
		NewDefinition?, ProcessDefinition, Errors, Errors'?),
	compute_status,
	export_process(ProcessDefinition?, Exported, Export,
				Exports, Exports'?),
	create_entry(GlobalList, Export?, ProcessDefinition?, NewDefinition,
			Entries, Entries'?),
	add_process_definition(NewDefinition?, Progeny, Progeny'),
	self;

    In ? call_sum(Name, Sum, Call) :
      Sums ! Name(Sum, Call) |
	self;

    In = [] :
      GlobalList = _,
      Exported = _,
      AddRef = [],
      Exports = [],
      Sums = [] |
	find_process_refs(Refs, Progeny, [], []),
	/* sum_procedures. */
	call#sum_procedures(Summed, Entries, Errors).


create_entry(GlobalList, Export, ProcessDefinition, NewDefinition,
		Entries, NextEntries) :-


    ProcessDefinition =?= {Name, Arity, ChannelNames, OuterLHS, _InnerLHS,
					CodeTuple},
    Name =\= "_",
    GlobalList =\= [],
    Export = true,
    Index := arity(OuterLHS),
    Index++,
    string_to_dlist(Name, NL, []) :
      ascii(' ', Space),
      Entries ! (Atom? :- Initializer?),
      NextEntries = Entries',
      NewDefinition = {Name, Arity, ChannelNames'?, OuterLHS'?, InnerLHS'?,
					CodeTuple} |
	piutils#tuple_to_atom(OuterLHS, Atom),
	list_to_string([Space|NL], Name'),
	split_channels(1, Index, ChannelNames, ParamList, ChannelList),
	make_lhs_tuples,
	initialize_global_channels(Index', OuterLHS'?, Initializer);

    ProcessDefinition =?= {Name, Arity, ChannelNames, OuterLHS, _InnerLHS,
					CodeTuple},
    Name =\= "_",
    GlobalList =\= [],
    Export = false,
    Index := arity(OuterLHS) :
      NextEntries = Entries,
      NewDefinition = {Name, Arity, ChannelNames'?, OuterLHS'?, InnerLHS'?,
					CodeTuple} |
	split_channels(1, Index, ChannelNames, ParamList, ChannelList),
	make_lhs_tuples;
	
    otherwise :
      GlobalList = _,
      Export = _,
      NewDefinition = ProcessDefinition,
      Entries = NextEntries.

  split_channels(I, Index, ChannelNames, ParamList, ChannelNameList) :-

    ChannelNames ? ChannelName,
    I < Index,
    I++ :
      ParamList ! ChannelName |
	self;

    ChannelNames ? ChannelName,
    I >= Index,
    I++ :
      ChannelNameList ! ChannelName |
	self;

    ChannelNames =?= [] :
      I = _,
      Index = _,
      ParamList = [],
      ChannelNameList = [].


  initialize_global_channels(Index, Tuple, Initializer)
				+ (List = Tail?, Tail) :-

    Index =< arity(Tuple),
    arg(Index, Tuple, `ChannelName),
    Index++ :
      Tail ! ChannelName(`ChannelName) |
	self;

    Index > arity(Tuple),
    arg(1, Tuple, ProcedureName) :
      Tail = [],
      Initializer = (pi_monitor#global_channels(List), ProcedureName).


/*
** serve_process_scope/6+3
**
** Serve requests from first-level processes.  See serve_process_scope/6+6
**
** Input:
**
**   In is the request input stream:
**
**      lhss(Outer, Inner) - return ProcessDefinition's
**                            OuterLHS and InnerLHS.
**
**      body_receive(Channel, Body, ChannelVar, ChannelList)
**
**      body_send(Message, Channel, Sender, ChannelList, ChannelVar)
**
**          
**
**      error(Diagnostic) - add  Name-Diagnostic  to Errors stream.
**
**      lookup_functor(Functor, CallType, CallDefinition)
**
**          Lookup process Functor and return:
**               CallType in {none, outer, inner}
**               CallDefinition = {Name, Arity, ChannelNames,
			OuterLHS, InnerLHS, Mode(SendRHS, ProcessRHS)}
**
**
**      process(PiLHS, Status, ProcessScope)
**
**          Analyze PiLHS (Left-hand-side) of process declaration and return
**               Status is one of  {single, double, nil} ;
**               ProcessScope - a stream to a process context handler.
**
**   NextOut, NextErrors are continuation streams.
**
** Output:
**
**   Out is a list of lookup_functor/3 requests which cannot be satisfied
**   at this level.
**
**   Errors is defined in serve_empty_scope.
**
** Local: see serve_empty_scope/6+3 above.
*/

serve_process_scope(In, ProcessDefinition, Out, NextOut, Errors, NextErrors) +
		(IdIndex = 1, Primes = [], Locals = [], Progeny = [],
		 Refs = AddRef?, AddRef) :-

    In ? lhss(Outer, Inner),
    ProcessDefinition =?= {_Name, _Arity, _ChannelNames, OuterLHS, InnerLHS,
					_CodeTuple} :
      Outer = OuterLHS,
      Inner = InnerLHS |
	self;

    In ? body_receive(FromChannel, Message, ChannelVar, ChannelList),
    ProcessDefinition =?= {Name, _Arity, ChannelNames, _OuterLHS, _InnerLHS,
					_CodeTuple} :
      ChannelVar = `ChannelName' |
	piutils#verify_channel(Name, FromChannel, ChannelNames, Locals,
				ChannelName, Errors, Errors'?),
	call#prime_local_channels(Primes, [ChannelName], [ChannelName']),
	message_to_channels(Message, Name, ChannelNames, Locals, true,
				MsChannelNames, Errors', Errors''?),
	piutils#names_to_channel_list(MsChannelNames?, ChannelList),
	self;

    In ? body_send(Message, ToChannel, Sender, ChannelList, ChannelVar),
    ProcessDefinition =?= {Name, _Arity, ChannelNames, _OuterLHS, _InnerLHS,
					_CodeTuple} :
      ChannelVar = `ChannelName'? |
	piutils#verify_channel(Name, ToChannel, ChannelNames, Locals,
				ChannelName, Errors, Errors'?),
	make_sender,
	message_to_channels(Message, Name, ChannelNames, Locals, false,
				MsChannelNames, Errors', Errors''?),
	call#prime_local_channels(Primes, [ChannelName? | MsChannelNames?],
					  [ChannelName' | MsChannelNames']),
	piutils#names_to_channel_list(MsChannelNames'?, ChannelList),
	self;

    In ? call(Body1, Body2) |
	call#make_local_call(ProcessDefinition, Locals, Primes, Body1, Body2,
				In'', In'?, Errors, Errors'?, _CallDefinition),
	self;

    In ? error(Description),
    arg(1, ProcessDefinition, Name) :
      Errors ! (Name - Description) |
	self;

    In ? guard_compare(Guard, ChannelList, NextChannelList, Comparer),
    Guard =?= {Operator, C1, C2},
    ProcessDefinition =?= {Name, _Arity, ChannelNames, _OuterLHS, _InnerLHS,
					_CodeTuple} |
	message_to_channels({C1, C2}, Name, ChannelNames, Locals, false, List,
				Errors, Errors'?),
	compare_channels_ok,
	self;

    In ? guard_receive(Channel, Message, SendId, Iterates, Consume),
    ProcessDefinition =?= {Name, _Arity, ChannelNames, _OuterLHS, _InnerLHS,
					_CodeTuple} :
      Primes = _ |
	piutils#verify_channel(Name, Channel, ChannelNames, Locals,
				ChannelName', Errors, Errors'?),
	parse_message(Name, ChannelNames, Message, MsChannelNames,
			Locals', Primes', Errors', Errors''?),
	call#prime_local_channels(Primes'?, MsChannelNames?, MsChannelNames'),
	piutils#names_to_channel_list(MsChannelNames'?, ChannelList),
	make_guard_receive,
	self;

    In ? guard_send(Channel, Message, SendId, SendIndex, Guard),
    ProcessDefinition =?= {Name, _Arity, ChannelNames, _OuterLHS, _InnerLHS,
					_CodeTuple} |
	piutils#verify_channel(Name, Channel, ChannelNames, Locals,
					ChannelName,
				Errors, Errors'?),
	message_to_channels(Message, Name, ChannelNames, Locals, false,
				MsChannelNames, Errors', Errors''?),
	call#prime_local_channels(Primes, [ChannelName? | MsChannelNames?],
					  [ChannelName' | MsChannelNames']),
	/* Check channels */
	piutils#names_to_channel_list(MsChannelNames'?, ChannelList),
	make_guard_sender,
	make_guard_send,
	self;

    In ? lookup_functor(Functor, CallType, CallDefinition),
    arg(1, ProcessDefinition, Functor) :
      CallType = inner,
      CallDefinition = ProcessDefinition |
	self;

    In ? lookup_functor(Functor, CallType, CallDefinition),
    arg(1, ProcessDefinition, Name), Name =\= Functor :
      AddRef ! lookup_functor(Functor, CallType, CallDefinition) |
	self;

    In ? new_scope_id(Id),
    arg(1, ProcessDefinition, Name),
    string_to_dlist(Name, DL, Tail),
    string_to_dlist(".", Dot, Suffix),
    convert_to_string(IdIndex, SI),
    IdIndex++,
    string_to_dlist(SI, SCs, []) :
      Tail = Dot,
      Suffix = SCs |
	list_to_string(DL, Id),
	self;

    In ? process(PiLHS, Status, ProcessScope),
    ProcessDefinition =?= {_Name, _Arity, ChannelNames, _OuterLHS, _InnerLHS,
					_CodeTuple} |
	piutils#concatenate_lists([Locals, ChannelNames], GlobalList),
	make_process_scope(PiLHS, ProcessScope, GlobalList,
		In'', In'?, NewDefinition?, NewDefinition, Errors, Errors'?),
	compute_status,
	add_process_definition(NewDefinition?, Progeny, Progeny'),
	self;

    In ? remote_call(Call1, Call2),
    ProcessDefinition =?= {Name, _Arity, ChannelNames, _OuterLHS, _InnerLHS,
					_CodeTuple} |
	call#make_remote_call(Name, Call1, ChannelNames, Locals, Primes, Call2,
				Errors, Errors'?),
	self;

    /* Code for communication procedures */
    In ? code(Mode, SendRHS, ProcessRHS),
    ProcessDefinition =?= {_Name, _Arity, _ChannelNames, _OuterLHS, _InnerLHS,
					CodeTuple} :
      CodeTuple = Mode?(SendRHS?, ProcessRHS?) |
	self;

    /* Pass along */
    In ? CallSum, CallSum = call_sum(_Name, _Sum, _Call) :
      Out ! CallSum |
	self;

    In = [],
    ProcessDefinition =?= {_Name, _Arity, _ChannelNames, _OuterLHS, _InnerLHS,
					CodeTuple} :
      IdIndex = _,
      Locals = _,
      Primes = _,
      AddRef = [],
      Errors = NextErrors |
	unify_without_failure(CodeTuple, none([], [])),
	find_process_refs(Refs, Progeny, Out, NextOut).

  compare_channels_ok(Operator, List, Comparer,
			ChannelList, NextChannelList) :-

    List =?= [C1, C2], C1 =\= "_", C2 =\= "_" :
      Compare = {Operator, `pinch(C1), `pinch(C2)} |
	compare_channel(C1, Extract1, ChannelList, ChannelList'),
	compare_channel(C2, Extract2, ChannelList'?, NextChannelList),
	make_comparer;

    otherwise :
      Operator = _,
      List = _,
      NextChannelList = ChannelList,
      Comparer = true.

  compare_channel(C, Extract, CL, NCL) :-

    CL ? Ch, C =\= Ch :
      NCL ! Ch |
	self;

    CL ? Ch, C =?= Ch :
      CL' = _,
      Extract = true,
      NCL = CL;

    CL = [] :
      Extract = (`C = {`"_", `pinch(C), `"_"}),
      NCL = [C].
    
  make_comparer(Extract1, Extract2, Compare, Comparer) :-

    Extract1 =?= Extract2,
    Extract1 =?= true :
      Comparer = Compare;

    /* Somebody wrote  a =?= a  or  a =\= a  */
    Extract1 =?= Extract2,
    Extract1 =\= true :
      Comparer = (Extract1, Compare);

    Extract1 =\= true,
    Extract2 =?= true :
      Comparer = (Extract1, Compare);

    Extract1 =?= true,
    Extract2 =\= true :
      Comparer = (Extract2, Compare);

    Extract1 =\= true,
    Extract2 =\= true,
    Extract1 =\= Extract2 :
      Comparer = (Extract1, Extract2, Compare).


/*
** find_process_refs/4
**
** Search for referenced processes, as part of a scope server
** termination.
**
** Input:
**
**    Refs is a stream of lookup_functor requests which have deferred until
**    the scope's definition has been completed.
**
**    Progeny is a list of child ProcessDefinitions.
**
**    NextOut is a continuation stream.
**
** Output:
**
**    Out is a stream of requests which cannot be satisfied at this level.
*/

find_process_refs(Refs, Progeny, Out, NextOut) :-

    Refs ? lookup_functor(Functor, CallType, CallDefinition) |
	search_progeny(Functor, Progeny, CallType, CallDefinition, Out, Out'),
	self;

    Refs = [] :
      Progeny = _,
      Out = NextOut.

/*
** search_progeny/6
**
** Search Progeny for a process whose Name is Functor.
**
** Input: Functor, Progeny, NextOut
**
** Output:
**
**   CallType is in {none, outer, inner}
**
**   CallDefinition is the ProcessDefinition of the found process.
**
**   Out relays unsatisied request, except at the base level.
*/

search_progeny(Functor, Progeny, CallType, CallDefinition, Out, NextOut) :-

    Progeny ? ProcessDefinition, arg(1, ProcessDefinition, Functor) :
      Progeny' = _,
      CallType = outer,
      CallDefinition = ProcessDefinition,
      Out = NextOut;


    Progeny ? ProcessDefinition, arg(1, ProcessDefinition, Name),
    Name =\= Functor |
	self;

    Progeny = [] :
      Out = [lookup_functor(Functor, CallType, CallDefinition) | NextOut];

    Progeny = [],
    Out = [] :
      Functor = _,
      CallType = none,
      CallDefinition = [],
      NextOut = [].

/*
** make_process_scope/9
**
** Analyze PiLHS, producing a ProcessDefinition and a process scope server,
** serve_process_scope.
**
** Input:
**
**   PiLHS is from a process/2 request.
**
**   GlobalList is a list of channel names which are global to the process.
**
**   NewDefinition is the ProcessDefinition used by the new scope.
**
**   NextOut, NextErrors are continuation streams.
**
** Output:
**
**   ProcessScope is a stream to serve_process_scope.
**
**   Out is a stream to relay lookup_functor/3 requests.
**
**   ProcessDefinition is defined in make_process_definition.
**
**   Errors is defined in serve_empty_scope.
*/

make_process_scope(PiLHS, ProcessScope, GlobalList,
	Out, NextOut, NewDefinition, ProcessDefinition, Errors, NextErrors) :-

    true :
      ProcessDefinition =?= {Name?, Arity?, ChannelNames?,
				OuterLHS?, InnerLHS?, _CodeTuple} |
	parse_lhs(PiLHS, Name, Arity,
		ParamList, ChannelList, Errors, Errors'?),
	piutils#check_for_duplicates(ParamList?, ParamList1,
				{Name?, duplicate_parameter},
					Errors', Errors''?),
	piutils#check_for_duplicates(ChannelList?, ChannelList1,
				{Name?, duplicate_channel},
					Errors'', Errors'''?),
	piutils#concatenate_lists([ParamList1?, ChannelList1?], LocalList),
	piutils#check_for_duplicates(LocalList?, LocalList1,
				{Name?, channel_duplicates_parameter},
					Errors''', Errors''''?),
	correct_for_duplication(LocalList?, LocalList1?, ParamList,
				ChannelList1?, ChannelList2),
	make_lhs_tuples(Name?, ParamList1?, GlobalList, ChannelList2?,
				ChannelNames, OuterLHS, InnerLHS),
	serve_process_scope(ProcessScope?, NewDefinition,
				Out, NextOut, Errors'''', NextErrors).

  correct_for_duplication(L1, L2, P, C1, C2) :-

    L1 =?= L2 :
      P = _,
      C2 = C1;

    otherwise :
      L1 = _,
      C1 = _ |
	piutils#subtract_list(L2, P, C2).
	

compute_status(NewDefinition, Status) :-

    NewDefinition =?= {_Name, _Arity, _ChannelNames, OuterLHS, _InnerLHS,
					_CodeTuple},
    OuterLHS =?= [] :
      Status = nil;    

    NewDefinition =?= {_Name, _Arity, _ChannelNames, OuterLHS, InnerLHS,
					_CodeTuple},
    OuterLHS =\= [], OuterLHS =?= InnerLHS :
      Status = single;

    otherwise :
      NewDefinition = _,
      Status = double.

export_process(ProcessDefinition, Exported, Export, Exports, NextExports) :-

    ProcessDefinition =?= {Name, Arity, _ChannelNames, _OuterLHS, _InnerLHS,
					_CodeTuple},
    Name =\= "_",
    Exported =?= all :
      Export = true,
      Exports ! (Name/Arity),
      NextExports = Exports';

    ProcessDefinition =?= {Name, Arity, _ChannelNames, _OuterLHS, _InnerLHS,
					_CodeTuple},
    Name =\= "_",
    Exported =\= all |
	exported_procedure;	

    otherwise :
      ProcessDefinition = _,
      Exported = _,
      Export = _,
      Exports = NextExports.

  exported_procedure(Name, Arity, Exported, Export, Exports, NextExports) :-

    Exported = Name :
      Export = true,
      Exports ! (Name/Arity),
      NextExports = Exports';

    Exported = (Name/Arity) :
      Export = true,
      Exports ! (Name/Arity),
      NextExports = Exports';

    Exported ? Name :
      Exported' = _,
      Export = true,
      Exports ! (Name/Arity),
      NextExports = Exports';

    Exported ? (Name/Arity) :
      Exported' = _,
      Export = true,
      Exports ! (Name/Arity),
      NextExports = Exports';

    Exported ? _Other,
    otherwise |
	self;

    Exported = [] :
      Name = _,
      Arity = _,
      Export = false,
      NextExports = Exports.


add_process_definition(ProcessDefinition, Progeny, NewProgeny) :-

    ProcessDefinition =?= {Name, _Arity, _ChannelNames, _OuterLHS, _InnerLHS,
					_CodeTuple},
    Name =\= "_" :
      NewProgeny = [ProcessDefinition | Progeny];

    otherwise :
      ProcessDefinition = _,
      NewProgeny = Progeny.


parse_lhs(PiLHS, Name, Arity, ParamList, ChannelList, Errors, NextErrors) :-

    PiLHS =?= `Functor, string(Functor), Functor =\= "" :
      Name = Functor, Arity = 0,
      ParamList = [],
      Errors = NextErrors |
	unify_without_failure(ChannelList, []);

    PiLHS = PiLHS' + Channels,
    writable(ChannelList) :
      ChannelList' = ChannelList? |
	extract_channel_list(Channels, ChannelList, Errors, Errors'),
	self;

    arg(1, PiLHS, `Functor), string(Functor), Functor =\= "",
    arity(PiLHS, A), A-- :
      Name = Functor,
      Arity = A' |
	extract_arglist(PiLHS, ParamList, Errors, NextErrors),
	unify_without_failure(ChannelList, []);

    otherwise :
      Errors ! improperly_formed_left_hand_side(PiLHS),
      PiLHS' = `"_" |
	self.

extract_channel_list(Channels, ChannelList, Errors, NextErrors) :-

    string(Channels), Channels =\= "_", Channels =\= "" :
      ChannelList = [Channels],
      Errors = NextErrors;

    Channels = `ChannelName,
    string(Channels), Channels =\= "_", Channels =\= "" :
      ChannelList = [ChannelName],
      Errors = NextErrors;

    Channels = (ChannelName, Channels'),
    string(ChannelName), ChannelName =\= "_", ChannelName =\= "" :
      ChannelList ! ChannelName |
	self;

    Channels = (`ChannelName, Channels'),
    string(ChannelName), ChannelName =\= "_", ChannelName =\= "" :
      ChannelList ! ChannelName |
	self;

    Channels = (NotChannel, Channels'),
    otherwise :
      Errors ! invalid_channel_in_channel_list(NotChannel) |
	self;

    otherwise :
      Errors = [invalid_channel_in_channel_list(Channels) | NextErrors],
      ChannelList = [].


extract_arglist(PiLHS, ParamList, Errors, NextErrors) + 
			(Index = 2) :-
    arity(PiLHS) < Index :
      ParamList = [],
      Errors = NextErrors;

    arity(PiLHS) >= Index,
    arg(Index, PiLHS, ChannelName), Index++,
    string(ChannelName), ChannelName =\= "" :
      ParamList ! ChannelName |
	self;

    arity(PiLHS) >= Index,
    arg(Index, PiLHS, `ChannelName), Index++,
    string(ChannelName), ChannelName =\= "" :
      ParamList ! ChannelName |
	self;

    otherwise,
    arg(Index, PiLHS, NotString), Index++ :
      Errors ! invalid_parameter(NotString) |
	self.


make_lhs_tuples(Name, ParamList, GlobalList, ChannelList,
		ChannelNames, OuterLHS, InnerLHS) :-

    Name =?= "_" :
      ParamList = _,
      GlobalList = _,
      ChannelList = _,
      OuterLHS = [],
      InnerLHS = [],
      ChannelNames = [];

    Name =\= "_",
    ChannelList =?= [] :
      InnerLHS = OuterLHS?|
	piutils#subtract_list(GlobalList, ParamList, GlobalList1),
	piutils#concatenate_lists([ParamList, GlobalList1?], ChannelNames),
	construct_lhs_tuple(Name, "", ChannelNames?, OuterLHS);

    Name =\= "_",
    ChannelList =\= [],
    GlobalList =?= [] |
	construct_lhs_tuple(Name, "", ParamList, OuterLHS),
	piutils#concatenate_lists([ParamList, ChannelList?], ChannelNames),
	construct_lhs_tuple(Name, ".", ChannelNames?, InnerLHS);

    Name =\= "_",
    ChannelList =\= [],
    GlobalList =\= [] |
	piutils#subtract_list(GlobalList, ParamList, GlobalList1),
	piutils#subtract_list(GlobalList1, ChannelList, GlobalList2),
	piutils#concatenate_lists([ParamList, GlobalList2?], OuterList),
	construct_lhs_tuple(Name, "", OuterList?, OuterLHS),
	piutils#concatenate_lists([OuterList?, ChannelList], ChannelNames),
	construct_lhs_tuple(Name, ".", ChannelNames?, InnerLHS).


construct_lhs_tuple(Name, Suffix, ChannelNames, Tuple) :-

    string_to_dlist(Name, NameCs, NameEnd),
    string_to_dlist(Suffix, SuffixCs, []) :
      NameEnd = SuffixCs |
	list_to_string(NameCs, Name'),
	piutils#make_lhs_tuple(Name'?, ChannelNames, Tuple).


make_guard_receive(ChannelName, ChannelList, SendId, Iterates, Consume) :-
    string_to_dlist(ChannelName, CL, Prime) :
      Prime = [39],

      Iterates =
	{(`ChannelName = {`pifcp(id), `pifcp(cv), `pifcp(mss)},
	   `pifcp(mss) = [{`"_", `"_", `"_", `pifcp(choice)}
			 | `pifcp(mssp)],
	   not_we(`pifcp(choice)) :
	     `ChannelNamePrime = {`pifcp(id), `pifcp(cv), ?pifcp(mssp)} |
		self),

	  (`ChannelName = {`pifcp(id), `pifcp(cv), `pifcp(mss)},
	   `pifcp(mss) =?= [{`SendId, `"_", `"_", `pifcp(chosen)}
			   | `pifcp(mssp)] :
	     `ChannelNamePrime = {`pifcp(id), `pifcp(cv),
					?pifcp(mssp)} |
		self)
	},

      Consume = (`ChannelName = {`"_", `"_", `pifcp(mss)},
	 `pifcp(mss) =?= [{`Sender, ChannelList, `pifcp(tag), `pifcp(choose)}
			 | `"_" /*pifcp(mssp)*/],
	  ExcludeSender? :
/* This may only be done if we can handle multiple primes - e.g.
** when the message is a <channel_list> which includes  ChannelName .
**		    `ChannelNamePrime = {`pifcp(id), `pifcp(cv), ?pifcp(mssp)},
*/
	    CancelSends?) |

	list_to_string(CL, ChannelNamePrime),
	receive_exclude_and_cancel.

  receive_exclude_and_cancel(SendId, Sender, ExcludeSender, CancelSends) :-

    SendId =?= "_" :
      Sender = "_",
      ExcludeSender = we(`pifcp(choose)),
      CancelSends = (`pifcp(choose) = `pifcp(tag));

    otherwise :
      Sender = pifcp(sender),
      ExcludeSender = (`SendId =\= `Sender, we(`pifcp(choose))),
      CancelSends = (`pifcp(chosen) = 0, `pifcp(choose) = `pifcp(tag)).


make_guard_send(ChannelName, ChannelList, Sender, SendIndex, Guard) :-

    true :
      VN = pinch(ChannelName),
      Guard = {`ChannelName = {`"_", `VN, `"_"},
		write_channel({Sender, ChannelList, SendIndex, `pifcp(chosen)},
				`VN)} .

  make_guard_sender(SendId, Name, ChannelName, Sender) :-

    SendId =?= "_" |
	make_sender;

    otherwise :
      Name = _,
      ChannelName = _,
      Sender = ?SendId.

  make_sender(Name, ChannelName, Sender) :-

    string_to_dlist(Name, NL, NC),
    string_to_dlist(ChannelName, CL, []) :
      NC = [46 | CL] |
	list_to_string(NL, Sender).


message_to_channels(Message, Name, ChannelNames, Locals, Underscore,
			MsChannelNames, Errors, NextErrors) + (Index = 1) :-

    Message = [] :
      Name = _,
      ChannelNames = _,
      Locals = _,
      Underscore = _,
      Index = _,
      MsChannelNames = [],
      Errors = NextErrors;

    arg(Index, Message, Channel), string(Channel), Channel =\= "",
    Underscore =\= true, Index++ :
      MsChannelNames ! OkChannelName? |
	piutils#verify_channel(Name, Channel, ChannelNames, Locals,
				OkChannelName, Errors, Errors'),
	self;

    arg(Index, Message, `Channel), string(Channel),
    Underscore =\= true, Index++ :
      MsChannelNames ! OkChannelName? |
	piutils#verify_channel(Name, Channel, ChannelNames, Locals,
				OkChannelName, Errors, Errors'),
	self;

    arg(Index, Message, _),
    Underscore =?= true, Index++ :
      MsChannelNames ! "_" |
	self;

    Index > arity(Message) :
      Message = _,
      Name = _,
      ChannelNames = _,
      Locals = _,
      Underscore = _,
      MsChannelNames = [],
      Errors = NextErrors;

    otherwise,
    arg(Index, Message, Channel),
    Index++ :
      Errors ! (Name - invalid_channel_in_message(Message, Channel)),
      MsChannelNames ! [] |
	self;

    otherwise :
      ChannelNames = _,
      Locals = _,
      Underscore = _,
      Index = _,
      MsChannelNames = [],
      Errors = [(Name - invalid_channel_list(Message)) | NextErrors].


parse_message(Name, ChannelNames, Message, MsChannelNames, Locals, Primes,
			Errors, NextErrors) :-

    Message = [] :
      Name = _,
      ChannelNames = _,
      MsChannelNames = [],
      Locals = [],
      Primes = [],
      Errors = NextErrors;

    tuple(Message) :
      Index = 1 |
	receive_channel_names(Index, Message, Name, Strings, MsChannelNames,
					Errors, Errors'?),
	piutils#check_for_duplicates(Strings, UniqueNames,
		{Name, duplicate_receive_channel}, Errors', NextErrors),
	instantiate_channels;

    otherwise :
      ChannelNames = _,
      MsChannelNames = [],
      Locals = [],
      Primes = [],
      Errors = [(Name - invalid_message(Message)) | NextErrors].


  receive_channel_names(Index, Message, Name, Strings, MsChannelNames,
				Errors, NextErrors) :-

    arg(Index, Message, ChannelName),
    string(ChannelName), ChannelName =\= "", ChannelName =\= "_",
    Index++ :
      Strings ! ChannelName,
      MsChannelNames ! ChannelName |
	self;

    arg(Index, Message, `ChannelName),
    string(ChannelName), ChannelName =\= "", ChannelName =\= "_",
    Index++ :
      Strings ! ChannelName,
      MsChannelNames ! ChannelName |
	self;

    arg(Index, Message, `"_"),
    Index++ :
      MsChannelNames ! "_" |
	self;

    Index > arity(Message) :
      Name = _,
      Strings = [],
      MsChannelNames = [],
      Errors = NextErrors;

    otherwise,
    arg(Index, Message, ChannelName),
    Index++ :
      Errors ! (Name - invalid_receive_channel(ChannelName)) |
	self.

  instantiate_channels(UniqueNames, ChannelNames, Locals, Primes) :-

    UniqueNames ? ChannelName |
	local_or_prime(ChannelName, ChannelNames,
		Locals, Locals'?, Primes, Primes'?),
	self;

    UniqueNames = [] :
      ChannelNames = _,
      Locals = [],
      Primes = [];

    ChannelNames = [] :
      UniqueNames = _,
      Locals = [],
      Primes = [].

  local_or_prime(ChannelName, ChannelNames, 
	Locals, NextLocals, Primes, NextPrimes) :-

    ChannelNames = [ChannelName | _],
    string_to_dlist(ChannelName, CL, Prime) :
      Prime = [39],
      Locals = NextLocals,
      Primes = [{ChannelName, ChannelNamePrime?} | NextPrimes] |
	list_to_string(CL, ChannelNamePrime);

    ChannelNames ? Other, Other =\= ChannelName |
	self;

    ChannelNames = [] :
      Locals = [ChannelName | NextLocals],
      Primes = NextPrimes.

