/*
Precompiler for Pi Calculus procedures - call management.

Bill Silverman, December 1999.

Last update by		$Author: bill $
		       	$Date: 2000/04/16 08:04:00 $
Currently locked by 	$Locker:  $
			$Revision: 2.0 $
			$Source: /spring/users1/Bill/Repository/PiFcp/pifcp/call.cp,v $

Copyright (C) 1999, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-language(compound).
-export([make_local_call/10, make_remote_call/8,
	 prime_local_channels/3, sum_procedures/5]).
      
make_local_call(ProcessDefinition, Locals, Primes, Body1, Body2,
		In, NextIn, Errors, NextErrors, CallDefinition) :-

    Body1 = true :
      ProcessDefinition = _,
      Locals = _,
      Primes = _,
      In = NextIn,
      Errors = NextErrors,
      Body2 = true,
      CallDefinition = [];

    Body1 =?= self :
      Body1' = `Name |
	extract_id(ProcessDefinition, Name, _Arity),
	self;

    arity(Body1) > 1, arg(1, Body1, self) |
	extract_id(ProcessDefinition, Name, _Arity),
	copy_goal_args(Body1, `Name?, Body1'),
	self;

    arity(Body1) > 1, arg(1, Body1, `Functor), string(Functor) |
	extract_id(ProcessDefinition, Name, _Arity),
	extract_lhs_parts(ProcessDefinition, ChannelNames, _OuterLHS, _InnerLHS),
	extract_arguments_or_substitutes(Name, Body1, Arguments, Substitutes,
						Errors, Errors'?),
	verify_call_channels(Name, Body1, ChannelNames, Locals,
				Errors', Errors''?),
	prime_local_channels(Primes, Arguments?, Arguments'),
	substituted_local_channels(Primes, Substitutes?, Substitutes'),
	lookup_call_functor,
	complete_local_call;

    Body1 = `Functor :
      Locals = _,
      Primes = _,
      Arguments = [],
      Substitutes = [] |
	extract_id(ProcessDefinition, Name, _Arity),
	lookup_call_functor,
	complete_local_call;

    Body1 = _ + _ :
      CallDefinition = {Name, 0, [/*fake_channels*/], {{fake_lhs}, {fake_lhs}},
				sum(fake_rhs, fake_communication_rhs)}|
	make_summed_call(ProcessDefinition, Locals, Primes, Body1, Body2, Name,
			 In, NextIn, Errors, NextErrors);

    otherwise,
    tuple(Body1),
    arity(Body1, Arity) :
      ProcessDefinition = _,
      Locals = _,
      Primes = _,
      In = [logix_variables(LogixVars?) | NextIn],
      Errors = NextErrors,
      CallDefinition = [],
      Index = 1 |
	copy_predicates(Body1, Index, Arity, Body2, LogixVars);	

    otherwise :
      Locals = _,
      Primes = _,
      Body2 = true,
      In = NextIn,
      Errors = [(Name? - invalid_local_call(Body1)) | NextErrors],
      CallDefinition = [] |
	extract_id(ProcessDefinition, Name, _Arity).

  copy_predicates(Predicates, Index, Arity, Body, LogixVars) :-

    Index++ < Arity,
    arg(Index, Predicates, Predicate) :
      Body = (Predicate, Body') |
	piutils#find_logix_variables(Predicate, LogixVars, LogixVars'?),
	self;

    Index =:= Arity,
    arg(Index, Predicates, Predicate) :
      Body = Predicate |
	piutils#find_logix_variables(Predicate, LogixVars, []).

  substituted_local_channels(Primes, Substitutes, Primed) :-

    Primes ? {Channel, ChannelP} |
	prime_substitutes(Channel, ChannelP, Substitutes, Substitutes'),
	self;

    Primes =?= [] :
      Primed = Substitutes.

  prime_substitutes(Channel, ChannelP, SubsIn, SubsOut) :-

    SubsIn ? (Substitute = Sub), Sub =?= Channel :
      SubsOut ! (Substitute = ChannelP) |
	self;

    SubsIn ?  (Substitute = Sub), Sub =\= Channel :
      SubsOut !  (Substitute = Sub) |
	self;

    SubsIn =?= [] :
      Channel = _,
      ChannelP = _,
      SubsOut = [].


verify_call_channels(Name, Goal, ChannelNames, Locals, Errors, NextErrors)
			+ (Index = 2) :-

    arg(Index, Goal, (_ = String)), string =\= "_",
    Index++ |
	piutils#verify_channel(Name, String, ChannelNames, Locals, _OkChannel,
				Errors, Errors'?),
	self;

    arg(Index, Goal, String), string =\= "_",
    Index++ |
	piutils#verify_channel(Name, String, ChannelNames, Locals, _OkChannel,
				Errors, Errors'?),
	self;

    Index =< arity(Goal),
    otherwise,
    Index++ |
	self;

    Index > arity(Goal) :
      Name = _,
      ChannelNames = _,
      Locals = _,
      Errors = NextErrors.
    

complete_local_call(CallType, CallDefinition, Arguments, Substitutes, Name,
				Body1, Body2, Errors, NextErrors) :-

    CallType =?= none :
      CallDefinition = _,
      Arguments = _,
      Substitutes = _,
      Name = _,
      Body2 = true,
      Errors = [Name - unknown_local_process(Body1) | NextErrors];

    CallType =\= none,
    list(Arguments) :
      Substitutes = _,
      Name = _,
      Body1 = _ |
	extract_id(CallDefinition, _Name, Arity),
	extract_lhs_parts(CallDefinition, _ChannelNames, Atom, _InnerLHS),
	substitute_arguments+(Index = 1, Substitutes = Substitutes'),
	call_with_substitutes;

    CallType =?= outer,
    Arguments =?= [] :
      Errors = NextErrors,
      Name = _,
      Body1 = _ |
	extract_lhs_parts(CallDefinition, _ChannelNames, Atom, _InnerLHS),
	call_with_substitutes;

    CallType =?= inner,
    Arguments =?= [] :
      Name = _,
      Body1 = _,
      Errors = NextErrors |
	extract_lhs_parts(CallDefinition, _ChannelNames, _OuterLHS, Atom),
	call_with_substitutes.


substitute_arguments(Index, Atom, Arity, Arguments, Substitutes, Name, Body1,
			Errors, NextErrors) :-

    Index++ =< Arity,
    arg(Index', Atom, S),
    Arguments ? C :
      Substitutes ! {S, C} |
	self;

    Index > Arity,
    Arguments = [] :
      Atom = _,
      Name = _,
      Body1 = _,
      Substitutes = [],
      Errors = NextErrors;

    Index > Arity,
    Arguments =\= [] :
      Atom = _,
      Substitutes = [],
      Errors = [Name - too_many_arguments(Body1) | NextErrors];

    Index =< Arity,
    Arguments =?= [] :
      Atom = _,
      Substitutes = [],
      Errors = [Name - not_enough_arguments(Body1) | NextErrors].


call_with_substitutes(Atom, Substitutes, Body2) :-

    Substitutes =?= [],
    arg(1, Atom, Name) :
      Body2 = Name;

    Substitutes =\= [],
    arg(1, Atom, Name) :
      Body2 = Name + Added |
	add_substitutes.

  add_substitutes(Substitutes, Added) :-

    Substitutes =?= [{S, C}] :
      Added = (S = `C);

    Substitutes ? {S, C},
    Substitutes' =\= [] :
      Added = (S = `C, Added') |
	self.


lookup_call_functor(Name, ProcessDefinition, Functor,
		CallType, CallDefinition, In, NextIn) :-

    Functor =?= Name :
      CallType = inner,
      CallDefinition = ProcessDefinition,
      In = NextIn;

    otherwise :
      Name = _,
      ProcessDefinition = _,
      In = [lookup_functor(Functor, CallType, CallDefinition) | NextIn].

extract_arguments_or_substitutes(Name, Tuple, Arguments, Substitutes,
				Errors, NextErrors) + (Index = 2, Type = _) :-

    arg(Index, Tuple, Channel), string(Channel), Channel =\= "_",
    Index++ :
      Type = channel,
      Arguments ! Channel |
	self;

    arg(Index, Tuple, `Channel), string(Channel), Channel =\= "_",
    Index++ :
      Type = channel,
      Arguments ! Channel |
	self;

    arg(Index, Tuple, S = C), string(S), S =\= "_", string(C),
    Index++ :
      Type = substitute,
      Substitutes ! {`S, C} |
	self;

    arg(Index, Tuple, S = `C), string(S), S =\= "_", string(C),
    Index++ :
      Type = substitute,
      Substitutes ! {`S, C} |
	self;

    arg(Index, Tuple, `S = C), string(S), S =\= "_", string(C),
    Index++ :
      Type = substitute,
      Substitutes ! {`S, C} |
	self;

    arg(Index, Tuple, `S = `C), string(S), S =\= "_", string(C),
    Index++ :
      Type = substitute,
      Substitutes ! {`S, C} |
	self;

    arity(Tuple) < Index :
      Name = _,
      Type = _,
      Arguments = [],
      Substitutes = [],
      Errors = NextErrors;

    otherwise,
    Index =:= 2 :
      Type = _,
      Arguments = [],
      Substitutes = [],
      Errors = [Name - first_argument_invalid(Tuple) | NextErrors];

    otherwise,
    Type = channel,
    arg(Index, Tuple, Arg),
    Index++ :
      Arguments ! "_",
      Errors ! (Name - invalid_channel_argument(Arg, Tuple)) |
	self;

    otherwise,
    Type = substitute,
    arg(Index, Tuple, Arg),
    Index++ :
      Errors ! (Name - invalid_substitute(Arg, Tuple)) |
	self.


make_remote_call(Name, ChannelNames, Locals, Primes, PiCall, CompoundCall,
				Errors, NextErrors) :-

	extract_arguments_or_substitutes(Name, PiCall, Arguments, _Subs,
			Errors, Errors'?, 2, channel),
	verify_call_channels(Name, PiCall, ChannelNames, Locals,
				Errors', Errors''?),
	prime_local_channels(Primes, Arguments?, Arguments'),
	complete_remote_call(Name, PiCall, Arguments'?, CompoundCall,
				Errors'', NextErrors).


complete_remote_call(Name, PiCall, Arguments, CompoundCall,
			Errors, NextErrors) :-

    arg(1, PiCall, `Functor), string(Functor), Arguments =\= [] :
      Name = _,
      Errors = NextErrors |
	piutils#make_lhs_tuple(Functor, Arguments, CompoundCall);

    arg(1, PiCall, `Functor), string(Functor), Arguments =?= [] :
      Name = _,
      CompoundCall = Functor,
      Errors = NextErrors;

    otherwise :
      Arguments = _,
      CompoundCall = [],
      Errors = [Name - invalid_remote_call(PiCall) | NextErrors].


prime_local_channels(Primes, Arguments, Primed) :-

    Primes ? {Channel, ChannelP} |
	prime_arguments(Channel, ChannelP, Arguments, Arguments'),
	self;

    Primes =?= [] :
      Primed = Arguments.

 prime_arguments(Channel, ChannelP, ArgsIn, ArgsOut) :-

    ArgsIn ? Arg, Arg =?= Channel :
      ArgsOut ! ChannelP |
	self;

    ArgsIn ? Arg, Arg =\= Channel :
      ArgsOut ! Arg |
	self;

    ArgsIn =?= [] :
      Channel = _,
      ChannelP = _,
      ArgsOut = [].

/***************** Summation Process server predicates. **********************/

make_summed_call(ProcessDefinition, Locals, Primes, Sum, Call, Name,
			In, NextIn, Errors, NextErrors) :-

    true :
      In ! call_sum(Name?, Procedures?, Call) |
	utils#binary_sort_merge(Names?, NameList),
	concatenated_sum_name(NameList?, Name),
	summed_call(ProcessDefinition, Locals, Primes, Sum, Names, Procedures,
			In', NextIn, Errors, NextErrors).

  concatenated_sum_name(NameList, Name) + (NLH = NLT?, NLT) :-


    NameList ? N :
      ascii("+", Plus),
      NLT' ! Plus |
	string_to_dlist(N, NLT, NLT'),
	self;

    NameList =?= [N],
    string_to_dlist(N, NH, []) :
      NLT = NH |
	list_to_string(NLH, Name);

    NameList = [] :
      NLH = _,
      NLT = _,
      Name = false.


summed_call(ProcessDefinition, Locals, Primes, Sum, Names, Procedures,
		In, NextIn, Errors, NextErrors) :-

    Sum = `Name + Sum', string(Name) :
      Names ! Name,
      Procedures ! {Call?, CallDefinition?} |
	make_local_call(ProcessDefinition, Locals, Primes, `Name, Call,
			In, In', Errors, Errors'?, CallDefinition),
	self;

    Sum = Sum' + `Name, string(Name) :
      Names ! Name,
      Procedures ! {Call?, CallDefinition?} |
	make_local_call(ProcessDefinition, Locals, Primes, `Name, Call,
			In, In', Errors, Errors'?, CallDefinition),
	self;

    Sum = `Name, string(Name) :
      Names = [Name],
      Procedures = [{Call?, CallDefinition?}] |
	make_local_call(ProcessDefinition, Locals, Primes, `Name, Call,
			In, NextIn, Errors, NextErrors, CallDefinition);

    Sum = Tuple + Sum', arg(1,Tuple,`Name), string(Name) :
      Names ! Name,
      Procedures ! {Call?, CallDefinition?} |
	make_local_call(ProcessDefinition, Locals, Primes, Tuple, Call,
			In, In', Errors, Errors'?, CallDefinition),
	self;

    Sum = Sum' + Tuple, arg(1,Tuple,`Name), string(Name) :
      Names ! Name,
      Procedures ! {Call?, CallDefinition?} |
	make_local_call(ProcessDefinition, Locals, Primes, Tuple, Call,
				In, In', Errors, Errors'?, CallDefinition),
	self;

    arg(1,Sum,`Name), string(Name) :
      Names = [Name],
      Procedures = [{Call?, CallDefinition?}] |
	make_local_call(ProcessDefinition, Locals, Primes, Sum, Call,
			In, NextIn, Errors, NextErrors, CallDefinition);
    otherwise :
      Locals = _,
      Primes = _,
      Errors = [Name - illegal_process_summation(Sum) | NextErrors],
      Names = [],
      Procedures = false,
      In = NextIn |
	extract_id(ProcessDefinition, Name, _Arity).

/****************** Summation  Empty server predicates. **********************/

sum_procedures(Summed, Entries, Optimize, NextOptimize, Errors) +
			(Cumulated = []) :-

    Summed ? Name(Procedures, Call) |
	cumulate,
	extract_procedure_parts(Procedures,
		Names, Calls, Channels, CodeTuples),
	optimize_sum(Name, Names, Optimize, Optimize'?),
	cumulated;

    Summed = [] :
      Cumulated = _,
      Entries = [],
      Optimize = NextOptimize,
      Errors = [].

  extract_procedure_parts(Procedures, Names, Calls, Channels, CodeTuples) :-

    Procedures ? {Call, ProcedureDefinition},
    ProcedureDefinition =?= {Name, _Arity, ChannelNames, _LHS, CodeTuple} :
      Names ! Name,
      Calls ! Call,
      Channels ! ChannelNames,
      CodeTuples ! CodeTuple |
	self;

    Procedures ? _Ignore,
    otherwise |
	self;

    Procedures = [] :
      Names = [],
      Calls = [],
      Channels = [],
      CodeTuples = [].

  cumulate(Name, Cumulated, Reply) :-

    Cumulated = [Name | _] :
      Reply = found;

    Cumulated ? Other,
    Other =\= Name |
	self;

    Cumulated = [] :
      Name = _,
      Reply = new.

  cumulated(Summed, Entries, Optimize, NextOptimize, Errors, Cumulated,
	Name, Calls, Channels, CodeTuples, Call, Reply) :-

    Reply =?= found :
      Channels = _,
      CodeTuples = _ |
	make_sum_call(Name, Calls, Call, Errors, Errors'?),
	sum_procedures;

    Reply =?= new :
      Cumulated' = [Name | Cumulated] |
	make_summed_rhs(Name, Calls, CodeTuples, 1, Prepares, Code,
			Streams, FinalMode, Errors, Errors'?),
	piutils#sort_out_duplicates(Channels?, SumChannels, _Reply),
	make_named_list(Prepares?, Writes, Name-duplicate_send_channel_in_sum,
				Errors', Errors''?),
	make_named_guard(Writes?, Ask, Tell),
	make_named_list(Code?, Code', Name-duplicate_receive_channel_in_sum,
				Errors'', Errors'''?),
	/* Eliminate duplicate stream names. */
	utils#binary_sort_merge(Streams, Streams'),
	make_named_predicates(';', Code', RHS),
	piutils#make_lhs_tuple(Name, SumChannels, Tuple),
	make_sum_procedure(FinalMode?, Name, (Ask? : Tell?), RHS?, Tuple?,
				Streams'?, Entries, Entries'?),
	make_sum_call(Name, Calls, Call, Errors''', Errors''''?),
	sum_procedures.


optimize_sum(Name, Names, Optimize, NextOptimize) :-

    Names =?= [] :
      Name = _,
      Optimize = NextOptimize;

    Names =\= [] :
      Optimize ! procedure(Notes?, 0, {Name?}, _Value) |
% screen#display(sum(Name, Value)),
	add_calls_and_channels.

  add_calls_and_channels(Names, Optimize, NextOptimize, Notes) :-

    Names ? Name :
      Optimize ! procedure([], 0, {Name}, {Calls, Channels}),
      Notes ! call(Calls),
      Notes' ! variables(Channels) |
% screen#display(add(Name, Calls, Channels)),
	self;

    Names = [] :
      Optimize = NextOptimize,
      Notes = [].


make_sum_call(Name, Calls, Call, Errors, NextErrors)
		+ (NamedArgs = AddArgs?, AddArgs) :-

    Calls ? String, string(String) |
	self;

    Calls ? (_Name + Arguments) |
	extract_named_arguments(Arguments, AddArgs, AddArgs'?),
	self;

    Calls = [] :
      AddArgs = [] |
	make_named_list(NamedArgs, ArgList,
		Name-duplicate_channel_substitution, Errors, NextErrors),
	complete_sum_call.

  complete_sum_call(Name, ArgList, Call) :-

    ArgList =?= [] :
      Call = Name;

    ArgList =\= [] :
      Call = Name+(Substitutes) |
	make_named_predicates(',', ArgList, Substitutes).


make_summed_rhs(Name, Calls, CodeTuples, Index, Prepares, Code, Streams,
			FinalMode, Errors, NextErrors) +
		(Mode = none, Sender = _) :-

    CodeTuples ? ProcessMode(SendRHS, ProcessRHS),
    Calls ? _,
    SendRHS =?= (Idents : Tells | _Relay) |
	piutils#untuple_predicate_list(",", Idents, Asks),
	piutils#untuple_predicate_list(",", Tells, Tells'),
	piutils#untuple_predicate_list(";", ProcessRHS, ClauseList),
	add_sends_and_receives(Asks?, Tells'?, ClauseList?, Sender?,
				Index, Index', Prepares, Prepares'?,
				Code, Code'?, Streams, Streams'?),
	piutils#update_process_mode(Mode, ProcessMode, Mode'),
	self;

    CodeTuples ? ProcessMode([], []),
    Calls ? Call :
      Errors ! (Name-invalid_mode_in_summation(Call? - ProcessMode)) |
	piutils#update_process_mode(Mode, ProcessMode, Mode'),
	self;

    CodeTuples = [] :
      Calls = _,
      Index = _,
      Prepares = [],
      Code = [],
      Streams = [],
      FinalMode = Mode,
      Errors = NextErrors |
	final_process_mode(Name, Mode, Sender).

  final_process_mode(Name, Mode, Sender) :-

    Mode =?= mixed :
      Name = _,
      Sender = `pifcp(sendid);

    Mode =?= send :
      Sender = Name;

    /* receive or none */
    otherwise :
      Name = _,
      Mode = _,
      Sender = [].

  add_sends_and_receives(Asks, Tells, ClauseList, Sender,
			Index, NewIndex, Prepares, NextPrepares,
			Code, NextCode, Streams, NextStreams) :-
    Asks ? Identify,
    Tells ? true,
    Asks' ? Read,
    Read =?= read_vector(2, _Vector, `StreamName):
      Prepares ! StreamName((Identify, Read), true) |
	self;

    Asks ? Identify,
    Identify = (`ChannelName = _Tuple),
    Tells ? Write,
    Write =?= write_channel(_,_),
    ClauseList ? Sent,
    Sent =?= (`pifcp(chosen) = _Index | Body),
    Index++ :
      Prepares ! ChannelName(Identify, Write'?),
      Code ! Index((`pifcp(chosen) = Index | Body)) |
	reindex_write(Write, Sender, Index, Write'),
	self;

    Asks ? Identify,
    Identify = (`ChannelName = _Tuple),
    Tells ? Write,
    Write =?= write_channel(_,_),
    ClauseList ? Sent,
    /* Belt and Suspenders */
    Sent =?= (`pifcp(chosen) = _Index : Tell | Body),
    Index++ :
      Prepares ! ChannelName(Identify, Write'?),
      Code ! Index((`pifcp(chosen) = Index : Tell | Body)) |
	reindex_write(Write, Sender, Index, Write'),
	self;

    ClauseList = [] :
      Sender = _,
      Asks = _,
      Tells = _,
      NewIndex = Index,
      NextPrepares = Prepares,
      Code = NextCode,
      Streams = NextStreams;

    /* Ignore cdr's - residual code. */
    otherwise,
    ClauseList ? Cdr,
    Cdr =?= (_ : _ | self) |
	self;

    otherwise,
    ClauseList ? Cdr,
    Cdr =?= (CdrStream, _ | self),
    CdrStream = (`StreamName ? _) :
      Streams ! `StreamName |
        /* Anti-Kluge to reclaim the channel name. */
	string_to_dlist(StreamName, SL, []),
	string_to_dlist("_pistr_", SL, NL),
	list_to_string(NL, ChannelName),
	analyze_receive(ClauseList', Sender, ChannelName, Cdr,
			Code, Code', ClauseList''),
	self.

  reindex_write(Write, Sender, Index, NewWrite) :-

    Write = write_channel({_Sender, ChannelList, _SendIndex, Chosen}, VN) :
      NewWrite = write_channel({Sender, ChannelList, Index, Chosen}, VN).


analyze_receive(ClauseList, Sender, ChannelName, Cdr, 
		Code, NextCode, NextClauseList) :-

    ClauseList = [Consume],
    Sender =?= [] :
      Code = [ChannelName(Cdr, Consume) | NextCode],
      NextClauseList = [];

    ClauseList ? Consume, Consume =\= (_ | self),
    Sender =?= [] :
      Code = [ChannelName(Cdr, Consume) | NextCode],
      NextClauseList = ClauseList';

    /* The summation is mixed. */
    ClauseList =?= [Consumer],
    Sender =\= [] :
      Code = [ChannelName(Cdr, Iterate?, Consume?) | NextCode],
      NextClauseList = [] |
	invent_iterate(ChannelName, Consumer, Iterate, Consume);

    ClauseList ? Consume, ClauseList' =\= [], Consume =\= (_ | self),
    Sender =\= [] :
      Code = [ChannelName(Cdr, Iterate?, Consume'?) | NextCode],
      NextClauseList = ClauseList'|
	invent_iterate(ChannelName, Consume, Iterate, Consume');

    /* This Process was already mixed. */
    ClauseList ? Iterate, 
    Iterate =?= (_ | self),
    ClauseList' ? Consume |
      Sender = _,
      Code = [ChannelName(Cdr, Iterate, Consume) | NextCode],
      NextClauseList = ClauseList''.

  invent_iterate(ChannelName, Consume, Iterate, NewConsume) :-

    Consume = (	_Mss ? {_Sender, ChannelList, _Tag, _Choose},
		_We : _Tell | Body ) :
      NewConsume = (Consume'? | Body) |
	servers#make_guard_receive(ChannelName, ChannelList, pifcp(sendid),
					{_Cdr, Iterate}, Consume').


/* Compare to make_RHS2 */
make_sum_procedure(Mode, Name, Writes, RHS, Tuple, Streams,
			Entries, NextEntries) :-

    Mode =?= receive :
      Entries = [Mode(Atom?, (Writes | SendChoices?), (ChoiceAtom? :- RHS))
		| NextEntries] |
	piutils#tuple_to_atom(Tuple, Atom),
	make_choice_name(Name, ".receive", SendChoices),
	make_choice_atom(Atom, SendChoices?, Streams, ChoiceAtom);

    Mode =?= send :
      Entries = [Mode(Atom?, (Writes | SendChoices?), (ChoiceAtom? :- RHS))
		| NextEntries] |
	piutils#tuple_to_atom(Tuple, Atom),
	make_choice_name(Name, ".send", SendChoices),
	make_choice_atom(Atom, SendChoices?, [`pifcp(chosen) | Streams],
				ChoiceAtom);

    Mode =?= mixed :
      Sender = `pifcp(sendid),
      Entries = [Mode(Atom?, (Writes? |
				pi_monitor#unique_sender(Name, Sender),
				MixedChoices?),
		 (ChoiceAtom? :- RHS?))
		| NextEntries] |
	piutils#tuple_to_atom(Tuple, Atom),
	make_choice_name(Name, ".mixed", MixedChoices),
	make_choice_atom(Atom, MixedChoices, [`pifcp(chosen), Sender |Streams],
				ChoiceAtom);

    /* conflict */
    otherwise :
      Name = _,
      Writes = _,
      Streams = _,
      Entries = [Mode(Atom?, RHS, []) | NextEntries] |
	piutils#tuple_to_atom(Tuple, Atom).

  make_choice_name(Prefix, Suffix, Name) :-
    string_to_dlist(Prefix, PL, PS),
    string_to_dlist(Suffix, SL, []) :
      PS = SL |
	list_to_string(PL, Name).

  make_choice_atom(InnerLHS, Name, ChoiceVars, ChoiceAtom) :-
	utils#tuple_to_dlist(InnerLHS, [_ | ChannelVariables], ChoiceVars),
	utils#list_to_tuple([Name | ChannelVariables], ChoiceAtom).


extract_id(Definition, Name, Arity) :-
    true :
      Definition = {Name, Arity, _ChannelNames, _LHS, _CodeTuple}.

extract_lhs_parts(Definition, ChannelNames, OuterLHS, InnerLHS) :-
    true :
      Definition = {_Name, _Arity, ChannelNames, LHS, _CodeTuple},
      LHS = {OuterLHS, InnerLHS}.

/************************** Summation Utilities ******************************/

make_named_guard(Writes, Ask, Tell) :-

    Writes ? _Name(Idents, Write), Writes' =\= [] :
      Ask = (Idents, Ask'?),
      Tell = (Write, Tell'?) |
	self;

    Writes = [_Name(Idents, Write)] :
      Ask = Idents,
      Tell = Write;

    Writes = [] :
      Ask = true,
      Tell = true.

make_named_list(NamedClauses, Clauses, Diagnostic, Errors, NextErrors) :-

	piutils#sort_out_duplicates([NamedClauses], Clauses, Reply),
	diagnose_duplicate.

  diagnose_duplicate(Reply, Diagnostic, Errors, NextErrors) :-

    Reply ? Duplicate,
    Diagnostic = Name - String :
      Errors ! (Name - String(Duplicate)) |
	self;

    Reply =?= [] :
      Diagnostic = _,
      Errors = NextErrors;

    otherwise :
      Reply = _,
      Errors = [Diagnostic | NextErrors].

make_named_predicates(Operator, List, PredicateList) :-

    List =?= [] :
      Operator = _,
      PredicateList = true;

    List ? Name(Predicate1, Predicate2, Predicate3) :
      PredicateList = {Operator, Predicate1,
				{Operator, Predicate2, PredicateList'}},
      List'' = [Name(Predicate3) | List'] |
	self;
    List ? Name(Predicate1, Predicate2) :
      PredicateList = {Operator, Predicate1, PredicateList'},
      List'' = [Name(Predicate2) | List'] |
	self;

    List ? _Name(Predicate),
    List' =\= [] :
      PredicateList = {Operator, Predicate, PredicateList'?} |
	self;

    List =?= [_Name(Predicate)] :
      Operator = _,
      PredicateList = Predicate.

extract_named_arguments(Arguments, Args, NextArgs) :-

    Arguments = (Substitution, Arguments'),
    Substitution =?= (`Name = _Value) :
      Args ! Name(Substitution) |
	self;

    Arguments =\= (_ , _),
    Arguments =?= (`Name = _Value) :
      Args = [Name(Arguments) | NextArgs].
