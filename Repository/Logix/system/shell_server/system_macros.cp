/*

Shell macros
Ehud Shapiro, 01-09-86

Last update by          $Author: bill $
                        $Date: 1994/04/04 11:56:55 $
Currently locked by     $Locker:  $
                        $Revision: 1.2 $
                        $Source: /baz/users/cvs-root/Source/system/shell_server/system_macros.cp,v $

Copyright (C) 1985, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-language(compound).
-export([expand/3]).
-mode(trust).

procedure expand(Command, Out_Head, Out_Tail).

expand(Command, Xs, Ys) :-
  Command = (X = Y) :
    Xs = Ys |
        unify(X,Y);

  Command = '^' :
     Xs = [display_stream(Bindings,[]),
           computation # dictionary(bindings, Bindings, 0) | Ys];

  Command = unbind :
     Xs = [computation # dictionary(unbind) | Ys];

  Command = {Hat, X},
  Hat     = '^' |
        display_variables(X, Xs, Ys);

  Command = (X := Y) :
     Xs = Ys |
        utils # evaluate(Y,X);

  Command = (Ms!M) :
     Xs = Ys |
        add_to_stream(M,Ms);

  Command = (Ms!) :
     Xs = Ys |
        close_stream(Ms);

  Command = info :
     Xs ! display_stream(Info, [type(ground), prefix(info)]),
     Command' = info(Info) |
        expand;

  Command = info(Info),
  info(1,N1),
  info(2,N2),
  info(3,N3),
  info(4,N4),
  info(5,N5),
  info(6,N6),
  info(7,N7),
  info(8,N8),
  info(9,N9) :
     Xs = Ys |
        unify_without_failure(
                        Info,
                        [cpu = N1,free_heap = N2,used_heap = N3,
                         creations = N4,terminations = N7,reductions = N5,
                         suspensions = N6,activations = N8,collections = N9
                        ]);

  Command = date :
     Xs  = Ys |
        processor # interface(date_time(_, Date, Time)),
        computation # display(term, Date-Time, known(Time));

  Command = status :
     Xs  = [computation(N,N),service(M,M),status(Status) | Ys] |
        computation_id(N,Id),
        status(Status,M,N,Id);

  Command = reset :
     Xs  = [computation(0,_),reset | Ys] ;

  Command = path :
     Xs = [to_context(service_id(SId)) | Ys] |
	service_id_path(SId,Path),
        computation # display(term, Path,type(ground));

  Command = (_,_) |
        identify({start, Command, _,_,_,_}, 1, start(Command),Xs,Ys);

  otherwise |
        expand_arg(Command,Xs,Ys).

expand_arg(Arg,Xs,Ys) :-
  string(Arg) |
        identify({Arg,_,_,_,_,_},0,Arg,Xs,Ys);

  Arg = {X} |
        expand(X,Xs,Ys);

  Arg = {F, A} |
        identify({F,A,_,_,_,_},1,Arg,Xs,Ys);

  Arg = {F, A, B} |
        identify({F,A,B,_,_,_},2,Arg,Xs,Ys);

  Arg = {F, A, B, C} |
        identify({F,A,B,C,_,_},3,Arg,Xs,Ys);

  Arg = {F, A, B, C, D} |
        identify({F,A,B,C,D,_},4,Arg,Xs,Ys);

  tuple(Arg),
  Args := arity(Arg) - 1, Args > 4,
  arg(1,Arg,F),
  arg(2,Arg,A), arg(3,Arg,B), arg(4,Arg,C), arg(5,Arg,D), arg(6,Arg,E) |
        identify({F,A,B,C,D,E},Args,Arg,Xs,Ys);

  otherwise :
     Xs = [Arg | Ys].

identify(Cmd, Args, CD, Xs, Ys) :-
  Cmd = services(Path,Sorted,_,_,_),
  Args < 3 :
     CD = _ |
        services_path(Args,Path,Path'),
        computation_utils #
             call_list([Path' # service_id(Scope)
                        | known(Scope,
                               ['_hierarchy'(services(Scope,Services))])
                        ],
                                  SystemReply
             ),
        services_reply(SystemReply, Services),
        sort_services(Services, Prefix, Sorted),
        display_services(Args, Prefix, Sorted, Xs,Ys);

  Cmd = cd(Path, _Ok, _, _, _),
  0 < Args, Args < 3 |
        change_director(Path, CD, Xs, Ys);

  Cmd = input(Path, _Ok,_,_,_),
  0 < Args, Args < 3 :
     Xs = Ys |
        computation_utils # call_id_goal(self # Path, Id, Name, Reply),
        expand_input(Reply, Id, Name, Ok),
        input_status(Ok, CD, Path);

  Cmd = close(M,Ok?,_,_,_),
  Args < 3 :
     CD = _ |
        expand_list_op(close,M,Ok,Args,Xs,Ys);

  Cmd = load(M,Ok?,_,_,_),
  Args < 3 :
     CD = _ |
        expand_list_op(load,M,Ok,Args,Xs,Ys);

  Cmd = edit(M,Ok?,_,_,_),
  Args < 3 :
     CD = _ |
        signal_fcp(suspend,Xs,Xs',_),
        expand_list_op(edit,M,Ok,Args,Xs',Ys);

  Cmd = compile(M,Options,Output,_,_),
  Args < 4 : CD = _ |
        unify_without_failure([], Options),
	expand_compile(compile, M, Options, Output, Args, Xs, Ys);

  Cmd = Encoder(M,Options',Output,_,_),
  Encoder = cdg,
  Args < 4 : CD = _,
     Options ! compiler(Encoder) |
        unify_without_failure([], Options'),
	expand_compile(cdg, M, Options, Output, Args, Xs, Ys);

  Cmd = Encoder(M,Options',Output,_,_),
  Encoder = ndg,
  Args < 4 : CD = _,
     Options ! compiler(Encoder) |
        unify_without_failure([], Options'),
	expand_compile(ndg, M, Options, Output, Args, Xs, Ys);

  Cmd = Encoder(M,Options',Output,_,_),
  Encoder = nccdg,
  Args < 4 : CD = _,
     Options ! compiler(Encoder) |
        unify_without_failure([], Options'),
	expand_compile(nccdg, M, Options, Output, Args, Xs, Ys);

  Cmd = Encoder(M,Options',Output,_,_),
  Encoder = wam,
  Args < 4 : CD = _,
     Options ! compiler(Encoder) |
        unify_without_failure([], Options'),
	expand_compile(wam, M, Options, Output, Args, Xs, Ys);

  Cmd = lint(M,_Options,Output,_,_),
  Args < 4 : CD = _,
    Output = [] |
        expand_service(lint, M, Output, Args, Xs, Ys, SuperId, Name,
			[lint#lint(context(SuperId,Name))], _);

  Cmd = type_check(M,_Options,Output,_,_),
  Args < 4 : CD = _ |
        expand_service(type_check, M, Output, Args, Xs, Ys, SuperId, Name, 
			[type_check#module(SuperId # Name, Output)], _);

  Cmd = abort(No,_Ok,_,_,_),
  Args < 3 |
        send_request(No,N,abort(N,Ok?),request(abort,N?,Ok),CD,Xs,Ys);

  Cmd = resume(No,_Ok,_,_,_),
  Args < 3 |
        send_request(No,N,resume(N,Ok?),request(resume,N?,Ok),CD,Xs,Ys);

  Cmd = suspend(No,_Ok,_,_,_),
  Args < 3 |
        send_request(No,N,suspend(N,Ok?),request(suspend,N?,Ok),CD,Xs,Ys);

  Cmd = verbose(No,Old?,_Ok,_,_),
  Args < 4 |
        send_request(No,N,verbose(N,Old?,Ok?),
			request(report(verbose,Old),N?,Ok),CD,Xs,Ys
	);

  Cmd = quiet(No,Old?,_Ok,_,_),
  Args < 4 |
        send_request(No,N,quiet(N,Old?,Ok?),
			request(report(quiet,Old),N?,Ok),CD,Xs,Ys
	);

  Cmd = diagnostic(No,Old?,_Ok,_,_),
  Args < 4 |
        send_request(No,N,diagnostic(N,Old?,Ok?),
			request(report(diagnostic,Old),N?,Ok),CD,Xs,Ys
	);

  Cmd = silent(No,Old?,_Ok,_,_),
  Args < 4 |
        send_request(No,N,silent(N,Old?,Ok?),
			request(report(silent,Old),N?,Ok),CD,Xs,Ys
	);

  Cmd = '#'(P,_,_,_,_),
  Args = 1 |
        expand_start(ok,{_,S,_,N,[]},start(S? # P,N,_,_),CD,Xs,Ys);

  Cmd = '#'(M,P,_,_,_),
  Args = 2 |
        expand_start(ok,{M,S,_,N,[]},start(S? # P,N,_,_),CD,Xs,Ys);

  Cmd = '@'(G,L,_,_,_),
  Args = 2 |
        parse_rpc(G@L,M,P,Reply),
        expand_start(Reply,{M,S,_,N,[]},start(S? # P,N,_,_),CD,Xs,Ys);

  Cmd = start(Conjunct,No,Events,_Ok,_),
  0 < Args, Args < 5 |
        conjunction(Conjunct, First, Ys',Ys),
        parse_rpc(First,M,P,Reply),
        expand_start(   Reply,{M,S,No,N,[]},
                        start(S? # P?,N,Events,_),CD,Xs,Ys');

  Cmd = time(RPC,Timing,No,Events,Ok),
  0 < Args |
        parse_rpc(RPC,M,P,Reply),
        meta_start(     Reply, {M,S,No,N,timing(Timing)},
                        {P?,Process,
                         timer, time(S? # Process?, Timing), N, Events, Ok
                        },
                        CD,Xs,Ys
        );

  Cmd = rpc(RPC,Timing,No,Events,Ok),
  0 < Args |
        parse_rpc(RPC,M,P,Reply),
        meta_start(     Reply, {M,S,No,N,rpc(Timing)},
                        {P?,Process,
                         timer,rpc(S? # Process?, Timing), N, Events, Ok
                        },
                        CD,Xs,Ys
        );

  Cmd = profile(RPC,Profile,No,Events,Ok),
  0 < Args |
        parse_rpc(RPC,M,P,Reply),
        meta_start(     Reply, {M,S,No,N,profile(Profile)},
                        {P?,Process,
                         profile, profile(S?, Process?, Profile), N, Events, Ok
                        },
                        CD,Xs,Ys
        );

  Cmd = debug(RPC,No,Events,Ok,_),
  0 < Args, Args < 5, tuple(RPC) |
        parse_rpc(RPC,M,P,Reply),
        conditional_id(Reply,N,Id),
        meta_start(     Reply, {M,S,No,N,[]},
                        {P?, Process,
                         debug, interpret(Id?, S? # Process?), N, Events, Ok
                        },
                        CD,Xs,Ys
        );

  Cmd = debug(PNo,No,_Ok,_,_),
  0 < Args, Args < 4, PNo > 0 :
     Xs  = [Computation?,extract(PNo,N?,Request,Ok)|Xs'] |
        define_and_protect(No,N,N1,computation(N,N1),Computation),
        interpret_reply(Ok,debug,Request,N,Xs',Ys),
        display_ok(CD,{_,_,_,Ok},N);

  Cmd = state(No,Goal,Events,_Ok,_),
  Args < 5 :
     Xs = [Computation?,state(N?,Goal,Events,Ok)|Ys] |
        define_and_protect(No,N,N1,computation(N,N1),Computation),
        display_value(Ok,CD,2,No,goal,Goal),
        display_value(Ok,CD,3,No,events,Events),
        display_ok(CD,{_,_,_,_,Ok},N);

  Cmd = events(No,Events,_Ok,_,_),
  Args < 4 :
     Xs = [Computation?,events(N?,Events,Ok) |Ys] |
        define_and_protect(No,N,N1,computation(N,N1),Computation),
        display_value(Ok,CD,2,No,events,Events),
        display_ok(CD,{_,_,_,Ok},N);

  Cmd = resolvent(No,Resolvent,_Ok,_,_),
  Args < 4 :
     Xs = [Computation?,resolvent(N?,Resolvent,Ok)|Ys] |
        define_and_protect(No,N,N1,computation(N,N1),Computation),
        display_resolvent(CD,Resolvent,N),
        display_ok(CD,{_,_,_,Ok},N);

  Cmd = extract(PNo,No,RPC,_Ok,_),
  0 < Args, Args < 5 :
     Xs = [Computation?,extract(PNo,N?,RPC,Ok)|Ys] |
        define_and_protect(No,N,N1,computation(N,N1),Computation),
        display_value(Ok,CD,3,N,extracted,RPC),
        display_ok(CD,{_,_,_,_,Ok},N);

  Cmd = add(Conjunct,No,Ok,_,_),
  0 < Args, Args < 4 |
        conjunction(Conjunct, First, Ys',Ys),
        parse_rpc(First, M, P, Reply),
        expand_add(Reply,CD,M,P,No,Ok,Xs,Ys');

  Cmd = computation(New,Old,_,_,_),
  Args < 3 :
     CD = _ |
        expand_current(Args,computation,New,Old,Xs,Ys);

  Cmd = service(New,Old,_,_,_),
  Args < 3 :
     CD = _ |
        expand_current(Args,service,New,Old,Xs,Ys);

  Cmd = signal_fcp(Signal,_Ok,_,_,_),
  0 < Args, Args < 3 |
        display_ok(CD,{_,_,Ok},'*'),
        signal_fcp(Signal,Xs,Ys,Ok);

  otherwise :
     Cmd = _, Args = _,
     Xs = [CD | Ys].


conjunction(Conjunct, First, Xs,Zs) :-
  Conjunct = (A, B) |
        conjunction(A, First, Xs,Ys),
        identify({add, B, _,_,_,_}, 1, add(B),Ys,Zs);

  Conjunct =\= (_,_) :
     Xs    = Zs,
     First = Conjunct.


services_path(Arg,PathIn,PathOut) :-
  Arg = 0 :
     PathIn  = _,
     PathOut = self;

  otherwise :
     Arg     = _,
     PathOut = PathIn.


services_reply(Reply,Services) :-
  Reply = true :
     Services = _;

  otherwise :
     Services = Reply.


sort_services(Services, Type, Sorted) :-
  list(Services) :
     Type  = service |                  
        utils # binary_sort_merge(Services, Sorted);

  Services =[] :
     Type = no_active_services,
     Sorted = [];

  otherwise :
     Type = '<*>',
     Sorted = Services.



change_director(Path, CD, Xs, Ys) :-
  true :
     Xs ! to_context(computation # call([Path # service_id(Id)], Events)) |
        Events? = [Event | _],
        terminated(Event, Id, Reply),
        found_context(Reply, Id, Ok', Xs',Ys),
        display_ok(CD, {_,_,Ok'}, '*').


terminated(Event, Id, Reply) :-
  Event = terminated :
     Id = _,
     Reply = true;

  Event = failed(_, Reason) :
     Id = [],
     Reply = false(Reason);

  otherwise :
     Id = [],
     Reply = false(Event).


found_context(Check, Id, Ok, Xs,Ys) :-
  Check = true,
  Id =\= [], Id =\= [_] |
        computation # '_hierarchy'(locate(Id, UID)),
        director_context(UID, Id, Ok, Xs,Ys);

  Check = true,
  Id    = [system] :
     Ok = true,
     Xs = ['_set_scope'([system], _, _) | Ys];

  otherwise,
  Check = true :
        Ok = false(cant_change_to(Id)),
        Xs = Ys ;

  Check =\= true :
     Id = _,
     Ok = Check,
     Xs = Ys.

director_context(UID,  Id, Resp, Xs ,Ys) :-
    string(UID) :
        Resp = true,
        Xs   = ['_set_scope'(Id, _, _) | Ys];

    otherwise :
        Id = _,
        Resp = UID,
        Xs   = Ys.

expand_input(Check,Id,Name,Ok) :-
  Check = true |
        file # execute_in_context(Id,get_file(Name,String,[],Ok)),
        expand_input(Ok,String);

  Check = false(Reason) :
     Id = _, Name = _,
     Ok = Reason.

expand_input(Ok, String) :-
  Ok = true |
        computation # input(string(String));

  otherwise :
     Ok = _, String = _.

input_status(Ok, CD, Path) :-
  arity(CD) =:= 2,
  Ok = true :
     Path = _ ;

  known(Ok) : Path = _,
     CD = {_, _, Ok} ;

  otherwise :
     CD = _ |
        computation # error(shell, input(Path), Ok).


parse_rpc(RPC,Mod,Pgm,Ok) :-
  RPC = M#P :
     Mod = M, Pgm = P, Ok = ok;

  RPC = #P :
     Mod = _, Pgm = P, Ok = ok;

  RPC = (M#P)@L :
     Mod = M, Pgm = P@L, Ok = ok;

  RPC = (#P)@L :
     Mod = _M, Pgm = P@L, Ok = ok;

  RPC = A@B@L :
     Pgm = Q@L |
        parse_rpc(A@B,Mod,Q,Ok);

  RPC = P@L,
  P =\= _#_, P =\= _@_, P =\= #_ :
     Mod = _,
     Pgm = P@L,
     Ok  = ok;

  otherwise :
     Mod = _,
     Pgm = RPC,
     Ok  = ng.



expand_list_op(OpCode, M, Ok, Args, Xs,Ys) :-
  true :
     Xs = [to_context(service_id(SId)) | Xs'] |
        expand_list_services(Args, M, List, Xs', Ys),
        expand_list_op1(List, OpCode, SId, Replies),
        Args' := max(Args, 1),
        expand_list_replies(Replies, Args', true, Ok).


expand_list_services(Args, Ms, List, Xs, Ys) :-
  Args = 0 :
     Ms = _,
     List = [S?],
     Xs = [service(S, _) | Ys] ;

  Args =\= 0,
  Ms ? M  :
     Xs ! service(M, _) ,
     List ! M | 
        expand_list_services;

  Ms = [] :
     Args = _,
     List = [],
     Xs = Ys;

  otherwise |
        expand_list_services(Args, [Ms], List, Xs, Ys).


expand_list_op1(Ms, OpCode,SId,Replies) :-
  Ms ? M :
     Replies ! Reply(OpCode, M) |
        revise_service(OpCode, SId, M, Reply),
        expand_list_op1;

  Ms = [] :
     OpCode = _, SId = _,
     Replies = [].


expand_list_replies(Replies, Args, Ok1, Ok2) :-
  Replies ? true(_, _) | 
        expand_list_replies;

  Replies = [] :
     Args = _,
     Ok1 = Ok2 ;

  Replies ? {false(no_service), close, _},
  Args = 1 |
        expand_list_replies;

  otherwise,
  Replies ? Reply(OpCode, Service),
  Arity := Args + 1,
  make_tuple(Arity, Operation),
  arg(1, Operation, OpCode^),
  arg(2, Operation, Service^) |
        display_ok(Operation, {_, _, Reply}, '*'),
        expand_list_reply_false(Reply, Ok1, Ok1'),
        expand_list_replies.


expand_list_reply_false(Reply, Ok1, Ok2) :-
  Ok1 = true :
     Reply = Ok2;

  otherwise :
     Reply = _,
     Ok1 = Ok2.


revise_service(OpCode, SId, Service, Reply) :-
  Service = A # B,
  A =\= _#_ |
        computation_utils # call_list([SId # A # service_id(SId')], Ok),
        revise_service1(Ok, OpCode, SId', A, B, Reply);

  Service = A # B,
  A = X # Y |
        revise_service(OpCode, SId, X # (Y#B), Reply);

  Service =\= _#_ |
        computation_utils # call_list([SId # Service # service_id(SId')], Ok),
        revise_service2(Ok, OpCode, Service, SId', Reply).

revise_service1(Ok, OpCode, SId, NotService, Service, Reply) :-
  Ok = true :
     NotService = _ |
        revise_service(OpCode, SId, Service, Reply);

  Ok = false(no_service) :
     OpCode = _, SId = _, Service = _ |
        close_non_service(NotService, no_path, Reply);

  otherwise :
     OpCode = _, SId = _, NotService = _, Service = _,
     Ok = Reply.


revise_service2(Ok, OpCode, NotService, SId, Reply) :-
  Ok = false(no_service) :
     OpCode = _, SId = _ |
        close_non_service(NotService, no_service, Reply);

  Ok =\= true,
  Ok =\= false(no_service) :
     OpCode = _, NotService = _, SId = _ |
     Reply = Ok;

  Ok = true,
  OpCode = load :
     NotService = _, SId = _,
     Reply = true;

  Ok = true,
  OpCode =\= load,
  SId ? Name,
  SId' =\= [],
  string_to_dlist(Name, NL, [Slash]) : NotService = _,
     ascii('/', Slash) |
	computation_utils #
		call_list([SId#'_unique_id'(U), SId'#'_unique_id'(U')], Ok'),
	attached(Ok', U', NL, U, SId', Name, Reply);

  otherwise,
  Ok = true :
     OpCode = _, NotService = _, SId = _,
     Reply = false(root_service).

attached(Ok, Super, NL, Self, Scope, Name, Reply) :-

  Ok =\= true : Super = _, NL = _, Self = _ |	% close module
        Scope # '_close'(Name, Reply);

  Ok = true,
  string_to_dlist(Super, List, NL),
  string_to_dlist(Self, List, []) |		% close included directory
	Scope # '_close'(Name, Reply);

  Ok = true,
  otherwise : Super = _, NL = _,		% cant close attached directory
	      Self = _, Scope = _, Name = _,
     Reply = false(attached) .

close_non_service(NotService, Diagnostic, Reply) :-
  string(NotService) :
     Root = [] |
        Root # '_close'(NotService, Ng),
        when(Ng, Reply, false(Diagnostic));

  otherwise :
     NotService = _,
     Reply = false(Diagnostic).


when(Done, ReplyIn, ReplyOut) :-
    known(Done) :
        ReplyIn = ReplyOut.


expand_compile(Service, M, Options, Output, Args, Xs, Ys) :-

    true :
     Operation ! get_module # compile([Name | SuperId], Options,

				      Output, Result
			      ) |
        expand_service(Service, M, Output, Args, Xs, Ys, SuperId, Name,
			Operation, Reply
	),
        compile_reload(Reply, Result, SuperId, Name, Operation').


expand_service(Functor, M, Output, Args, Xs, Ys,
		SuperId, Name, Operation, Reply
) :-
        true :
            Xs = [start( computation_utils #
                            call_output([computation_utils#
                                             call_id_goal(self#S, SuperId,
                                                          Name, Reply
                                             ),
                                         identifier(Id),
					 display(option, prefix, Id?, _)
                                        | Continue],
                                        Out,
                                        [prefix(Id), type(unparse)]
                            ),
                        N,Events,_Reply,[display]
                 ),
                 Service?,computation(N?,_),
                 new_goal(N?,Functor(S))
                |Ys] |

        define_and_protect(M,S,S1,service(S?,S1),Service),
        service_output(Args,Output,Out),
        computation_id(N,Id),
        display_events(Events,Id),
	continue_operation(Reply, SuperId, Operation, Continue, Output).

continue_operation(Reply, SuperId, Operation, Continue, Output) :-

  Reply = true : Output = _,
     Continue = known(SuperId, Operation);

  Reply = false(Other) : Operation = _,
     Continue = [computation#event(Other), []#service_id(Output)],
     SuperId = [] .


service_output(Args,Out1,Out2) :-
  Args > 3 :
     Out1 = _,
     Out2 = [];

  otherwise :
     Args = _,
     Out1 = Out2.


compile_reload(Reply, Result, SuperId, Name, Operation) :-
  Reply = true, Result = true, Name =\= self :
     Operation = [SuperId#'_close'(Name,_)];

  Reply = true, Result = true, Name = self,
  SuperId ? Name' :
     Operation = [SuperId'#'_close'(Name',_)];

  Reply = true, Result = false(Other) : SuperId = _, Name = _,
     Operation = [computation#event(Other)] ;

  otherwise : Reply = _, Result = _, SuperId = _, Name = _, Operation = _ .
        

send_request(No,N,Command,Send,Request,Xs, Ys) :-
  true :
     Xs = [Computation?,Send|Ys] |
        define_and_protect(No,N,N1,computation(N,N1),Computation),
        display_ok(Request,Command,N).


meta_start(Status,Details,Ps,R,Xs,Zs) :-
  Ps = {P,Process,Service,Function,N,Events,Ok} :
     P = Process|
        expand_start(   Status,Details,
                        start(Service? # Function,N,Events,Reply),
                        R,Xs,Ys
        ),
        meta_started(Status,Reply,Ok,N,R,Ys,Zs).
        
meta_started(Status,Reply,Ok,N,Request,Xs,Zs) :-

  Reply = true :
     Status = ok,
     Xs = [new_goal(N, Request)|Zs] |
	unify_without_failure(Reply, Ok);

  otherwise :
     Status = _, N = _, Request = _,
     Xs = Zs |
	unify_without_failure(Reply, Ok);

  Status = ng : Reply = _, Ok = _, N = _, Request = _,
     Xs = Zs .


expand_start(Status, Details, Start, Request, Xs, Ys) :-
  Status = ok,
  Start   = start(RPC,A,B,C),
  Details = {M,S,No,N,Dt} :
     Xs = [Service, computation(N?,_), start(RPC,A,B,C,[display]),
	   request(N?, display(option, prefix, Id?, _Old), _Ok) | Ys] |
        computation_id(N,Id),
        define_and_protect(M,S,S1,service(S?,S1),Service),
        unify_without_failure(N?,No),
        display_data(Dt,N,Request,Ex),
        display_events(Ex,Request,Start,N),
        display_ok(Request,Start,N);

  Status  = ng,
  Details = {M,_,No,N,Dt},
  Start   = start(Service#Goal,A,B,C),
  unknown(Service) :
     M = [],
     Xs = [computation(N?, _), Start', new_goal(N, []), signal(Goal, N , _)
                |Ys],
     Start' = start(processor # [machine(idle_wait(Done)) | Nil?],A,B,C,[]) |
        unify_without_failure(N?,No),
        display_data(Dt,N,Request,Ex),
        display_events(Ex,Request,Start,N),
        display_ok(Request,Start',N),
	when(Done, [], Nil);

  otherwise,                                    % meta-start with non-RPC
  Status  = ng :
     Details = _, Start = _,
     Xs = [Request | Ys].


expand_add(Status,Request,M,P,No,Ok,Xs,Ys) :-
  Status = ok : Ok = _,
     Xs = [Service, Computation, request(S # P, N, Ok')|Ys] |
        define_and_protect(M,S,S1,service(S?,S1),Service),
        define_and_protect(No,N,N1,computation(N,N1),Computation),
        display_ok(Request,{_,_,_,Ok'},N);

  Status = ng :
     M = _,
     Send = request(P, N, Ok) |
        send_request(No,N,Send,Send,Request,Xs,Ys).


expand_current(Type, Current, New, Old, Xs, Ys) :-
  Type = 0 :
     New = _, Old = _,
     Xs = [{Current,N,N} | Ys] |
	  computation # display(term, Current = N,known(N));

  Type = 1 :
     Old = _,
     Xs = [SetNew | Ys] |
        define_and_protect(New,_,Old,{Current,New,Old},SetNew);

  Type = 2 :
     Xs = [{Current,New,Old} | Ys] |
        true.


define_and_protect(Given,New,Old,CommandIn,CommandOut) :-
  true :
     Given = New?,
     CommandIn = CommandOut,
     New = Old;

  known(Given) :
     Old = _,
     Given = New,
     CommandIn = CommandOut.


display_services(Type, Prefix, Sorted, Xs, Ys) :-
  Type = 2 :
     Prefix = _, Sorted = _,
     Xs = Ys;

  otherwise :
     Type = _,
     Xs = [display_stream(Sorted,prefix(Prefix))|Ys].
        

display_data(Dt, N, Request, Ex) :-
  Dt = [] :
     N = _, Request = _,
     Ex = 4;

  Dt = {_,_},
  arity(Request) >= 3 :
     N = _,
     Ex = 5;

  otherwise,
  Dt = {Name,Data} :
     Request = _,
     Ex = 5 |
        computation_id(N,[' ',Name],Id),
        computation # display(stream, Data,[known(Id),prefix(Id)]).


display_events(Ex, Request, Start, Number) :-
  arity(Request) >= Ex :
     Start = _, Number = _;

  arg(4,Start,Events) :
     Ex = _, Request = _ |
        computation_id(Number,Id),
        display_events(Events,Id).


display_events(Events,Id) :-
  Events ? failed(call(_),_) |
        display_events;

  otherwise,
  Events ? Event,
  known(Id) |
        display_event(Event,Display,How),
        computation # display(term, Display, [prefix(Id),close(Id,Id'),How]),
        display_events;

  Events = [] :
     Id = _.


display_event(Event,Message,How) :-

  Event = diagnostic(Diagnostic) :
     Message = Diagnostic,
     How = type(unparse) ;

  Event = comment(Comment), Comment = (_:_) :
     Message = Comment,
     How = type(unparse) ;

  Event = comment(Comment), Comment =\= (_:_) :
     Message = (Comment : ""),
     How = type(unparse) ;

  Event = failed(Service#Goal,Reason),
  string(Reason) :
     How = known(Message) |
        display_target(Service,Reason,Target),
        display_failed_rpc(Target,Goal,Reason,Message);

  Event = false(event(Event')) :
     How = known(Message) |
        display_event1(false,Event',Message);

  Event = false(Event'),
  Event' =\= event(_) :
     How = known(Message), How' = _ |
        self,
        display_event1(false,Message',Message);

  Event = event(false(Event')) :
     How = known(Message) |
        display_event1(false,Event',Message);

  Event = event(Event'),
  Event' =\= false(_) :
     How = known(Message), How' = _ |
        self,
        display_event1(event,Message',Message);

  otherwise :
     Event = Message,
     How = known(Message) .

display_event1(Functor,Event,Message) :-
  tuple(Event),
  N := arity(Event) + 1,
  make_tuple(N, M), arg(1, M, Functor^) |
        copy_event(1, Event, M, Message);

  otherwise :
     Message = Functor(Event) |
        true.


display_target(Strings,Reason,Target) :-

  Strings = [self, String | _Strings'],
  string(String),
  Reason =\= invalid :
     Target = String;

  Strings = [String | _Strings'],
  string(String), String =\= self,
  Reason =\= invalid :
     Target = String;

  otherwise :
     Reason = _,
     Target = Strings.


display_failed_rpc(Target,Goal,Reason,Out) :-
  Reason = no_service,
  known(Target) :
     Goal = _,
     Out = (Target,no_service);

  otherwise,
  known(Target) :
     Out = Reason(Target#Goal).


copy_event(N, E, M, Message) :-
  arg(N, E, A),
  N' := N + 1,
  arg(N', M, A^) |
        copy_event;

  otherwise :
     N = _, E = _,
     M = Message.


display_value(Status, Request, AM, N, Name, Value) :-
  arity(Request) > AM :
     Status = _, N = _, Name = _, Value = _;

  otherwise,
  Status = true,
  known(Name), known(Value) :
     Request = _, AM = _ |
        computation_id(N,[' ',Name,' ='],Id),
        computation # display(term, Value, [known(Id),prefix(Id)]);

  otherwise :
     Status = _, Request = _, AM = _ ,N = _, Name = _, Value = _ .


display_resolvent(Request, Resolvent, N) :-
  arity(Request) >= 3 :
     Resolvent = _, N = _;

  otherwise :
     Request = _ |
        computation_id(N,[' goal'],Id),
        display_resolvent_stream(Resolvent, Stream),
        computation#shell(display_stream, Stream, [known(Id),prefix(Id)]).


display_resolvent_stream(Resolvent, Stream) :-
  Resolvent ? Call,
  Call = call(_, _) :
     Stream ! Call |
        display_resolvent_stream;

  Resolvent ? [Name | _] # Goals |
        display_resolvent_goals(Name, Goals, Stream, Stream'),
        display_resolvent_stream;

  Resolvent = [] :
    Stream  = [].


display_resolvent_goals(Name, Goals, Stream, NewStream) :-
  Goals ? Goal :
     Stream ! Name # Goal |
        display_resolvent_goals;

  Goals = [] :
     Name   = _,
     Stream = NewStream.


display_ok(R, C, Number) :-
  A := arity(R), A =:= arity(C),
  arg(A, C, Reply),
  arg(A, R, Ok) : Number = _,
     Ok = Reply? ;

  otherwise |
        display_ok1(R,C,Number).


display_ok1(R,C,Number) :-
  A := arity(C), arg(A, C, Ng), Ng =\= true |
        computation_id(Number,Id),
        display_event(Ng,Display,_),
        computation # display(term, R = Display,
			      [known(Id),prefix(Id),known(Display)]);

  otherwise :
     R = _, C = _, Number = _.


unify(X,Y) :-
  true :
     X = Y | true;

  otherwise |
        display_events([event(error(X =\= Y))], '<*>').



status(Status,M,N,Id) :-
  Status = 1,
  N = 1 |
        computation # display(term, (M,Id));

  Status = N1-N2,
  N1 =< N, N =< N2 |
        computation # display(term, (N1-N2,M,Id));

  otherwise :
     N = _, Id = _ |
        computation # display(term, (Status,M,'?')).


conditional_id(NG,N,Id) :-
  NG = ng :
     N = 0 |
        computation_id(0,Id);

  otherwise :
     NG = _ |
        computation_id(N,Id).


computation_id(N,Id) :-
        computation_id(N,[],Id).


computation_id(N, Tail, Id) :-
  N =\= [] |
        processor # link(execute(concatenate,{['<',N,'>'|Tail],Id,0,10000}));
  otherwise : Id = _,
     Id' = "?" |
	self.


interpret_reply(Ok,Do,Request,N,Xs,Ys) :-
  Ok = true :
     Xs ! request(RPC,N,Reply),
     RPC = Do#interpret(Id, Request) |
        computation_id(N,Id),
        interpret_reply1(Reply,RPC,Xs',Ys);

  otherwise :
     Ok = _, Do = _, Request = _, N = _,
     Xs = Ys.


interpret_reply1(Ok,RPC,Xs,Ys) :-
  Ok = true :
     RPC = _,
     Xs = Ys;

  otherwise :
     Ok = _ |
        expand_arg(RPC,Xs,Ys).


display_variables(Ident, Xs, Ys) :-
  Ident = '^' :
      Xs = [ computation # dictionary(bindings, Bindings, 1),
             display_stream(Bindings,[]) | Ys ];

  Ident =  `VarName,
  string(VarName) :
      Xs = [ computation # [dictionary(find,VarName,Value,Done),
			    dictionary(state,State) | SD] | Ys ] |
        display_variable(Done,VarName,Value,State,SD);

  otherwise :
      Xs = [ computation # [dictionary(find,Ident,Value,Reply),
			    dictionary(state,State) | SD] | Ys ] |
        display_variable(Reply,Ident,Value,State,SD).


display_variable(Reply, Id, Value,State,SD) :-
  Reply = true,
  State =\= incremental,
  string(Id), known(Value) :
     SD = display(term, Id=Value);

  Reply = true,  
  State = incremental,
  string(Id), known(Value) :
    SD = incremental_display # display(Id=Value);

  integer(Id) :
    SD ! processor # link(execute(concatenate,{['_X',Id],Id',0,10000})) | 
        display_variable;

  Reply = true,
  unknown(Value), string(Id) : State = _,
     SD = display(term, uninstantiated_variable(Id)) ;
        
  Reply = false,
  unknown(Value) , string(Id) : State = _,
     SD = display(term, undeclared_variable(Id)) ;

  otherwise :
     Reply = _, Value = _, State = _,
     SD = display(term, invalid_variable_name(Id)) .


add_to_stream(X,Ms) :-
  true :
     Ms = [M? | _Ms'] |
	M  = X;

  Ms ? _M |
        add_to_stream;

  otherwise :
     X = _ |
        computation # error((Ms =\= '[_ | _]')).


close_stream(Ms) :-
   true : Ms = [];

   Ms ? _M |
        close_stream;

   otherwise |
        computation # error((Ms =\= '[_ | _]')).


/* * * * * * * Remove from multi-user system * * * * * * * * * * * * * * * * */

signal_fcp(Signal,Left,Right,Ok) :-
        tuple_request(Signal, Request),
        processor # machine(request(Request, Reply)),
        signal_fcp_reply(Reply,Left,Right,Ok).


tuple_request(TupleIn, TupleOut) :-
  tuple(TupleIn) :
     TupleOut = TupleIn;
        
  otherwise :
     TupleOut = {TupleIn}.

signal_fcp_reply(Reply,Left,Right,Ok) :-
  Reply = done :
     Left = Right,
     Ok   = true;

  otherwise :
     Left = Right,
     Ok   = false(Reply).
