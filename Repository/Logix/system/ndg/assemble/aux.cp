/* $Header: /baz/users/cvs-root/Source/system/ndg/assemble/aux.cp,v 1.1.1.1 1993/12/31 10:42:28 fcp Exp $ */
% This module, aux.cp, contains a bunch of auxilliary functions used by 
% procedures.cp

-export([deref/9, deref_value/8, call_args/9, unify_args/8, registers/8,
	do_vars/8,
	tag_labels/8, branch_labels/8, case_hash/6, allocate_list/8,
	allocate_enqueue/5, allocate_pr/5, if_tag_not/9, special_arg/3,
	ifs/10, compare/10, do_integer_arith/9, do_number_arith/9, fetch/6]).
-mode(trust).
-include(dg_opcodes).
-include(registers).
-language([evaluate,compound,colon]).

procedure deref((Register ; Indexed), Register, Register,
		Requests, Requests, SItems, Items,
		Instructions, Instructions
).

deref(Arg1, Arg2, Arg3, Requests1, Requests2, Items1, Items2,
	Instructions1, Instructions2
) :-

    Arg1 = a(I), Arg2 = a(I), Arg3 = a(K) |
	deref_value(I, K, Requests1, Requests2, Items1, Items2,
			Instructions1, Instructions2
	);

    Arg1 = a(I), Arg2 = a(J), Arg3 = a(K),
    I =\= J :
      Instructions2 = Instructions1,
      Items1 = [FCP_dg_deref_3, RegE(I), RegE(J), RegE(K) |  Items2],
      Requests1 = [instruction(deref, (I, J, K), 4) | Requests2] ;

    Arg1 = {a(I), IX}, Arg2 = a(J), Arg3 = a(K) :
      Instructions2 = Instructions1,
      Requests1 = [instruction(deref, ((I, IX), J, K), 5) | Requests2],
      Items1 = [FCP_dg_deref_subarg_3,
		RegE(I), IndexE(IX), RegE(J), RegE(K) | Items2] .


procedure deref_value(Integer, Integer, Requests, Requests, SItems, Items,
			Instructions, Instructions
).

deref_value(I, K, Requests1, Requests2, Items1, Items2,
		Instructions1, Instructions2
) :-

    Instructions1 ? if_integer(a(K), "=\=", Integer, Label) :
      Instructions2 = [deref_integer(a(I), a(K), Integer, Label)
      | Instructions1'],
      Requests2 = Requests1,
      Items2 = Items1 ;

    Instructions1 ? if_real(a(K), "=\=", Real, Label) :
      Instructions2 = [deref_real(a(I), a(K), Real, Label) | Instructions1'],
      Requests2 = Requests1,
      Items2 = Items1 ;

    Instructions1 ? if_string(a(K), "=\=", String, Label) :
      Instructions2 = [deref_string(a(I), a(K), String, Label)
		      | Instructions1'],
      Requests2 = Requests1,
      Items2 = Items1 ;

    Instructions1 ? if_tag(a(K), "=\=", nil, Label) :
      Instructions2 = [deref_nil(a(I), a(K), Label) | Instructions1'],
      Requests2 = Requests1,
      Items2 = Items1 ;

    Instructions1 ? if_tag(a(K), "=\=", list, Label) :
      Instructions2 = [old_deref_list(a(I), a(K), Label) | Instructions1'],
      Requests2 = Requests1,
      Items2 = Items1 ;

    Instructions1 ? if_tuple(a(K), "=\=", Arity, Label) :
      Instructions2 = [deref_tuple(a(I), a(K), Arity, Label) | Instructions1'],
      Requests2 = Requests1,
      Items2 = Items1 ;

    Instructions1 ? switch_on_tag(a(K), TagLabels) :
      Instructions2 = [deref_on_tag(a(I), a(K), TagLabels) | Instructions1'],
      Requests2 = Requests1,
      Items2 = Items1 ;

    Instructions1 ? branch_integer(a(K), Low, High, Labels) :
      Instructions2 = [deref_branch_integer(a(I), a(K), Low, High, Labels)
		      | Instructions1'],
      Requests2 = Requests1,
      Items2 = Items1 ;

    Instructions1 ? branch_tuple(a(K), Low, High, Labels) :
      Instructions2 = [deref_branch_tuple(a(I), a(K), Low, High, Labels)
		      | Instructions1'],
      Requests2 = Requests1,
      Items2 = Items1 ;

    Instructions1 ? case_hash_integer(a(K), EntryList) :
      Instructions2 = [deref_hash_integer(a(I), a(K), EntryList)
		      | Instructions1'],
      Requests2 = Requests1,
      Items2 = Items1 ;

    Instructions1 ? case_hash_string(a(I), EntryList) :
      Instructions2 = [deref_hash_string(a(I), a(K), EntryList)
		      | Instructions1'],
      Requests2 = Requests1,
      Items2 = Items1 ;

    Instructions1 ? if_tag(a(K), "=\=", variable, label(_)),
    Instructions1' ? suspend_on(a(I)),
    Instructions1'' ? goto(Label) :
      Instructions2 = Instructions1''',
      Requests1 = [instruction(deref_known, (I, else(Address)), 2),
		   label_reference(Label, Reference, Address)
		  | Requests2],
      Items1 = [FCP_dg_drf1_if_var_suspend, RegE(I), Reference | Items2] ;

    otherwise :
      Instructions2 = Instructions1,
      Requests1 = [instruction(deref_value, (I, K), 3) | Requests2],
      Items1 = [FCP_dg_deref_2, RegE(I), RegE(K) | Items2] .



do_vars(Regs, Arguments, K, L, TailItems, Items, Count, N) :-

    L' := L + 1,
    N' := N + 1,
    Regs ? Reg , Reg = a(I) :
	Arguments = (I, Arguments'),
	Items = [RegE(I) | Items'] |
	    do_vars;

    Regs = []:
	Arguments = '',
	Count = N,
	K = L,
	Items = TailItems.

unify_args(Asgns, Args, Requests1, Requests2, Items1, Items2, N, K) :-

    K' := K + 1,
    string_to_dlist(unify_reg_, MnemonicBase, Suffix),
    Asgns ? Asgn, Args ? Arg, Asgn = a(I) :
	Requests1 ! instruction(Mnemonic, (I, OtherArgs), Count),
	Items1 = [OpCode, RegE(I) | OtherItems] |
	    do_unify_args(FCP_dg_unify_reg_reg, OpCode, Suffix, Arg,
			  Requests1', Requests1'', OtherArgs,
			  Items1', OtherItems, Count, 2),
	    list_to_string(MnemonicBase, Mnemonic),
	    unify_args;

    K' := K + 1,
    string_to_dlist(unify_xreg_, MnemonicBase, Suffix),
    Asgns ? Asgn, Args ? Arg, Asgn = {a(I), IX} :
	Requests1 ! instruction(Mnemonic, (I, IX, OtherArgs), Count),
	Items1 = [OpCode, RegE(I), IndexE(IX) | OtherItems] |
	    do_unify_args(FCP_dg_unify_xreg_reg, OpCode, Suffix, Arg,
			  Requests1', Requests1'', OtherArgs,
			  Items1', OtherItems, Count, 3),
	    list_to_string(MnemonicBase, Mnemonic),
	    unify_args;

    Asgns = [], Args = []:
	N = K,
	Requests1 = Requests2,
	Items1 = Items2.

do_unify_args(BaseOp, OpCode, Suffix, Arg, R1Prime, R1Prime', OtherArgs,
		TailItems, Items, Count, K) :-

    Arg = a(I) ,
    OpCode^ := BaseOp,
    Count^ := K + 1,
    string_to_dlist(reg, Suffix^, []) :
	OtherArgs = (I),
	R1Prime = R1Prime',
	Items = [RegE(I) | TailItems] ;

    Arg = {a(I), IX},
    OpCode^ := BaseOp + 1,
    Count^ := K + 2,
    string_to_dlist(xreg, Suffix^, []) :
	OtherArgs = (I, IX),
	R1Prime = R1Prime',
	Items = [RegE(I), IndexE(IX) | TailItems] ;

    Arg = "&"({a(I), IX}),
    OpCode^ := BaseOp + 2,
    Count^ := K + 2,
    string_to_dlist(axreg, Suffix^, []) :
	OtherArgs = (I, IX),
	R1Prime = R1Prime',
	Items = [RegE(I), IndexE(IX) | TailItems] ;

    Arg = ro(a(I)),
    OpCode^ := BaseOp + 3,
    Count^ := K + 1,
    string_to_dlist(roreg, Suffix^, []) :
	OtherArgs = (I),
	R1Prime = R1Prime',
	Items = [RegE(I) | TailItems] ;

    Arg = ro("&"({a(I), IX})),
    OpCode^ := BaseOp + 4,
    Count^ := K + 2,
    string_to_dlist(roaxreg, Suffix^, []) :
	OtherArgs = (I, IX),
	R1Prime = R1Prime',
	Items = [RegE(I), IndexE(IX) | TailItems] ;

    Arg = car(a(I)),
    OpCode^ := BaseOp + 5,
    Count^ := K + 1,
    string_to_dlist(carreg, Suffix^, []) :
	OtherArgs = (I),
	R1Prime = R1Prime',
	Items = [RegE(I) | TailItems] ;

    Arg = integer(_),
    OpCode^ := BaseOp + 6,
    Count^ := K,
    string_to_dlist(word, Suffix^, []) :
	OtherArgs = (Arg),
	R1Prime ! data_escape(Arg, Reference),
	Items = [Reference | TailItems] ;

    integer(Arg),
    OpCode^ := BaseOp + 6,
    Count^ := K,
    string_to_dlist(word, Suffix^, []) :
	OtherArgs = (integer(Arg)),
	R1Prime ! data_escape(integer(Arg), Reference),
	Items = [Reference | TailItems] ;

    string(Arg),
    OpCode^ := BaseOp + 7,
    Count^ := K,
    string_to_dlist(string, Suffix^, []) :
	OtherArgs = (string(Arg)),
	R1Prime ! string_reference(Arg, Reference),
	Items = [ Reference | TailItems] ;

    real(Arg),
    OpCode^ := BaseOp + 8,
    Count^ := K,
    string_to_dlist(real, Suffix^, []) :
	OtherArgs = (Arg),
	R1Prime ! data_escape(Arg, Reference),
	Items = [Reference| TailItems] ;

    Arg = [],
    OpCode^ := BaseOp + 9,
    Count^ := K,
    string_to_dlist(nil, Suffix^, []) :
	OtherArgs = (''),
	R1Prime = R1Prime',
	Items = TailItems .

do_integer_arith(Prefix, BaseOp, IntArg1, IntArg2, RegArg,
			Requests, Requests', Items, Items') :-

    IntArg1 = a(_) |
	do_integer_arith_reg;

    IntArg1 =\= a(_) |
	do_integer_arith_int.

do_integer_arith_reg(Prefix, BaseOp, IntArg1, IntArg2, RegArg,
			Requests, Requests', Items, Items') :-

    IntArg2 = a(_) |
	do_integer_arith_reg_reg;

    IntArg2 =\= a(_) |
	do_integer_arith_reg_int.

do_integer_arith_int(Prefix, BaseOp, IntArg1, IntArg2, RegArg,
			Requests, Requests', Items, Items') :-

    IntArg2 = a(_) |
	do_integer_arith_int_reg;

    IntArg2 =\= a(_) |
	do_integer_arith_int_int.

do_integer_arith_reg_reg(Prefix, BaseOp, IntArg1, IntArg2, RegArg,
			Requests, Requests', Items, Items') :-

    IntArg1 = a(I), IntArg2 = a(J), RegArg = a(K),
    OpCode := BaseOp,
    string_to_dlist(Prefix, Mnemonic_Base, Suffix),
    string_to_dlist(reg_reg_reg, Suffix, []) :
      Requests ! instruction(Mnemonic, (I, J, K), 4),
      Items = [OpCode, RegE(I), RegE(J), RegE(K) | Items'] |
	list_to_string(Mnemonic_Base, Mnemonic);

    IntArg1 = a(I), IntArg2 = a(J), RegArg = {a(K), IX},
    OpCode := BaseOp + 1,
    string_to_dlist(Prefix, Mnemonic_Base, Suffix),
    string_to_dlist(reg_reg_xreg, Suffix, []) :
      Requests ! instruction(Mnemonic, (I, J, K, IX), 5),
      Items = [OpCode, RegE(I), RegE(J), RegE(K), IndexE(IX) | Items'] |
	list_to_string(Mnemonic_Base, Mnemonic).

do_integer_arith_reg_int(Prefix, BaseOp, IntArg1, IntArg2, RegArg,
			Requests, Requests', Items, Items') :-

    IntArg1 = a(I), RegArg = a(J),
    OpCode := BaseOp + 2,
    string_to_dlist(Prefix, Mnemonic_Base, Suffix),
    string_to_dlist(reg_int_reg, Suffix, []) :
      Data = integer(IntArg2),
      Requests = [instruction(Mnemonic, (I, Data, J), 3),
		data_escape(Data, IntegerWord) | Requests'],
      Items = [OpCode, RegE(I), IntegerWord, RegE(J) | Items'] |
	list_to_string(Mnemonic_Base, Mnemonic);

    IntArg1 = a(I), RegArg = {a(J), IX},
    OpCode := BaseOp + 3,
    string_to_dlist(Prefix, Mnemonic_Base, Suffix),
    string_to_dlist(reg_int_xreg, Suffix, []) :
      Data = integer(IntArg2),
      Requests = [instruction(Mnemonic, (I, Data, J, IX), 4),
		data_escape(Data, IntegerWord) | Requests'],
      Items = [OpCode, RegE(I), IntegerWord, RegE(J), IndexE(IX) | Items'] |
	list_to_string(Mnemonic_Base, Mnemonic).


do_integer_arith_int_reg(Prefix, BaseOp, IntArg1, IntArg2, RegArg,
			Requests, Requests', Items, Items') :-


    IntArg2 = a(I), RegArg = a(J),
    OpCode := BaseOp + 4,
    string_to_dlist(Prefix, Mnemonic_Base, Suffix),
    string_to_dlist(int_reg_reg, Suffix, []) :
      Data = integer(IntArg1),
      Requests = [instruction(Mnemonic, (Data, I, J), 3),
		data_escape(Data, IntegerWord) | Requests'],
      Items = [OpCode, IntegerWord, RegE(I), RegE(J)| Items'] |
	list_to_string(Mnemonic_Base, Mnemonic);

    IntArg2 = a(I), RegArg = {a(J), IX},
    OpCode := BaseOp + 5,
    string_to_dlist(Prefix, Mnemonic_Base, Suffix),
    string_to_dlist(int_reg_xreg, Suffix, []) :
      Data = integer(IntArg1),
      Requests = [instruction(Mnemonic, (Data, I, J, IX), 4),
		data_escape(Data, IntegerWord) | Requests'],
      Items = [OpCode, IntegerWord, RegE(I), RegE(J), IndexE(IX) | Items'] |
	list_to_string(Mnemonic_Base, Mnemonic).


do_integer_arith_int_int(Prefix, BaseOp, IntArg1, IntArg2, RegArg,
			Requests, Requests', Items, Items') :-

    RegArg = a(I),
    OpCode := BaseOp + 6,
    string_to_dlist(Prefix, Mnemonic_Base, Suffix),
    string_to_dlist(int_int_reg, Suffix, []) :
      Data1 = integer(IntArg1),
      Data2 = integer(IntArg2),
      Requests = [instruction(Mnemonic, (Data1, Data2, I), 2),
		data_escape(Data1, IntegerWord1),
		data_escape(Data2, IntegerWord2) | Requests'],
      Items = [OpCode, IntegerWord1, IntegerWord2, RegE(I) | Items'] |
	list_to_string(Mnemonic_Base, Mnemonic);

    RegArg = {a(I), IX},
    OpCode := BaseOp + 7,
    string_to_dlist(Prefix, Mnemonic_Base, Suffix),
    string_to_dlist(int_int_xreg, Suffix, []) :
      Data1 = integer(IntArg1),
      Data2 = integer(IntArg2),
      Requests = [instruction(Mnemonic, (Data1, Data2, I, IX), 3),
		data_escape(Data1, IntegerWord1),
		data_escape(Data2, IntegerWord2) | Requests'],
      Items = [OpCode,IntegerWord1,IntegerWord2, RegE(I), IndexE(IX) | Items'] |
	list_to_string(Mnemonic_Base, Mnemonic).


do_number_arith(Prefix, BaseOp, NumArg1, NumArg2, Reg,
		Requests, Requests', Items, Items') :-

    NumArg1 = a(_) |
	do_number_arith_reg;

    integer(NumArg1) |
	do_number_arith_int;

    real(NumArg1) |
	do_number_arith_real.

do_number_arith_reg(Prefix, BaseOp, NumArg1, NumArg2, Reg,
		Requests, Requests', Items, Items') :-

    NumArg1 = a(I), NumArg2 = a(J), Reg = a(K),
    NumArg1 = a(I), NumArg2 = a(J), Reg = a(K),
    OpCode := BaseOp,
    string_to_dlist(Prefix, Mnemonic_Base, Suffix),
    string_to_dlist(reg_reg, Suffix, []) :
      Requests = [instruction(Mnemonic, (I, J, K), 4) | Requests'],
      Items = [OpCode, RegE(I), RegE(J), RegE(K) | Items'] |
	list_to_string(Mnemonic_Base, Mnemonic);

    NumArg1 = a(I), NumArg2 =\= a(_), integer(NumArg2), Reg = a(J),
    OpCode := BaseOp + 1,
    string_to_dlist(Prefix, Mnemonic_Base, Suffix),
    string_to_dlist(reg_int, Suffix, []) :
      Data = integer(NumArg2),
      Requests = [instruction(Mnemonic, (I, Data, J), 3),
		data_escape(Data, IntegerWord) | Requests'],
      Items = [OpCode, RegE(I), IntegerWord, RegE(J) | Items'] |
	list_to_string(Mnemonic_Base, Mnemonic);

    NumArg1 = a(I), NumArg2 =\= a(_), real(NumArg2), Reg = a(J),
    OpCode := BaseOp + 2,
    string_to_dlist(Prefix, Mnemonic_Base, Suffix),
    string_to_dlist(reg_real, Suffix, []) :
      Data = real(NumArg2),
      Requests = [instruction(Mnemonic, (I, Data, J), 3),
		data_escape(Data, RealWords) | Requests'],
      Items = [OpCode, RegE(I), RealWords, RegE(J) | Items'] |
	list_to_string(Mnemonic_Base, Mnemonic).

do_number_arith_int(Prefix, BaseOp, NumArg1, NumArg2, Reg,
		Requests, Requests', Items, Items') :-

    NumArg1 =\= a(_), integer(NumArg1), NumArg2 = a(I), Reg = a(J),
    OpCode := BaseOp + 3,
    string_to_dlist(Prefix, Mnemonic_Base, Suffix),
    string_to_dlist(int_reg, Suffix, []) :
      Data = integer(NumArg1),
      Requests = [instruction(Mnemonic, (Data, I, J), 3),
		data_escape(Data, IntegerWord) | Requests'],
      Items = [OpCode, IntegerWord, RegE(I), RegE(J) | Items'] |
	list_to_string(Mnemonic_Base, Mnemonic);

    NumArg1 =\= a(_), integer(NumArg1),
    OpCode := BaseOp + 4,
    NumArg2 =\= a(_), integer(NumArg2), Reg = a(I),
    string_to_dlist(Prefix, Mnemonic_Base, Suffix),
    string_to_dlist(int_int, Suffix, []) :
      Data1 = integer(NumArg1),
      Data2 = integer(NumArg2),
      Requests = [instruction(Mnemonic, (Data1, Data2, I), 2),
		data_escape(Data1, IntegerWord1),
		data_escape(Data2, IntegerWord2) | Requests'],
      Items = [OpCode, IntegerWord1, IntegerWord2, RegE(I) | Items'] |
	list_to_string(Mnemonic_Base, Mnemonic);

    NumArg1 =\= a(_), integer(NumArg1),
    OpCode := BaseOp + 5,
    NumArg2 =\= a(_), real(NumArg2), Reg = a(I),
    string_to_dlist(Prefix, Mnemonic_Base, Suffix),
    string_to_dlist(int_real, Suffix, []) :
      Data1 = integer(NumArg1),
      Data2 = real(NumArg2),
      Requests = [instruction(Mnemonic, (Data1, Data2, I), 2),
		data_escape(Data1, IntegerWord1),
		data_escape(Data2, RealWords) | Requests'],
      Items = [OpCode, IntegerWord1, RealWords, RegE(I) | Items'] |
	list_to_string(Mnemonic_Base, Mnemonic).

do_number_arith_real(Prefix, BaseOp, NumArg1, NumArg2, Reg,
		Requests, Requests', Items, Items') :-

    NumArg1 =\= a(_), real(NumArg1),
    OpCode := BaseOp + 6,
    NumArg2 = a(I), Reg = a(J),
    string_to_dlist(Prefix, Mnemonic_Base, Suffix),
    string_to_dlist(real_reg, Suffix, []) :
      Data = real(NumArg1),
      Requests = [instruction(Mnemonic, (Data, I, J), 3),
		data_escape(Data, RealWords) | Requests'],
      Items = [OpCode, RealWords, RegE(I), RegE(J) | Items'] |
	list_to_string(Mnemonic_Base, Mnemonic);

    NumArg1 =\= a(_), real(NumArg1),
    OpCode := BaseOp + 7,
    NumArg2 =\= a(_), integer(NumArg2), Reg = a(I),
    string_to_dlist(Prefix, Mnemonic_Base, Suffix),
    string_to_dlist(real_int, Suffix, []) :
      Data1 = real(NumArg1),
      Data2 = integer(NumArg2),
      Requests = [instruction(Mnemonic, (Data1, Data2, I), 2),
		data_escape(Data1, RealWords),
		data_escape(Data2, IntegerWord) | Requests'],
      Items = [OpCode, RealWords, IntegerWord, RegE(I) | Items'] |
	list_to_string(Mnemonic_Base, Mnemonic);

    NumArg1 =\= a(_), real(NumArg1),
    OpCode := BaseOp + 8,
    NumArg2 =\= a(_), real(NumArg2), Reg = a(I),
    string_to_dlist(Prefix, Mnemonic_Base, Suffix),
    string_to_dlist(real_real, Suffix, []) :
      Data1 = real(NumArg1),
      Data2 = real(NumArg2),
      Requests = [instruction(Mnemonic, (Data1, Data2, I), 2),
		data_escape(Data1, RealWords1),
		data_escape(Data2, RealWords2) | Requests'],
      Items = [OpCode, RealWords1, RealWords2, RegE(I) | Items'] |
	list_to_string(Mnemonic_Base, Mnemonic).

procedure allocate_list(Args, Arg, Requests, Requests, Items, Items,
				Instructions, Instructions
).


allocate_list(Args, Arg, Requests1, Requests2, Items1, Items2,
		Instructions1, Instructions2
) :-

    Args = [Car | Cdr], Cdr =\= [_|_],		  % not_list(Cdr) - often []
    Args =\= [a(_) | {'_'}] :
      Instructions2 = Instructions1 |
	registers([Arg], 1, Requests1, Requests1',
		  Items1, [FCP_dg_allocate_list_cell | Items1'],
		  allocate_list_cell, Arguments?
	),
	arguments # encode([Car, Cdr], list, Requests1', Requests2,
			   Items1', Items2, Arguments
		    );

    Args = [a(J) | {'_'}] :
      Instructions2 = Instructions1 |
	registers([Arg], 2, Requests1, Requests2,
		  Items1, [FCP_dg_allocate_list_we, RegE(J) | Items2],
		  allocate_list_we, J
	);

    Args = [_ | Cdr], list(Cdr) : 
      Instructions2 = Instructions1 |
	registers([Arg], 2, Requests1, Requests1',
		  Items1, [FCP_dg_allocate_listN, N | Items1'],
		  allocate_listN, (N, Arguments)
	),
	list_arguments(Args, Args'),
	arguments # encode(Args', list, Requests1', Requests2, Items1', Items2,
				Arguments, N
		    ).


procedure list_arguments(Args, Args).

list_arguments(Args1, Args2) :-

    Args1 ? Arg :
      Args2 ! Arg |
	list_arguments;

    otherwise :
      Args2 = [Args1] .


procedure allocate_enqueue(Args, Arg, Instructions, Instructions, APSummary).

allocate_enqueue(Args, Arg, Instructions1, Instructions2, Summary) :-

    Instructions1 ? enqueue(PID, Arg) :	% Arg = a(_)
      Instructions1' = Instructions2 |
	allocate_regs_enqueue(Args, PID, Summary);

    otherwise :
      Instructions2 = Instructions1,
      Summary = allocate_pr(Args, Arg) .

procedure allocate_regs_enqueue(Args, PID, APSummary).

APSummary ::= allocate_pr(Args, Arg) ;
	      allocate_enqueue(Args, PID) ;
	      allocate_regs_enqueue(Integer, Items, Items, [Integer], PID).

allocate_regs_enqueue(Args, PID, Summary)
   + (N = 0, AllArgs = Args, Regs = R, AllRegs = R, RL = RR, RegList = RR) :-

    Args ? a(I),
    N' := N + 1 :
      Regs ! RegE(I),
      RL ! I |
	allocate_regs_enqueue;

    Args ? Other, Other =\= a(_) : Args' = _,
				   N = _, Regs = _, AllRegs = _,
				   RL = _, RegList = _ |
      Summary = allocate_enqueue(AllArgs, PID) ;

    Args = [] : AllArgs = _,
      RL = [],
      Summary = allocate_regs_enqueue(N, AllRegs, Regs, RegList, PID) .

procedure allocate_pr(APSummary, Requests, Requests, Items, Items).

allocate_pr(Summary, Requests1, Requests2, Items1, Items2) :-

    Summary = allocate_pr(Args, Arg) |
	registers([Arg], 2, Requests1, Requests1',
		  Items1, [FCP_dg_allocate_pr, N | Items1'],
		  allocate_PR, (N, Arguments)
	),
	arguments # encode(Args, process, Requests1', Requests2,
				Items1', Items2, Arguments, N
		    );

    Summary = allocate_enqueue(Args, PID) :
      Requests1 = [instruction(allocate_enqueue,
			       (N, PID @ Address, Arguments),
			       2
		   ),
		   procedure_reference(PID, Reference, Address)
		  | Requests1'],
      Items1 = [FCP_dg_alloc_pr_enqu, N, Reference | Items1'] |
	arguments # encode(Args, process,
			   Requests1', Requests2,
			   Items1', Items2,
			   Arguments, N
		    );

    Summary = allocate_regs_enqueue(N, AllRegs, AfterRegs, RegList, PID) :
      Requests1 = [instruction(allocate_regs_enqueue,
			       (N, PID @ Address, RegList), 2
		   ),
		   procedure_reference(PID, Reference, Address),
		   increment(N)
		  | Requests2],
      Items1 = [FCP_dg_alloc_pr_regs_enqu, N, Reference | AllRegs],
      AfterRegs = Items2 .


procedure fetch(Compound, Register, Requests, Requests, SItems, Items).

fetch(Term, Register, Requests1, Requests2, Items1, Items2) :-

    known(Term),
    freeze(Term, Frozen, []) :
      Requests1 = [instruction(fetch, (I, Term), 2),
		   string_reference(Frozen, Reference)
		  | Requests2] |
	Register = a(I),
      Items1 = [FCP_dg_fetch, RegE(I), Reference | Items2] ;

    otherwise |
	ground(Term, Term'),
	fetch.

procedure ground(Any, Any).
procedure ground(Any, Any, Any).

ground(Term, Right) + (Left = Term) :-

    Term ? Car |
	ground(Car, Left, Left'),
	ground;

    tuple(Term),
    N := arity(Term) |
	ground_args(Term, Left, Right, N);

    constant(Term) :
      Left = Right .

procedure ground_args(Tuple, Any, Any, Integer).

ground_args(Tuple, Left, Right, N) :-

    N > 0,
    arg(N, Tuple, A),
    N' := N - 1 |
	ground(A, Left, Left'),
	ground_args;

    N = 0 : Tuple = _,
      Left = Right .


Relation ::= "<", ">", "=<", ">=", "=", "=\=" .

procedure ifs(Integer, String, Arg, Relation, Arg, LabelRef,
		Requests, Requests, Items, Items
).

ifs(BaseOp, OperandType, Arg1, Relation, Arg2, Label, Requests1, Requests2,
	Items1, Items2
) :-
    string_to_dlist(if_, If, OT),
    string_to_dlist(OperandType, OT, Suffix) |
	if_arguments(BaseOp, Relation, OpCode, Suffix),
	list_to_string(If, IfOp),
	registers([Arg1, Arg2], 1,
		  Requests1, [label_reference(Label, Reference, Address)
			     | Requests2],
		  Items1, [OpCode, Reference | Items2],
		  IfOp, then(Address)
	).


% Even though compare is almost identical to ifs, we keep them separate now
% for possible future optimization.
procedure compare(Integer, String, Arg, Relation, Arg, LabelRef,
		Requests, Requests, Items, Items
).

compare(BaseOp, OperandType, Arg1, Relation, Arg2, Label, Requests1, Requests2,
	Items1, Items2
) :-
    string_to_dlist(cmp_, Cmp, OT),
    string_to_dlist(OperandType, OT, Suffix) |
	if_arguments(BaseOp, Relation, OpCode, Suffix),
	list_to_string(Cmp, CmpOp),
	registers([Arg1, Arg2], 1,
		  Requests1, [label_reference(Label, Reference, Address)
			     | Requests2],
		  Items1, [OpCode, Reference | Items2],
		  CmpOp, else(Address)
	).


procedure if_arguments(OpCode, Relation, OpCode, String).

if_arguments(BaseOp, Relation, OpCode, Suffix) :-

    Relation = "<",
    string_to_dlist('LT', Suffix^, []) :
      BaseOp = OpCode;

    Relation = ">",
    string_to_dlist('GT', Suffix^, []),
    OpCode^ := BaseOp + 1 |
	true;

    Relation = "=<",
    string_to_dlist('LE', Suffix^, []),
    OpCode^ := BaseOp + 2 |
	true;

    Relation = ">=",
    string_to_dlist('GE', Suffix^, []),
    OpCode^ := BaseOp + 3 |
	true;

    Relation = "=",
    string_to_dlist('EQ', Suffix^, []),
    OpCode^ := BaseOp + 4 |
	true;

    Relation = "=\=",
    string_to_dlist('NE', Suffix^, []),
    OpCode^ := BaseOp + 5 |
	true.


procedure if_tag_not(Integer, Tag, LabelRef, Requests, Requests, Items, Items,
			Instructions, Instructions
).

if_tag_not(I, Tag, Label, Requests1, Requests2, Items1, Items2,
		Instructions1, Instructions2
) :-

    Tag = variable,
    Instructions1 ? suspend_on(a(J)),
    Instructions1' = [Label | _] :
      Instructions1' = Instructions2,
      Items1 = [FCP_dg_if_var_suspend, RegE(I), RegE(J) | Items2],
      Requests1 = [instruction(if_var, (I, J), 3) | Requests2] ;

    otherwise :
      Instructions2 = Instructions1,
      Items1 = [OpCode, RegE(I), Reference | Items2],
      Requests1 = [instruction(Mnemonic, (I, then(Address)), 2),
		  label_reference(Label, Reference, Address)
		 | Requests2] |
	if_tag_opcode(Tag, OpCode, Mnemonic).

procedure if_tag_opcode(Tag, Integer, String).

if_tag_opcode(reference, FCP_dg_if_not_reference^, if_not_reference^).
if_tag_opcode(variable, FCP_dg_if_not_variable^, if_not_variable^).
if_tag_opcode(we, FCP_dg_if_not_writable^, if_not_we^).
if_tag_opcode(ro, FCP_dg_if_not_read_only^, if_not_ro^).
if_tag_opcode(integer, FCP_dg_if_not_integer^, if_not_integer^).
if_tag_opcode(real, FCP_dg_if_not_real^, if_not_real^).
if_tag_opcode(string, FCP_dg_if_not_string^, if_not_string^).
if_tag_opcode(nil, FCP_dg_if_not_nil^, if_not_nil^).
if_tag_opcode(list, FCP_dg_if_not_list^, if_not_list^).
if_tag_opcode(tuple, FCP_dg_if_not_tuple^, if_not_tuple^).
if_tag_opcode(tuple, FCP_dg_if_not_vector^, if_not_vector^).

/* * * * * * * * * * * * * * * A U X I L I A R Y * * * * * * * * * * * * * * */

procedure call_args(String, Integer, [Any], Any, Integer, Requests, Requests,
			Items, Items
).

call_args(Name, OpCode, Args, Else, N, Requests1, Requests2, Items1, Items2) :-

    Args = [] :
      N = 0,
      Requests1 = [instruction(Name, Else, 1) | Requests2],
      Items1 = [OpCode | Items2] ;

    Args = [a(I)] :
      N = 1,
      Requests1 = [instruction(Name(I), Else, 2) | Requests2],
      Items1 = [OpCode, RegE(I) | Items2] ;

    Args = [a(I), a(J)] :
      N = 2,
      Requests1 = [instruction(Name(I, J), Else, 3) | Requests2],
      Items1 = [OpCode, RegE(I), RegE(J) | Items2] ;

    Args = [a(I), a(J), a(K)] :
      N = 3,
      Requests1 = [instruction(Name(I, J, K), Else, 4) | Requests2],
      Items1 = [OpCode, RegE(I), RegE(J), RegE(K) | Items2] ;

    Args = [a(I), a(J), a(K), a(L)] :
      N = 4,
      Requests1 = [instruction(Name(I, J, K, L), Else, 5) | Requests2],
      Items1 = [OpCode, RegE(I), RegE(J), RegE(K), RegE(L) | Items2] ;

    Args = [a(I), a(J), a(K), a(L), a(M)] :
      N = 5,
      Requests1 = [instruction(Name(I, J, K, L, M), Else, 6) | Requests2],
      Items1 = [OpCode, RegE(I), RegE(J), RegE(K), RegE(L), RegE(M) | Items2] ;

    otherwise |
	load_args(Args, Args', Source, Destination, LAST_arg_reg),
	assign # multiple_copy(Source, Destination, Requests1, Requests1',
				Items1, Items1'
		 ),
	call_args.


procedure registers([Any], Count, Requests, Requests, SItems, Items, Any).

registers(Args, N, Requests1, Requests2, Items1, Items2,
		Mnemonic, Added
) :-

    Args = [],
    Items2 = [OpCode | Items2'] :
      Requests1 = [instruction(Mnemonic, Added, N) | Requests2],
      Items1 = [OpCode | Items2'] ;

    Args = [a(I)],
    N' := N + 1,
    Items2 = [OpCode | Items2'] :
      Requests1 = [instruction(Mnemonic, Added', N') | Requests2],
      Items1 = [OpCode, RegE(I) | Items2'] |
	augmented_arguments(Added, I, (I, Added), Added');

    Args = [a(I), a(J)],
    N' := N + 2,
    Items2 ? OpCode :
      Requests1 = [instruction(Mnemonic, Added', N') | Requests2],
      Items1 = [OpCode, RegE(I), RegE(J) | Items2'] |
	augmented_arguments(Added, (I, J), (I, J, Added), Added');

    Args = [a(I), a(J), a(K)],
    N' := N + 3,
    Items2 ? OpCode :
      Requests1 = [instruction(Mnemonic, Added', N') | Requests2] |
	augmented_arguments(Added, (I, J, K), (I, J, K, Added),
				Added'
	),
      Items1 = [OpCode, RegE(I), RegE(J), RegE(K) | Items2'] ;

    Args = [a(I), a(J), a(K), a(L)],
    N' := N + 4,
    Items2 ? OpCode :
      Requests1 = [instruction(Mnemonic, Added', N') | Requests2],
      Items1 = [OpCode, RegE(I), RegE(J), RegE(K), RegE(L) | Items2'] |
	augmented_arguments(Added, (I, J, K, L), (I, J, K, L, Added), Added');

    otherwise |
	load_args(Args, Args', Source, Destination, LAST_arg_reg),
	assign # multiple_copy(Source, Destination, Requests1, Requests1',
				Items1, Items1'
		 ),
	registers.

Registers ::=  [a(Number)].

procedure load_args(Args, Registers, Args, Registers, Number) .

load_args(Args1, Args2, Source, Destination, Index) :-

    Args1 ? Register, Register = a(_) :
      Args2 ! Register |
	load_args;

    Args1 ? Other, Other =\= a(_),
    Index' := Index - 1 :
      Temp =  a(Index),
      Args2 ! Temp,
      Source ! Other,
      Destination ! Temp |
	load_args;

    Args1 = [], Index > MAX_arg_reg :
      Args2 = [],
      Source = [],
      Destination = [] .


procedure augmented_arguments(Any, Any, Any, Any).

augmented_arguments(A, U, UA, AU) :-

    A = '' : UA = _,
      U = AU ;

    A =\= '' : U = _,
      UA = AU .


procedure special_arg(CompArg, SpecialTuple, CompArg).

special_arg(Arg1, Special, Arg2) :-

    integer(Arg1) :
      Arg2 = Special ;

    otherwise : Special = _,
      Arg2 = Arg1 .


procedure tag_labels(TagLabels, Count, Integer, Requests, Requests,
			Items, Items, [Integer]
).

tag_labels(TagLabels, N, TagBits, Requests1, Requests2, Items1, Items2,
		Addresses
) :-
    make_tuple(14, Tags) |
	tag_bits(TagLabels, TagBits, Tags, Otherwise),
	tag_table(Tags, N, TagBits, Requests1, Requests2, Items1, Items2,
			Otherwise, Addresses
	).

tag_bits(TagLabels, TagBits, Tags, Otherwise) :-

    TagLabels ? {Tag,Label}, list(TagLabels') |
	tag_bits,
	tag_bit(Tag, Label, TagBits', TagBits, Tags);

    TagLabels = [Otherwise^] : Tags = _,
      TagBits = 0 .

procedure tag_table(Tags, N, TagBits, Requests1, Requests2, Items1, Items2,
			String, [Integer]
).
procedure tag_table(Tags, N, TagBits, Requests1, Requests2, Items1, Items2,
			String, [Integer], Index, Count
).

tag_table(Tags, N, TagBits, Requests1, Requests2, Items1, Items2, Otherwise,
		Addresses
)	 + (K = 1, Branches = 0) :-

    TagBits /\ 1 =:= 1,
    TagBits' := TagBits/2,
    arg(K, Tags, Label),
    K' := K + 1,
    Branches' := Branches + 1 :
      Requests1 ! label_reference(Label, Reference, Address),
      Items1 ! Reference,
      Addresses ! Address |
	tag_table;

    TagBits =:= 0 : Tags = _, K = _,
      N = Branches,
      Requests1 = [label_reference(Otherwise, Reference, Address) | Requests2],
      Addresses = [Address],
      Items1 = [Reference | Items2] ;

    otherwise,
    TagBits' := TagBits/2,
    K' := K + 1 |
	tag_table.

procedure tag_bit(Tag, String, Integer, Integer, Tuple).

tag_bit(Tag, Label, TagBits1, TagBits2, Tags) :-

    Tag = reference,
    TagBits2^ := TagBits1 \/ 1,
    arg(1, Tags, Label^) |
	true;

    Tag = private_we,
    TagBits2^ := TagBits1 \/ 2,
    arg(2, Tags, Label^) |
	true;

    Tag = shared_we,
    TagBits2^ := TagBits1 \/ 4,
    arg(3, Tags, Label^) |
	true;

    Tag = private_ro,
    TagBits2^ := TagBits1 \/ 8,
    arg(4, Tags, Label^) |
	true;

    Tag = shared_ro,
    TagBits2^ := TagBits1 \/ 16,
    arg(5, Tags, Label^) |
	true;

    Tag = integer,
    TagBits2^ := TagBits1 \/ 32,
    arg(6, Tags, Label^) |
	true;

    Tag = real,
    TagBits2^ := TagBits1 \/ 64,
    arg(7, Tags, Label^) |
	true;

    Tag = string,
    TagBits2^ := TagBits1 \/ 128,
    arg(8, Tags, Label^) |
	true;

    Tag = nil,
    TagBits2^ := TagBits1 \/ 256,
    arg(9, Tags, Label^) |
	true;

    Tag = list_reference,
    TagBits2^ := TagBits1 \/ 512,
    arg(10, Tags, Label^) |
	true;

    Tag = list_integer,
    TagBits2^ := TagBits1 \/ 1024,
    arg(11, Tags, Label^) |
	true;

    Tag = list_nil,
    TagBits2^ := TagBits1 \/ 2048,
    arg(12, Tags, Label^) |
	true;

    Tag = tuple,
    TagBits2^ := TagBits1 \/ 4096,
    arg(13, Tags, Label^) |
	true;

    Tag = vector,
    TagBits2^ := TagBits1 \/ 8192,
    arg(11, Tags, Label^) |
	true;

    Tag = we,
    TagBits2^ := TagBits1 \/ 6,		% = private_we \/ shared_we
    arg(2, Tags, Label^),
    arg(3, Tags, Label^) |
	true;

    Tag = ro,
    TagBits2^ := TagBits1 \/ 24,	% = private_ro \/ shared_ro
    arg(4, Tags, Label^),
    arg(5, Tags, Label^) |
	true;

    Tag = variable,
    TagBits2^ := TagBits1 \/ 30,	% = we \/ ro
    arg(2, Tags, Label^),
    arg(3, Tags, Label^),
    arg(4, Tags, Label^),
    arg(5, Tags, Label^) |
	true;

    Tag = list,
    TagBits2^ := TagBits1 \/ 3584,	% = list_reference \/ list_integer \/
    arg(10, Tags, Label^),		%		list_nil
    arg(11, Tags, Label^),
    arg(12, Tags, Label^) |
	true.

procedure branch_labels(Labels, Requests, Requests, Items, Items, [Integer],
			Count, Count
).

branch_labels(Labels, Requests1, Requests2, Items1, Items2, Addresses, K, N) :-

    Labels ? Label,
    K' := K + 1 :
      Requests1 ! label_reference(Label, Reference, Address),
      Items1 ! Reference,
      Addresses ! Address |
	branch_labels;

    Labels = [] :
      Requests1 = Requests2,
      Items1 = Items2,
      Addresses = [],
      K = N .


procedure case_hash(LabelList, Args, Requests, Requests, Items, Items).

case_hash(EntryList, Arguments, Requests1, Requests2, Items1, Items2) :-
    EntryList = [label(_) | _] :		% Consistency Check
      Arguments = (N, Addresses),
      Requests1 ! increment(1),
      Items1 ! N |
	branch_labels(EntryList, Requests1', Requests2, Items1', Items2,
			Addresses, -1, N	% plus an otherwise label
	).
