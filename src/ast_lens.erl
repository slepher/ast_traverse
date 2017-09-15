%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 30 Aug 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(ast_lens).

%% API
-export([modify/3, children_lens/2]).
%% forms 
-export([forms/1, form/1]).
-export([farities/1, farity/1, variables/1, variable/1]).
-export([record_defs/1, record_def/1, record_field_def/1]).
%% clauses -> {patterns, guards, expressions}
%% patterns
-export([patterns/1, pattern/1]).
-export([pattern_bin_elements/1, pattern_bin_element/1]).
-export([pattern_associations/1, pattern_association/1]).
-export([pattern_record_fields/1, pattern_record_field/1]).
%% guards
-export([guards/1, guard/1, guard_test/1]).
-export([guard_bin_elements/1, guard_bin_element/1]).
-export([guard_test_associations/1, guard_test_association/1]).
-export([guard_test_call_remote/1]).
-export([guard_test_record_fields/1, guard_test_record_field/1]).
%% expressions
-export([expressions/1, expression/1]).
-export([expression_bin_elements/1, expression_bin_element/1]).
-export([expression_associations/1, expression_association/1]).
-export([expression_call_remote/1]).
-export([record_inits/1, record_init/1, record_updates/1, record_update/1]).
-export([fun_clauses/1, fun_clause/1]).
-export([case_clauses/1, case_clause/1, if_clauses/1, if_clause/1]).
-export([receive_clauses/1, receive_clause/1, catch_clauses/1, catch_clause/1]).
-export([lc_bc_quals/1, lc_bc_qual/1]).
-export([function_types/1, function_type1/1, function_type/1, function_constraint/1, function_body/1]).

-export([constraint/1, types/1, type/1, association_types/1, association_type/1, field_types/1, field_type/1]).
-export([bit_type_specifiers/1, bit_type_specifier/1]).

%%%===================================================================
%%% API
%%%===================================================================

modify(Monad, {Get, Put} = _Lens, F) ->
    fun(X) ->
            ast_monad:lift_m(Monad, fun(Y) -> Put(Y, X) end, F(Get(X)))
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

lens_r(N) ->
    {fun(R) -> element(N, R) end,
     fun(A, R) -> setelement(N, R, A) end}.

lens_l(N) ->
    {fun(R) -> lists:nth(N, R) end,
     fun(A, R) -> setnth(N, R, A) end}.

lens_lh() ->
    {fun([H|_T]) -> H end,
     fun(A, [_H|T]) -> [A|T] end}.

lens_lt() ->
    {fun([_H|T]) -> T end,
     fun(A, [H|_T]) -> [H|A] end}.

lens_id() ->
    {fun(X) -> X end,
     fun(A, _X) -> A end}.

c({LG, LP}, {KG, KP}) ->
    {fun(R) -> KG(LG(R)) end,
     fun(A, R) -> LP(KP(A, LG(R)), R) end}.

setnth(1, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].

children(_Type, _ChildType, []) ->
    [];
children(Type, ChildType, [_H|_T]) ->
    [{ChildType, lens_lh()}, {Type, lens_lt()}].

children_lens(Type, Node) ->
    ?MODULE:Type(Node).

sub_lenses(SubLensesList) ->
    [[{Type, c(Lens, SubLens)} || {Type, SubLens} <- SubLenses] 
     || {Lens, SubLenses} <- SubLensesList].

default_or(_Type, default) ->
    [];
default_or(Type, _Value) ->
    [{Type, lens_id()}].

%% codes below inited from 
%% https://github.com/erlang/otp/blob/master/lib/stdlib/examples/erl_id_trans.erl
%% and adjusted according to 
%% http://erlang.org/doc/apps/erts/absform.html

%% forms(Fs) -> lists:map((F) -> form(F) end, Fs).

forms(Forms) when is_list(Forms) ->
    children(forms, form, Forms).


%% -type form(Form) -> Form.
%%  Here we show every known form and valid internal structure. We do not
%%  that the ordering is correct!

%% First the various attributes.

%% If F is an attribute -export([Fun_1/A_1, ..., Fun_k/A_k])
%% then Rep(F) = {attribute,LINE,export,[{Fun_1,A_1}, ..., {Fun_k,A_k}]}.
form({attribute,_Line,export,_Es0}) ->
    [{farities, lens_r(4)}];

%% If F is an attribute -import(Mod,[Fun_1/A_1, ..., Fun_k/A_k])
%% then Rep(F) = {attribute,LINE,import,{Mod,[{Fun_1,A_1}, ..., {Fun_k,A_k}]}}.
form({attribute,_Line,import,{_Mod,_Is0}}) ->
    [{farities, c(lens_r(5), lens_r(2))}];

%% If F is an attribute -module(Mod)
%% then Rep(F) = {attribute,LINE,module,Mod}.
form({attribute,_Line,module,_Mod}) ->
    [];

%% If F is an attribute -file(File,Line)
%% then Rep(F) = {attribute,LINE,file,{File,Line}}.
form({attribute,_Line,file,{_File,_Line}}) ->	%This is valid anywhere.
    [];

%% If F is a function declaration Name Fc_1 ; ... ; Name Fc_k
%% where each Fc_i is a function clause with a pattern sequence of the same length Arity
%% then Rep(F) = {function,LINE,Name,Arity,[Rep(Fc_1), ...,Rep(Fc_k)]}.
form({function,_Line,_Name0,_Arity0,_Clauses0}) ->
    [{fun_clauses, lens_r(5)}];

%% If F is a function specification -Spec Name Ft_1; ...; Ft_k
%% where Spec is either the atom spec or the atom callback
%% and each Ft_i is a possibly constrained function type with an argument sequence of the same length Arity
%% then Rep(F) = {attribute,Line,Spec,{{Name,Arity},[Rep(Ft_1), ..., Rep(Ft_k)]}}.
form({attribute,_Line,spec,{{_N,_A},_FTs}}) ->
    [{function_types, c(lens_r(4), lens_r(2))}];
form({attribute,_Line,callback,{{_N,_A},_FTs}}) ->
    [{function_types, c(lens_r(4), lens_r(2))}];

%% If F is a function specification -spec Mod:Name Ft_1; ...; Ft_k
%% where each Ft_i is a possibly constrained function type with an argument sequence of the same length Arity
%% then Rep(F) = {attribute,Line,spec,{{Mod,Name,Arity},[Rep(Ft_1), ..., Rep(Ft_k)]}}.
form({attribute,_Line,spec,{{_M,_N,_A},_FTs}}) ->
    [{function_types, c(lens_r(4), lens_r(2))}];

%% If F is a record declaration -record(Name,{V_1, ..., V_k})
%% where each V_i is a record field
%% then Rep(F) = {attribute,LINE,record,{Name,[Rep(V_1), ..., Rep(V_k)]}}
%% For Rep(V), see below.
form({attribute,_Line,record,{_Name,_Defs0}}) ->
    [{record_defs, c(lens_r(4), lens_r(2))}];

%% If F is a type declaration -Type Name(V_1, ..., V_k) :: T
%% where Type is either the atom type or the atom opaque
%% each V_i is a variable, and T is a type
%% then Rep(F) = {attribute,LINE,Type,{Name,Rep(T),[Rep(V_1), ..., Rep(V_k)]}}.
form({attribute,_Line,type,{_N,_T,_Vs}}) ->
    [{Type, c(lens_r(4), lens_r(N))} || {Type, N} <- [{type, 2}, {variables, 3}]];
form({attribute,_Line,opaque,{_N,_T,_Vs}}) ->
    [{Type, c(lens_r(4), lens_r(N))} || {Type, N} <- [{type, 2}, {variables, 3}]];

%% these a not mentioned in document absctract format
form({attribute,_Line,export_type,_Es0}) ->
    [{farities, lens_r(4)}];
form({attribute,_Line,optional_callbacks,_Es0}) ->
    [{farities, lens_r(4)}];
form({attribute,_Line,compile,_C}) ->
    [];
form({attribute,_Line,asm,{function,_N,_A,_Code}}) ->
    [];

%% If F is a wild attribute -A(T), then Rep(F) = {attribute,LINE,A,T}.
form({attribute,_Line,_Attr,_Val}) ->		%The general attribute.
    [];

%% Extra forms from the parser.
%% In addition to the representations of forms
%% the list that represents a module declaration 
%% (as returned by functions in epp(3) and erl_parse(3)) can contain the following:
%% Tuples {error,E} and {warning,W}, denoting syntactically incorrect forms and warnings.
form({error,_E}) -> 
    [];
form({warning,_W}) -> 
    [];

%% {eof,LOCATION}, denoting an end-of-stream encountered before a complete form had been parsed. 
%% The word LOCATION represents an integer, and denotes the number of the last line in the source file.
form({eof,_Line}) -> 
    [].

%% -type farities([Farity]) -> [Farity] when Farity <= {atom(),integer()}.
farities(FairyList) ->
    children(farities, farity, FairyList).

farity({_Name, _Arity}) ->
    [].

%% -type record_defs([RecDef]) -> [RecDef].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *parser*!
record_defs(Defs) ->
    children(record_defs, record_def, Defs).

%% Each field in a record declaration can have an optional, explicit, default initializer expression, and an optional type.

%% If V is (A | A = E) :: T, where T is a type, then Rep(V) = {typed_record_field, Rep(VF), Rep(T)}.
record_def({typed_record_field, _RecordField,_Type}) ->
    [{record_field, lens_r(2)}, {type, lens_r(3)}];
%% If V is (A | A = E), then Rep(V) = Rep(VF)
record_def(RecordField) ->
    record_field_def(RecordField).

%% If VF is A, then Rep(VF) = {record_field,LINE,Rep(A)}.
record_field_def({record_field,_Line,{atom,_La,_A}}) ->
    [];
%% If VF is A = E, where E is an expression, then Rep(VF) = {record_field,LINE,Rep(A),Rep(E)}.
record_field_def({record_field,_Line, {atom,_La,_A},_Val0}) ->
    [{expression, lens_r(4)}].


%% -type variables([Var]) -> [Var]

variables(List) ->
    children(variables, variable, List).

variable({var, _Line, _Var}) ->
    [].

%% -type clause(Clause) -> Clause.

clause({clause,_Line,_H0,_G0,_B0}) ->
    [{patterns, lens_r(3)}, {guards, lens_r(4)}, {expressions, lens_r(5)}].

%% -type patterns([Pattern]) -> [Pattern].
%%  These patterns are processed "sequentially" for purposes of variable
%%  definition etc.

%% If Ps is a sequence of patterns P_1, ..., P_k, then Rep(Ps) = [Rep(P_1), ..., Rep(P_k)]
%% Such sequences occur as the list of arguments to a function or fun.
patterns(Patterns) ->
    children(patterns, pattern, Patterns).

%% -type pattern(Pattern) -> Pattern.
%%  N.B. Only valid patterns are included here.

%% There are five kinds of atomic literals, which are represented in the same way in patterns, expressions, and guards:
%% If L is an atom literal, then Rep(L) = {atom,LINE,L}.
pattern({atom,_Line,_A}) -> [];

%% If L is a character literal, then Rep(L) = {char,LINE,L}.
pattern({char,_Line,_C}) -> [];

%% If L is a float literal, then Rep(L) = {float,LINE,L}.
pattern({float,_Line,_F}) -> [];

%% If L is an integer literal, then Rep(L) = {integer,LINE,L}.
pattern({integer,_Line,_I}) -> [];

%% If L is a string literal consisting of the characters C_1, ..., C_k, then Rep(L) = {string,LINE,[C_1, ..., C_k]}.
pattern({string,_Line,_S}) -> [];

%% If P is a bitstring pattern <<PBE_1, ..., PBE_k>>
%% then Rep(P) = {bin,LINE,[Rep(PBE_1), ..., Rep(PBE_k)]
%% for Rep(PBE), see below.
pattern({bin,_Line,_Fs}) ->
    [{pattern_bin_elements, lens_r(3)}];

%% If P is a compound pattern P_1 = P_2
%% then Rep(P) = {match,LINE,Rep(P_1),Rep(P_2)}.
pattern({match,_Line,_L0,_R0}) ->
    [{pattern, lens_r(3)}, {pattern, lens_r(4)}];

%% If P is a cons pattern [P_h | P_t]
%% then Rep(P) = {cons,LINE,Rep(P_h),Rep(P_t)}.
pattern({cons,_Line,_H0,_T0}) ->
    [{pattern, lens_r(3)}, {pattern, lens_r(4)}];

%% If P is a map pattern #{A_1, ..., A_k}
%% where each A_i is an association P_i_1 := P_i_2
%% then Rep(P) = {map,LINE,[Rep(A_1), ..., Rep(A_k)]}
%% For Rep(A), see below.
pattern({map,_Line,_Ps0}) ->
    [{pattern_associations, lens_r(3)}];

%% If P is a nil pattern [], then Rep(P) = {nil,LINE}.
pattern({nil,_Line}) -> [];

%% If P is an operator pattern P_1 Op P_2
%% where Op is a binary operator (this is either an occurrence of ++ applied to a literal string or character list
%% or an occurrence of an expression that can be evaluated to a number at compile time)
%% then Rep(P) = {op,LINE,Op,Rep(P_1),Rep(P_2)}.
pattern({op,_Line,_Op,_L,_R}) ->
    [{expression, lens_r(4)}, {pattern, lens_r(5)}];

%% If P is an operator pattern Op P_0
%% where Op is a unary operator (this is an occurrence of an expression that can be evaluated to a number at compile time)
%% then Rep(P) = {op,LINE,Op,Rep(P_0)}.
pattern({op,_Line,_Op,_A}) ->
    [{expression, lens_r(4)}];

%% If P is a record field index pattern #Name.Field
%% where Field is an atom, then Rep(P) = {record_index,LINE,Name,Rep(Field)}.
pattern({record_index,_Line,_Name,{atom, _Line, _Field}}) ->
    [{pattern, lens_r(4)}];

%% If P is a record pattern #Name{PF_1, ..., PF_k}
%% then Rep(P) = {record,LINE,Name,[Rep(PF_1), ..., Rep(PF_k)]}.
%% for Rep(PF), see below.
pattern({record,_Line,_Name,_Pfs0}) ->
    [{pattern_record_fields, lens_r(4)}];

%% If P is a tuple pattern {P_1, ..., P_k}
%% then Rep(P) = {tuple,LINE,[Rep(P_1), ..., Rep(P_k)]}.
pattern({tuple,_Line,_Ps0}) ->
    [{patterns, lens_r(3)}];

%% If P is a universal pattern _, then Rep(P) = {var,LINE,'_'}.
%% If P is a variable pattern V
%% then Rep(P) = {var,LINE,A}
%% where A is an atom with a printname consisting of the same characters as V.
pattern({var,_Line,_V}) -> 
    [].

pattern_bin_elements(Grps) ->
    children(pattern_bin_elements, pattern_bin_element, Grps).

%% if PBE is a bin element P:Size/TSL
%% Rep(PBE) = Rep(BE).
pattern_bin_element(BinElement) ->
    bin_element(patttern, BinElement).

pattern_associations(Associations) ->
    children(pattern_associations, pattern_association, Associations).

%% If A is an association K := V
%% then Rep(A) = {map_field_exact,LINE,Rep(K),Rep(V)}.
pattern_association({map_field_exact,_Line,_K,_V}) ->
    [{pattern, lens_r(3)}, {pattern, lens_r(4)}].

%% -type pattern_fields([Field]) -> [Field].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.
pattern_record_fields(Fields) ->
    children(pattern_record_fields, pattern_record_field, Fields).


%% if PF is an record field pattern Field = Value,
%% then Rep(PRF) = Rep(RF)
%% for Rep(RF), see below.
pattern_record_field(Field) ->
    record_field(pattern, Field).


%% -type guard([GuardTest]) -> [GuardTest].

guards(Guards) when is_list(Guards) ->
    children(guards, guard, Guards).

guard(Guard) when is_list(Guard) ->
    children(guard, guard_test, Guard).

%% Before R9, there were special rules regarding the expressions on
%% top level in guards. Those limitations are now lifted - therefore
%% there is no need for a special clause for the toplevel expressions.
%% -type guard_test(GuardExpression) -> GuardExpression.

%% If Gt is an atomic literal L, then Rep(Gt) = Rep(L).
guard_test({integer,_Line,_I}) -> [];
guard_test({char,_Line,_C}) -> [];
guard_test({float,_Line,_F}) -> [];
guard_test({atom,_Line,_A}) -> [];
guard_test({string,_Line,_S}) -> [];

%% If Gt is a bitstring constructor <<GBE_1, ..., GBE_k>>
%% then Rep(Gt) = {bin,LINE, [Rep(GBE_1), ..., Rep(GBE_k)]}. 
%% For Rep(GBE), see above
guard_test({bin,_Line,_Fs}) ->
    [{guard_bin_elements, lens_r(3)}];

%% If Gt is a cons skeleton [Gt_h | Gt_t]
%% then Rep(Gt) = {cons,LINE,Rep(Gt_h),Rep(Gt_t)}.
guard_test({cons,_Line,_H0,_T0}) ->
    [{guard_test, lens_r(3)}, {guard_test, lens_r(4)}];

%% If Gt is a function call A(Gt_1, ..., Gt_k)
%% where A is an atom, then Rep(Gt) = {call,LINE,Rep(A),[Rep(Gt_1), ..., Rep(Gt_k)]}.
guard_test({call,_Line,{atom,_La,F},As0}) ->
    case erl_internal:guard_bif(F, length(As0)) of
	true -> 
            [{guard_test, lens_r(3)}, {guard, lens_r(4)}]
    end;

%% If Gt is a function call A_m:A(Gt_1, ..., Gt_k)
%% where A_m is the atom erlang and A is an atom or an operator
%% then Rep(Gt) = {call,LINE,{remote,LINE,Rep(A_m),Rep(A)},[Rep(Gt_1), ..., Rep(Gt_k)]}.
% Guard bif's can be remote, but only in the module erlang...
guard_test({call,_Line,{remote,_La,{atom,_Lb,erlang},{atom,_Lc,F}},As0}) ->
    case erl_internal:guard_bif(F, length(As0)) or
        erl_internal:arith_op(F, length(As0)) or 
        erl_internal:comp_op(F, length(As0)) or
        erl_internal:bool_op(F, length(As0)) of
	true -> 
            [{guard_test_call_remote, lens_r(3)}, {guard, lens_r(4)}]
    end;

%% If Gt is a map creation #{A_1, ..., A_k}
%% where each A_i is an association Gt_i_1 => Gt_i_2 or Gt_i_1 := Gt_i_2
%% then Rep(Gt) = {map,LINE,[Rep(A_1), ..., Rep(A_k)]}.
%% For Rep(A), see below.
guard_test({map,_Line,_Es0}) ->
    [{guard_test_associations, lens_r(3)}];

%% If Gt is a map update Gt_0#{A_1, ..., A_k}
%% where each A_i is an association Gt_i_1 => Gt_i_2 or Gt_i_1 := Gt_i_2
%% then Rep(Gt) = {map,LINE,Rep(Gt_0),[Rep(A_1), ..., Rep(A_k)]}
%% For Rep(A), see below.
guard_test({map,_Line,_Map0,_Es0}) ->
    [{guard_test, lens_r(3)}, {guard_test_associations, lens_r(4)}];

%% If Gt is nil, [], then Rep(Gt) = {nil,LINE}.
guard_test({nil,_Line}) -> [];

%% If Gt is an operator guard test Gt_1 Op Gt_2
%% where Op is a binary operator other than match operator =
%% then Rep(Gt) = {op,LINE,Op,Rep(Gt_1),Rep(Gt_2)}.
guard_test({op,_Line,Op,_L0,_R0}) when Op =:= 'andalso'; Op =:= 'orelse' ->
    %% R11B: andalso/orelse are now allowed in guards.
    [{guard_test, lens_r(4)}, {guard_test, lens_r(5)}];
guard_test({op,_Line,Op,_L0,_R0}) ->
    case erl_internal:arith_op(Op, 2) or
	  erl_internal:bool_op(Op, 2) or 
	  erl_internal:comp_op(Op, 2) of
	true ->
            [{guard_test, lens_r(4)}, {guard_test, lens_r(5)}]
    end;

%% If Gt is an operator guard test Op Gt_0
%% where Op is a unary operator, then Rep(Gt) = {op,LINE,Op,Rep(Gt_0)}.
guard_test({op,_Line,Op,_A0}) ->
    case erl_internal:arith_op(Op, 1) or 
	 erl_internal:bool_op(Op, 1) of
        true ->             
            [{guard_test, lens_r(4)}]
    end;

%% If Gt is a record pattern #Name{GF_1, ..., GF_k}
%% then Rep(P) = {record,LINE,Name,[Rep(GF_1), ..., Rep(GF_k)]}.
%% for Rep(GF), see below.
guard_test({record,_Line,_Name,_Inits0}) ->
    [{guard_test_record_fields, lens_r(4)}];

%% If Gt is a record field access Gt_0#Name.Field
%% where Field is an atom
%% then Rep(Gt) = {record_field,LINE,Rep(Gt_0),Name,Rep(Field)}.
guard_test({record_field,_Line0,_Rec0,_Name,{atom,_Line1,_Field0}}) ->
    [{guard_test, lens_r(3)}, {guard_test, lens_r(5)}];
%% If Gt is a record field index #Name.Field
%% where Field is an atom
%% then Rep(Gt) = {record_index,LINE,Name,Rep(Field)}.
guard_test({record_index,_Line0,_Name,{atom,_Line1,_Field0}}) ->
    [{guard_test, lens_r(5)}];
%% If Gt is a tuple skeleton {Gt_1, ..., Gt_k}
%% then Rep(Gt) = {tuple,LINE,[Rep(Gt_1), ..., Rep(Gt_k)]}.
guard_test({tuple,_Line,_Es0}) ->
    [{guard, lens_r(3)}];

%% If Gt is a variable pattern V
%% then Rep(Gt) = {var,LINE,A}
%% where A is an atom with a printname consisting of the same characters as V.
guard_test({var,_Line,_V}) -> [].

guard_bin_elements(Grps) ->
    children(guard_bin_elements, guard_bin_element, Grps).

%% if GBE is a bin element P:Size/TSL
%% Rep(GBE) = Rep(BE).
guard_bin_element(Element) ->
    bin_element(guard_test, Element).

guard_test_associations(Associations) ->
    children(guard_test_associations, guard_test_association, Associations).

guard_test_association(Association) ->
    association(guard_test, Association).

guard_test_call_remote({remote,_Line2,_M0,_F0}) ->
    [{guard_test, lens_r(3)}, {guard_test, lens_r(4)}].

%% -type guard([GuardExpression]) -> [GuardExpression].
%%  These expressions are processed "in parallel" for purposes of variable
%%  definition etc.

guard_test_record_fields(Inits) when is_list(Inits) ->
    children(guard_test_record_fields, guard_test_record_field, Inits).

guard_test_record_field(Field) ->
    record_field(guard_test, Field).

%% -type expressions([Expression]) -> [Expression].
%%  These expressions are processed "sequentially" for purposes of variable
%%  definition etc.

expressions(Expressions) when is_list(Expressions) ->
    children(expressions, expression, Expressions).

%% -type expression(Expression) -> Expression.

%% If E is an atomic literal L, then Rep(E) = Rep(L).
expression({integer,_Line,_I}) -> [];
expression({float,_Line,_F}) -> [];
expression({atom,_Line,_A}) -> [];
expression({string,_Line,_S}) -> [];
expression({char,_Line,_C}) -> [];

%% If E is a bitstring comprehension <<E_0 || Q_1, ..., Q_k>>
%% where each Q_i is a qualifier
%% then Rep(E) = {bc,LINE,Rep(E_0),[Rep(Q_1), ..., Rep(Q_k)]}. For Rep(Q), see below.
expression({bc,_Line,_E0,_Qs0}) ->
    [{expression, lens_r(3)}, {lc_bc_quals, lens_r(4)}];

%% If E is a bitstring constructor <<EBE_1, ..., EBE_k>>
%% then Rep(E) = {bin,LINE, [Rep(EBE_1), ..., Rep(EBE_k)]}. 
%% For Rep(EBE), see below.
expression({bin,_Line,_Fs}) ->
    [{expression_bin_elements, lens_r(3)}];
%% If E is a block expression begin B end, where B is a body
%% then Rep(E) = {block,LINE,Rep(B)}.
expression({block,_Line,_Es}) ->
    [{expressions, lens_r(3)}];

%% If E is a case expression case E_0 of Cc_1 ; ... ; Cc_k end
%% where E_0 is an expression and each Cc_i is a case clause
%% then Rep(E) = {'case',LINE,Rep(E_0),[Rep(Cc_1), ..., Rep(Cc_k)]}.
expression({'case',_Line,_E0,_Cs0}) ->
    [{expression, lens_r(3)}, {case_clauses, lens_r(4)}];

%% If E is a catch expression catch E_0
%% then Rep(E) = {'catch',LINE,Rep(E_0)}.
expression({'catch',_Line,_E0}) ->
    [{expression, lens_r(3)}];

%% If E is a cons skeleton [E_h | E_t]
%% then Rep(E) = {cons,LINE,Rep(E_h),Rep(E_t)}.
expression({cons,_Line,_H0,_T0}) ->
    [{expression, lens_r(3)}, {expression, lens_r(4)}];

%% If E is a fun expression fun Body
%% then Rep(E) = {'fun',LINE, Rep(FB) }.
%% For Rep(FB), see below.
expression({'fun',_Line, _Body}) ->
    [{function_body, lens_r(3)}];

%% If E is a fun expression fun Name Fc_1 ; ... ; Name Fc_k end
%% where Name is a variable and each Fc_i is a function clause
%% then Rep(E) = {named_fun,LINE,Name,[Rep(Fc_1), ..., Rep(Fc_k)]}.
expression({named_fun,_Loc,_Name,_Cs}) ->
    [{fun_clauses, lens_r(4)}];

%% If E is a function call E_m:E_0(E_1, ..., E_k)
%% then Rep(E) = {call,LINE,{remote,LINE,Rep(E_m),Rep(E_0)},[Rep(E_1), ..., Rep(E_k)]}.
expression({call,_Line1, {remote,_Line2,_M0,_F0}, _As0}) ->
    [{expression_call_remote, lens_r(3)}, {expressions, lens_r(4)}];

%% If E is a function call E_0(E_1, ..., E_k)
%% then Rep(E) = {call,LINE,Rep(E_0),[Rep(E_1), ..., Rep(E_k)]}.
expression({call,_Line,_F0,_As0}) ->
    [{expression, lens_r(3)}, {expressions, lens_r(4)}];

%% If E is an if expression if Ic_1 ; ... ; Ic_k end
%% where each Ic_i is an if clause, then Rep(E) = {'if',LINE,[Rep(Ic_1), ..., Rep(Ic_k)]}.
expression({'if',_Line,_Cs0}) ->
    [{if_clauses, lens_r(3)}];
%% If E is a list comprehension [E_0 || Q_1, ..., Q_k]
%% where each Q_i is a qualifier
%% then Rep(E) = {lc,LINE,Rep(E_0),[Rep(Q_1), ..., Rep(Q_k)]}. For Rep(Q), see below.
expression({lc,_Line,_E0,_Qs0}) ->
    [{expression, lens_r(3)}, {lc_bc_quals, lens_r(4)}];

%% If E is a map creation #{EA_1, ..., EA_k}
%% where each EA_i is an association E_i_1 => E_i_2 or E_i_1 := E_i_2
%% then Rep(E) = {map,LINE,[Rep(EA_1), ..., Rep(EA_k)]}.
%% For Rep(EA), see below.
expression({map,_Line,_Es0}) ->
    [{expression_associations, lens_r(3)}];

%% If E is a map update E_0#{EA_1, ..., EA_k}
%% where each EA_i is an association E_i_1 => E_i_2 or E_i_1 := E_i_2, 
%% then Rep(E) = {map,LINE,Rep(E_0),[Rep(EA_1), ..., Rep(EA_k)]}.
%% For Rep(EA), see below.
expression({map,_Line,_Map0,_Es0}) ->
    [{expression, lens_r(3)}, {expression_associations, lens_r(4)}];

%% If E is a match operator expression P = E_0
%% where P is a pattern
%% then Rep(E) = {match,LINE,Rep(P),Rep(E_0)}.
expression({match,_Line,_P0,_E0}) ->
    [{pattern, lens_r(3)}, {expression, lens_r(4)}];

%% If E is nil, []
%% then Rep(E) = {nil,LINE}.
expression({nil,_Line}) -> [];

%% If E is an operator expression E_1 Op E_2
%% where Op is a binary operator other than match operator =
%% then Rep(E) = {op,LINE,Op,Rep(E_1),Rep(E_2)}.
expression({op,_Line,_Op,_L0,_R0}) ->
    [{expression, lens_r(4)}, {expression, lens_r(5)}];

%% If E is an operator expression Op E_0
%% where Op is a unary operator, then Rep(E) = {op,LINE,Op,Rep(E_0)}.
expression({op,_Line,_Op,_A0}) ->
    [{expression, lens_r(4)}];

%% If E is a receive expression receive Cc_1 ; ... ; Cc_k end, 
%% where each Cc_i is a case clause, 
%% then Rep(E) = {'receive',LINE,[Rep(Cc_1), ..., Rep(Cc_k)]}.
expression({'receive',_Line,_Cs0}) ->
    [{receive_clauses, lens_r(3)}];

%% If E is a receive expression receive Cc_1 ; ... ; Cc_k after E_0 -> B_t end
%% where each Cc_i is a case clause, E_0 is an expression, and B_t is a body
%% then Rep(E) = {'receive',LINE,[Rep(Cc_1), ..., Rep(Cc_k)],Rep(E_0),Rep(B_t)}.
expression({'receive',_Line,_Cs0,_To0,_ToEs0}) ->
    [{receive_clauses, lens_r(3)}, {expression, lens_r(4)}, {expressions, lens_r(5)}];

%% If E is a record creation #Name{Field_1=E_1, ..., Field_k=E_k}, 
%% where each Field_i is an atom or _, 
%% then Rep(E) = {record,LINE,Name,[{record_field,LINE,Rep(Field_1),Rep(E_1)}, ..., {record_field,LINE,Rep(Field_k),Rep(E_k)}]}.
expression({record,_Line,_Name,_Inits0}) ->
    [{record_inits, lens_r(4)}];

%% If E is a record field access E_0#Name.Field
%% where Field is an atom
%% then Rep(E) = {record_field,LINE,Rep(E_0),Name,Rep(Field)}.
expression({record_field,_Line,_Rec0,_Name,{atom, _Line1, _Field}}) ->
    [{expression, lens_r(3)}, {expression, lens_r(5)}];

%% If E is a record field index #Name.Field
%% where Field is an atom
%% then Rep(E) = {record_index,LINE,Name,Rep(Field)}.
expression({record_index,_Line,_Name,{atom, _Line1, _Field}}) ->
    [{expression, lens_r(4)}];

%% If E is a record update E_0#Name{Field_1=E_1, ..., Field_k=E_k}
%% where each Field_i is an atom
%% then Rep(E) = {record,LINE,Rep(E_0),Name,[{record_field,LINE,Rep(Field_1),Rep(E_1)}, ..., {record_field,LINE,Rep(Field_k),Rep(E_k)}]}.
expression({record,_Line,_Rec0,_Name,_Upds0}) ->
    [{expression, lens_r(3)}, {record_updates, lens_r(5)}];

%% If E is a tuple skeleton {E_1, ..., E_k}
%% then Rep(E) = {tuple,LINE,[Rep(E_1), ..., Rep(E_k)]}.
expression({tuple,_Line,_Es0}) ->
    [{expressions, lens_r(3)}];

%% If E is a try expression try B of Cc_1 ; ... ; Cc_k catch Tc_1 ; ... ; Tc_n end
%% where B is a body, each Cc_i is a case clause, and each Tc_j is a catch clause
%% then Rep(E) = {'try',LINE,Rep(B),[Rep(Cc_1), ..., Rep(Cc_k)],[Rep(Tc_1), ..., Rep(Tc_n)],[]}.

%% If E is a try expression try B after A end
%% where B and A are bodies
%% then Rep(E) = {'try',LINE,Rep(B),[],[],Rep(A)}.

%% If E is a try expression try B of Cc_1 ; ... ; Cc_k after A end
%% where B and A are a bodies
%% and each Cc_i is a case clause
%% then Rep(E) = {'try',LINE,Rep(B),[Rep(Cc_1), ..., Rep(Cc_k)],[],Rep(A)}.

%% If E is a try expression try B catch Tc_1 ; ... ; Tc_k after A end
%% where B and A are bodies, and each Tc_i is a catch clause
%% then Rep(E) = {'try',LINE,Rep(B),[],[Rep(Tc_1), ..., Rep(Tc_k)],Rep(A)}.

%% If E is a try expression try B of Cc_1 ; ... ; Cc_k catch Tc_1 ; ... ; Tc_n after A end
%% where B and A are a bodies, each Cc_i is a case clause, and each Tc_j is a catch clause, 
%% then Rep(E) = {'try',LINE,Rep(B),[Rep(Cc_1), ..., Rep(Cc_k)],[Rep(Tc_1), ..., Rep(Tc_n)],Rep(A)}.
expression({'try',_Line,_Es0,_Scs0,_Ccs0,_As0}) ->
    [{expressions, lens_r(3)}, {try_clauses, lens_r(4)}, {catch_clauses, lens_r(5)}, {expressions, lens_r(6)}];

%% If E is a variable V, then Rep(E) = {var,LINE,A},
%% where A is an atom with a printname consisting of the same characters as V.
expression({var,_Line,_V}) -> [].

expression_bin_elements(BinElements) ->
    children(expression_bin_elements, expression_bin_element, BinElements).

%% if EBE is a bin element P:Size/TSL
%% Rep(EBE) = Rep(BE).
expression_bin_element(BinElement) ->
    bin_element(expression, BinElement).

expression_associations(Associations) ->
    children(expression_associations, expression_association, Associations).

%% If EA is an association K => V or K := V
%% then Rep(EA) = Rep(A)
expression_association(Association) ->
    association(expression, Association).

expression_call_remote({remote,_Line2,_M0,_F0}) ->
    [{expression, lens_r(3)}, {expression, lens_r(4)}].

%% If FB is a fun expression Name/Arity
%% then Rep(FB) = {function,Name,Arity}.
function_body({function,_F,_A}) ->
    [];
%% If FB is a fun expression Module:Name/Arity
%% then Rep(FB) = {function,Rep(Module),Rep(Name),Rep(Arity)}.
%% (Before Erlang/OTP R15: Rep(E) = {function,Module,Name,Arity}.)
function_body({function,M,F,A}) when is_atom(M), is_atom(F), is_integer(A) ->
    [];
function_body({function,_M0,_F0,_A0}) ->
    [{expression, lens_r(N)} || N <- [2,3,4]];
%% If FB is a fun expression Fc_1 ; ... ; Fc_k end
%% where each Fc_i is a function clause
%% then Rep(E) = {clauses,[Rep(Fc_1), ..., Rep(Fc_k)]}.
function_body({clauses,_Cs0}) ->
    [{fun_clauses, lens_r(2)}].

%% -type record_inits([RecordInit]) -> [RecordInit].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.
record_inits(Inits) when is_list(Inits) ->
    children(record_inits, record_init, Inits).

record_init(Init) ->
    record_field(expression, Init).

%% -type record_updates([RecordUpd]) -> [RecordUpd].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

record_updates(Updates) when is_list(Updates) ->
    children(record_updates, record_update, Updates).

record_update({record_field,_Lf,{atom,_La,_F},_Val0}) ->
    [{expression, lens_r(3)}, {expression, lens_r(4)}].

catch_clauses(Clauses) when is_list(Clauses) ->
    children(catch_clauses, catch_clause, Clauses).

catch_clause(Clause) ->
    clause(Clause).

case_clauses(Clauses) when is_list(Clauses) ->
    children(case_clauses, case_clause, Clauses).

case_clause(Clause) when is_list(Clause) ->
    clause(Clause).

if_clauses(Clauses) when is_list(Clauses) ->
    children(if_clauses, if_clause, Clauses).

if_clause(Clause) ->
    clause(Clause).

receive_clauses(Clauses) when is_list(Clauses) ->
    children(receive_clauses, receive_clause, Clauses).

receive_clause(Clause) ->
    clause(Clause).

fun_clauses(Clauses) when is_list(Clauses) ->
    children(fun_clauses, fun_clause, Clauses).

fun_clause(Clause) ->
    clause(Clause).

lc_bc_quals(Quals) when is_list(Quals) ->
    children(lc_bc_quals, lc_bc_qual, Quals).

%% -type lc_bc_quals([Qualifier]) -> [Qualifier].
%%  Allow filters to be both guard tests and general expressions.

%% If Q is a generator P <- E, where P is a pattern and E is an expression
%% then Rep(Q) = {generate,LINE,Rep(P),Rep(E)}.
lc_bc_qual({generate,_Line,_P0,_E0}) ->
    [{pattern, lens_r(3)}, {expression, lens_r(4)}];

%% If Q is a bitstring generator P <= E,
%% where P is a pattern and E is an expression
%% then Rep(Q) = {b_generate,LINE,Rep(P),Rep(E)}.
lc_bc_qual({b_generate,_Line,_P0,_E0}) ->
    [{pattern, lens_r(3)}, {expression, lens_r(4)}];

%% If Q is a filter E, where E is an expression
%% then Rep(Q) = Rep(E).
lc_bc_qual(_Quals) ->
    [{expression, lens_id()}].

%% -type fun_clauses([Clause]) -> [Clause].

function_types(List) when is_list(List) ->
    children(function_types, function_type, List).

%% If Ft is a constrained function type Ft_1 when Fc,
%% where Ft_1 is a function type and Fc is a function constraint, 
%% then Rep(T) = {type,LINE,bounded_fun,[Rep(Ft_1),Rep(Fc)]}. For Rep(Fc), see below.

function_type({type,_Line,bounded_fun,[_Ft,_Fc]}) ->
    [{Type, c(lens_r(4), lens_l(N))} || {Type, N} <- [{function_type, 1}, {function_constraint, 2}]];
function_type(Ft) ->
    function_type1(Ft).

%% If Ft is a function type (T_1, ..., T_n) -> T_0, where each T_i is a type
%% then Rep(Ft) = {type,LINE,'fun',[{type,LINE,product,[Rep(T_1), ..., Rep(T_n)]},Rep(T_0)]}.
function_type1({type,_Line,'fun',[{type,_Lt,product,_As},_B]}) ->
    [{type, c(lens_r(4), lens_l(1))}, {type, c(lens_r(4), lens_l(2))}].

function_constraint(Constraints) when is_list(Constraints) ->
    children(function_constraint, constraint, Constraints).

%% If C is a constraint V :: T
%% where V is a type variable and T is a type
%% then Rep(C) = {type,LINE,constraint,[{atom,LINE,is_subtype},[Rep(V),Rep(T)]]}.
constraint({type,_Line, constraint,[{atom,_L,_A},[_V,_T]]}) ->
    [{type, c(lens_r(4), lens_l(1))}|[{type, c(lens_r(4), c(lens_l(2), lens_l(N)))} || N <- [1, 2]]].

%% If T is an annotated type A :: T_0
%% where A is a variable
%% then Rep(T) = {ann_type,LINE,[Rep(A),Rep(T_0)]}.
type({ann_type,_Line,[{var,_Lv,_V},_T]}) ->
    [{type, c(lens_r(3), lens_l(L))} || L <- [1,2]];

%% If T is an atom or integer literal L
%% then Rep(T) = Rep(L).
type({atom,_Line,_A}) -> [];
type({integer,_Line,_I}) -> [];

%% If T is a bitstring type <<_:M,_:_*N>>
%% where M and N are singleton integer types
%% then Rep(T) = {type,LINE,binary,[Rep(M),Rep(N)]}.
type({type,_Line,binary,[_M,_N]}) ->
    [{type, c(lens_r(4), lens_l(N))} || N <- [1, 2]];

%% If T is the empty list type []
%% then Rep(T) = {type,Line,nil,[]}.
type({type,_Line,nil,[]}) ->
    [];

%% If T is a fun type fun()
%% then Rep(T) = {type,LINE,'fun',[]}.
type({type,_Line,'fun',[]}) ->
    [];
%% If T is a fun type fun((...) -> T_0)
%% then Rep(T) = {type,LINE,'fun',[{type,LINE,any},Rep(T_0)]}.
type({type,_Line,'fun',[{type,_Lt,any},_B]}) ->
    [{type, c(lens_r(4), lens_l(L))} || L <- [1,2]];

type({type,_Line, any}) ->
    [];

%% If T is a fun type fun(Ft)
%% where Ft is a function type, then Rep(T) = Rep(Ft). For Rep(Ft), see below.
type({type,_Line,bounded_fun,_As} = Type) ->
    function_type(Type);
type({type,_Line,'fun',[{type,_Line1,product,_As}, _B]} = Type) ->
    function_type(Type);

type({type,_Line,product,_As}) ->
    [{types, lens_r(4)}];

%% If T is an integer range type L .. H
%% where L and H are singleton integer types
%% then Rep(T) = {type,LINE,range,[Rep(L),Rep(H)]}.
type({type,_Line,range,[_L,_H]}) ->
    [{type, c(lens_r(4), lens_l(N))} || N <- [1, 2]];

%% If T is a map type map()
%% then Rep(T) = {type,LINE,map,any}.
type({type,_Line,map,any}) ->
    [];

%% If T is a map type #{A_1, ..., A_k}
%% where each A_i is an association type
%% then Rep(T) = {type,LINE,map,[Rep(A_1), ..., Rep(A_k)]}. For Rep(A), see below.
type({type,_Line,map,_Ps}) ->
    [{association_types, lens_r(4)}];

%% If T is an operator type T_1 Op T_2
%% where Op is a binary operator 
%% (this is an occurrence of an expression that can be evaluated to an integer at compile time)
%%  then Rep(T) = {op,LINE,Op,Rep(T_1),Rep(T_2)}.
type({op,_Line,_Op,_L,_R}) ->
    [{type, lens_r(4)}, {type, lens_r(5)}];

%% If T is an operator type Op T_0
%% where Op is a unary operator
%% (this is an occurrence of an expression that can be evaluated to an integer at compile time)
%% then Rep(T) = {op,LINE,Op,Rep(T_0)}.
type({op,_Line,_Op,_T}) ->
    [{type, lens_r(3)}];

%% If T is a record type #Name{F_1, ..., F_k}
%% where each F_i is a record field type
%% then Rep(T) = {type,LINE,record,[Rep(Name),Rep(F_1), ..., Rep(F_k)]}
%% For Rep(F), see below.
type({type,_Line,record,[{atom,_La,_N}|_Fs]}) ->
    [{type, c(lens_r(4), lens_lh())}, {field_types, c(lens_r(4), lens_lt())}];

%% If T is a remote type M:N(T_1, ..., T_k)
%% then Rep(T) = {remote_type,LINE,[Rep(M),Rep(N),[Rep(T_1), ..., Rep(T_k)]]}.
type({remote_type,_Line,[{atom,_Lm,_M},{atom,_Ln,_N},_As]}) ->
    [{Type, c(lens_r(3), lens_l(L))} || {Type, L} <- [{type, 1}, {type,2}, {types,3}]];

%% If T is a tuple type tuple()
%% then Rep(T) = {type,LINE,tuple,any}.
type({type,_Line,tuple,any}) ->
    [];
%% If T is a tuple type {T_1, ..., T_k}
%% then Rep(T) = {type,LINE,tuple,[Rep(T_1), ..., Rep(T_k)]}.
type({type,_Line,tuple,_Ts}) ->
    [{types, lens_r(4)}];

%% If T is a type union T_1 | ... | T_k
%% then Rep(T) = {type,LINE,union,[Rep(T_1), ..., Rep(T_k)]}.
type({type,_Line,union,_Ts}) ->
    [{types, lens_r(4)}];

%% If T is a type variable V
%% then Rep(T) = {var,LINE,A}
%% where A is an atom with a printname consisting of the same characters as V
%% A type variable is any variable except underscore (_).
type({var,_Line,_V}) ->
    [];

%% If T is a user-defined type N(T_1, ..., T_k)
%% then Rep(T) = {user_type,LINE,N,[Rep(T_1), ..., Rep(T_k)]}.
type({user_type,_Line,_N,_As}) ->
    [{types, lens_r(4)}];

%% If T is a predefined (or built-in) type N(T_1, ..., T_k)
%% then Rep(T) = {type,LINE,N,[Rep(T_1), ..., Rep(T_k)]}.
type({type,_Line,_N,_As}) ->
    [{types, lens_r(4)}].

association_types(Types) when is_list(Types) ->
    children(association_types, association_type, Types).

%% If A is an association type K => V
%% where K and V are types
%% then Rep(A) = {type,LINE,map_field_assoc,[Rep(K),Rep(V)]}.
association_type({type,_Line,map_field_assoc,[_K,_V]}) ->
    [{type, c(lens_r(4), lens_l(N))} || N <- [1,2]];

%% If A is an association type K := V
%% where K and V are types
%% then Rep(A) = {type,LINE,map_field_exact,[Rep(K),Rep(V)]}.
association_type({type,_Line,map_field_exact,[_K,_V]}) ->
    [{type, c(lens_r(4), lens_l(N))} || N <- [1,2]].

field_types(Types) when is_list(Types) ->
    children(field_types, field_type, Types).

%% If F is a record field type Name :: Type
%% where Type is a type
%% then Rep(F) = {type,LINE,field_type,[Rep(Name),Rep(Type)]}.
field_type({type,_Line,field_type,[{atom,_La,_A},_T]}) ->
    [{type, c(lens_r(4), lens_l(L))} || L <- [1,2]].

types(Types) when is_list(Types) ->
    children(types, type, Types).

%% if BE is a bin element P:Size/TSL
%% Rep(BE) = [{bin_element,LINE, Rep(ST),Rep(Size),Rep(TSL)}
%% while sub_type is pattern, Rep(ST) = Rep(P), Rep(Size) = Rep(E),
%% while sub_type is expression, Rep(ST) = Rep(E), Rep(Size) = Rep(E),
%% while sub_type is guard test, Rep(ST) = Rep(Gt), Rep(Size) = Rep(Gt)
%% An omitted Size is represented by default
%% An omitted TSL is represented by default.
%% For Rep(TSL), see below
bin_element(SubType, {bin_element,_L1,_P1, S1, T1}) ->
    EType = 
        case SubType of
            guard_test ->
                guard_test;
            _ ->
                expression
        end,
    [{SubType, lens_r(3)}|
     sub_lenses([{lens_r(4), default_or(EType, S1)}, {lens_r(5), default_or(bit_type_specifiers, T1)}])].

%% A type specifier list TSL for a bitstring element is a sequence of type specifiers TS_1 - ... - TS_k
%% and Rep(TSL) = [Rep(TS_1), ..., Rep(TS_k)].
bit_type_specifiers(BitTypes) ->
    children(bit_type_specifiers, bit_type_specifier, BitTypes).

%% If TS is a type specifier A, where A is an atom, then Rep(TS) = A.
bit_type_specifier(Atom) when is_atom(Atom) ->
    [];
%% If TS is a type specifier A:Value, where A is an atom and Value is an integer, then Rep(TS) = {A,Value}.
bit_type_specifier({Atom, Integer}) when is_atom(Atom), is_integer(Integer) ->
    [].

%% If A is an association K => V
%% then Rep(A) = {map_field_assoc,LINE,Rep(K),Rep(V)}.
%% while sub_type is pattern, Rep(K) = Rep(P), Rep(V) = Rep(P)
%% while sub_type is guard_test, Rep(K) = Rep(Gt), Rep(V) = Rep(Gt)
%% while sub_type is expression, Rep(K) = Rep(Gt), Rep(V) = Rep(Gt)
association(SubType, {map_field_assoc,_Line,_K,_V}) ->
    [{SubType, lens_r(3)}, {SubType, lens_r(4)}];

%% If A is an association K := V
%% then Rep(A) = {map_field_exact,LINE,Rep(K),Rep(V)}.
%% while sub_type is pattern, Rep(K) = Rep(P), Rep(V) = Rep(P)
%% while sub_type is guard_test, Rep(K) = Rep(Gt), Rep(V) = Rep(Gt)
%% while sub_type is expression, Rep(K) = Rep(Gt), Rep(V) = Rep(Gt)
association(SubType, {map_field_exact,_Line,_K,_V}) ->
    [{SubType, lens_r(3)}, {SubType, lens_r(4)}].

%% if RF is a record field Field = Value,
%% then Rep(RF) = {record_field,LINE,Rep(Field),Rep(ST)}
%% while sub_type is pattern, Rep(ST) = Rep(P)
%% while sub_type is guard_test, Rep(ST) = Rep(Gt)
%% while sub_type is expression, Rep(ST) = Rep(E)
%% where Rep(Field) is an atom or _,
record_field(Type, {record_field,_Lf,{atom,_La,_F},_P0}) ->
    [{Type, lens_r(3)}, {Type, lens_r(4)}];
record_field(Type, {record_field,_Lf,{var,_La,'_'},_P0}) ->
    [{Type, lens_r(3)}, {Type, lens_r(4)}].
