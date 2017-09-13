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
-export([lift_m/3, bind/3, return/2]).
-export([modify/3, children_lens/2]).
-export([forms/1, form/1, farity_list/1, farity/1]).
-export([variable_list/1, variable/1]).
-export([record_defs/1, record_def/1, record_inits/1, record_init/1, record_updates/1, record_update/1]).
-export([clauses/1, clause/1, head/1, patterns/1, pattern/1, pattern_grps/1, pattern_grp/1, pattern_fields/1, pattern_field/1]).
-export([guard/1, guard0/1, guard_test/1, grecord_inits/1, grecord_init/1, gexpr_list/1, gexpr/1]).
-export([default_or_expr/1, default_or_bit_types/1, bit_types/1, bit_type/1]).
-export([expr_list/1, exprs/1, expr/1]).
-export([function_type_list/1, function_type1/1, function_type/1, function_constraint/1, function_body/1]).
-export([fun_clauses/1, icr_clauses/1, lc_bc_quals/1, lc_bc_qual/1]).
-export([constraint/1, type_list/1, type/1, map_pair_types/1, map_pair_type/1, field_types/1, field_type/1]).

%%%===================================================================
%%% API
%%%===================================================================
lift_m(Monad, F, X) ->
    bind(Monad, X,
         fun(A) ->
                 return(Monad, F(A))
         end).

%% same as monad:bind/3
bind({T, _IM} = M, X, F) ->
    T:'>>='(X, F, M);
bind(M, X, F) ->
    M:'>>='(X, F).

return({T, _IM} = M, A) ->
    T:return(A, M);
return(M, A) ->
    M:return(A).

modify(Monad, {Get, Put} = _Lens, F) ->
    fun(X) ->
            lift_m(Monad, fun(Y) -> Put(Y, X) end, F(Get(X)))
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

%% forms(Fs) -> lists:map((F) -> form(F) end, Fs).

children_lens(Type, Node) ->
    erlando_ast_lens:Type(Node).

forms(Forms) when is_list(Forms) ->
    children(forms, form, Forms).

%% -type form(Form) -> Form.
%%  Here we show every known form and valid internal structure. We do not
%%  that the ordering is correct!

%% First the various attributes.
form({attribute,_Line,module,_Mod}) ->
    [];
form({attribute,_Line,file,{_File,_Line}}) ->	%This is valid anywhere.
    [];
form({attribute,_Line,export,_Es0}) ->
    [{farity_list, lens_r(4)}];
form({attribute,_Line,import,{_Mod,_Is0}}) ->
    [{farity_list, c(lens_r(5), lens_r(2))}];
form({attribute,_Line,export_type,_Es0}) ->
    [{farity_list, lens_r(4)}];
form({attribute,_Line,optional_callbacks,_Es0}) ->
    [{farity_list, lens_r(4)}];
form({attribute,_Line,compile,_C}) ->
    [];
form({attribute,_Line,record,{_Name,_Defs0}}) ->
    [{record_defs, c(lens_r(4), lens_r(2))}];
form({attribute,_Line,asm,{function,_N,_A,_Code}}) ->
    [];
form({attribute,_Line,type,{_N,_T,_Vs}}) ->
    [{Type, c(lens_r(4), lens_r(N))} || {Type, N} <- [{type, 2}, {variable_list, 3}]];
form({attribute,_Line,opaque,{_N,_T,_Vs}}) ->
    [{Type, c(lens_r(4), lens_r(N))} || {Type, N} <- [{type, 2}, {variable_list, 3}]];
form({attribute,_Line,spec,{{_N,_A},_FTs}}) ->
    [{function_type_list, c(lens_r(4), lens_r(2))}];
form({attribute,_Line,spec,{{_M,_N,_A},_FTs}}) ->
    [{function_type_list, c(lens_r(4), lens_r(2))}];
form({attribute,_Line,callback,{{_N,_A},_FTs}}) ->
    [{function_type_list, c(lens_r(4), lens_r(2))}];
form({attribute,_Line,_Attr,_Val}) ->		%The general attribute.
    [];
form({function,_Line,_Name0,_Arity0,_Clauses0}) ->
    [{clauses, lens_r(5)}];
%% Extra forms from the parser.
form({error,_E}) -> 
    [];
form({warning,_W}) -> 
    [];
form({eof,_Line}) -> 
    [].
%% -type farity_list([Farity]) -> [Farity] when Farity <= {atom(),integer()}.

farity_list(FairyList) ->
    children(farity_list, farity, FairyList).

farity({_Name, _Arity}) ->
    [].

%% -type variable_list([Var]) -> [Var]

variable_list(List) ->
    children(variable_list, variable, List).

variable({var, _Line, _Var}) ->
    [].

%% -type record_defs([RecDef]) -> [RecDef].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *parser*!

record_defs(Defs) ->
    children(record_defs, record_def, Defs).

record_def({record_field,_Line,{atom,_La,_A},_Val0}) ->
    [{expr, lens_r(4)}];
record_def({record_field,_Line,{atom,_La,_A}}) ->
    [];
record_def({typed_record_field,{record_field,_Line,{atom,_La,_A},_Val0},_Type}) ->
    [{expr, c(lens_r(2), lens_r(4)), lens_r(3)}, {type, lens_r(3)}];
record_def({typed_record_field,{record_field,_Line,{atom,_La,_A}},_Type}) ->
    [{type, lens_r(3)}].

%% -type clauses([Clause]) -> [Clause].

clauses(Cs) ->
    children(clauses, clause, Cs).

%% -type clause(Clause) -> Clause.

clause({clause,_Line,_H0,_G0,_B0}) ->
    [{head, lens_r(3)}, {guard, lens_r(4)}, {exprs, lens_r(5)}].

%% -type head([Pattern]) -> [Pattern].

head(_Ps) -> [{patterns, lens_id()}].

%% -type patterns([Pattern]) -> [Pattern].
%%  These patterns are processed "sequentially" for purposes of variable
%%  definition etc.

patterns(Patterns) ->
    children(patterns, pattern, Patterns).

%% -type pattern(Pattern) -> Pattern.
%%  N.B. Only valid patterns are included here.

pattern({var,_Line,_V}) -> 
    [];
pattern({match,_Line,_L0,_R0}) ->
    [{pattern, lens_r(3)}, {pattern, lens_r(4)}];
pattern({integer,_Line,_I}) -> [];
pattern({char,_Line,_C}) -> [];
pattern({float,_Line,_F}) -> [];
pattern({atom,_Line,_A}) -> [];
pattern({string,_Line,_S}) -> [];
pattern({nil,_Line}) -> [];
pattern({cons,_Line,_H0,_T0}) ->
    [{pattern, lens_r(3)}, {pattern, lens_r(4)}];
pattern({tuple,_Line,_Ps0}) ->
    [{patterns, lens_r(3)}];
pattern({map,_Line,_Ps0}) ->
    [{patterns, lens_r(3)}];
pattern({map_field_exact,_Line,_K,_V}) ->
    [{expr, lens_r(3)}, {pattern, lens_r(4)}];
pattern({record,_Line,_Name,_Pfs0}) ->
    [{pattern_fields, lens_r(4)}];
pattern({record_index,_Line,_Name,_Field0}) ->
    [{pattern, lens_r(4)}];
pattern({record_field,_Line,_Rec0,_Name,_Field0}) ->
    [{expr, lens_r(3)}, {expr, lens_r(5)}];
pattern({record_field,_Line,_Rec0,_Field0}) ->
    [{expr, lens_r(3)}, {expr, lens_r(4)}];
pattern({bin,_Line,_Fs}) ->
    [{pattern_grps, lens_r(3)}];
pattern({op,_Line,_Op,_A}) ->
    [{expr, lens_r(4)}];
pattern({op,_Line,_Op,_L,_R}) ->
    [{expr, lens_r(4)}, {pattern, lens_r(5)}].

pattern_grps(Grps) ->
    children(pattern_grps, pattern_grp, Grps).

pattern_grp({bin_element,_L1,_E1,_S1,_T1}) ->
    [{default_or_expr, lens_r(4)}, {default_or_bit_types, lens_r(5)}].

default_or_expr(default) ->
    [];
default_or_expr(Expr) ->
    expr(Expr).

default_or_bit_types(default) ->
    [];
default_or_bit_types(Types) ->
    bit_types(Types).

bit_types(BitTypes) ->
    children(bit_types, bit_type, BitTypes).

bit_type(Atom) when is_atom(Atom) ->
    [];
bit_type({Atom, Integer}) when is_atom(Atom), is_integer(Integer) ->
    [].

%% -type pattern_fields([Field]) -> [Field].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

pattern_fields(Fields) ->
    children(pattern_fields, pattern_field, Fields).

pattern_field({record_field,_Lf,{atom,_La,_F},_P0}) ->
    [{pattern, lens_r(4)}];
pattern_field({record_field,_Lf,{var,_La,'_'},_P0}) ->
    [{pattern, lens_r(4)}].

%% -type guard([GuardTest]) -> [GuardTest].

guard(Guards) when is_list(Guards) ->
    children(guard, guard0, Guards).

guard0(Guards) when is_list(Guards) ->
    children(guard0, guard_test, Guards).

guard_test({call,_Line,{atom,_La,F},As0}) ->
    case erl_internal:type_test(F, length(As0)) of
        true -> 
            [{gexpr_list, lens_r(4)}];
        _ ->
            [{gexpr, lens_id()}]
    end;
guard_test(_Any) ->
    [{gexpr, lens_id()}].

%% Before R9, there were special rules regarding the expressions on
%% top level in guards. Those limitations are now lifted - therefore
%% there is no need for a special clause for the toplevel expressions.
%% -type gexpr(GuardExpr) -> GuardExpr.

gexpr({var,_Line,_V}) -> [];
gexpr({integer,_Line,_I}) -> [];
gexpr({char,_Line,_C}) -> [];
gexpr({float,_Line,_F}) -> [];
gexpr({atom,_Line,_A}) -> [];
gexpr({string,_Line,_S}) -> [];
gexpr({nil,_Line}) -> [];
gexpr({map,_Line,_Map0,_Es0}) ->
    [{gexpr, lens_r(3)}, {gexpr_list, lens_r(4)}];
gexpr({map,_Line,_Es0}) ->
    [{gexpr_list, lens_r(3)}];
gexpr({map_field_assoc,_Line,_K,_V}) ->
    [{gexpr, lens_r(3)}, {gexpr, lens_r(4)}];
gexpr({map_field_exact,_Line,_K,_V}) ->
    [{gexpr, lens_r(3)}, {gexpr, lens_r(4)}];
gexpr({cons,_Line,_H0,_T0}) ->
    [{gexpr, lens_r(3)}, {gexpr, lens_r(4)}];
gexpr({tuple,_Line,_Es0}) ->
    [{gexpr_list, lens_r(3)}];
gexpr({record_index,_Line,_Name,_Field0}) ->
    [{gexpr, lens_r(4)}];
gexpr({record_field,_Line,_Rec0,_Name,_Field0}) ->
    [{gexpr, lens_r(3)}, {gexpr, lens_r(5)}];
gexpr({record,_Line,_Name,_Inits0}) ->
    [{grecord_inits, lens_r(4)}];
gexpr({call,_Line,{atom,_La,F},As0}) ->
    case erl_internal:guard_bif(F, length(As0)) of
	true -> 
            [{gexpr_list, lens_r(4)}]
    end;
% Guard bif's can be remote, but only in the module erlang...
gexpr({call,_Line,{remote,_La,{atom,_Lb,erlang},{atom,_Lc,F}},As0}) ->
    case erl_internal:guard_bif(F, length(As0)) or
        erl_internal:arith_op(F, length(As0)) or 
        erl_internal:comp_op(F, length(As0)) or
        erl_internal:bool_op(F, length(As0)) of
	true -> 
            [{gexpr_list, lens_r(4)}]
    end;
gexpr({bin,_Line,_Fs}) ->
    [{pattern_grp, lens_r(3)}];
gexpr({op,_Line,Op,_A0}) ->
    case erl_internal:arith_op(Op, 1) or 
	 erl_internal:bool_op(Op, 1) of
        true ->             
            [{gexpr, lens_r(4)}]
    end;
gexpr({op,_Line,Op,_L0,_R0}) when Op =:= 'andalso'; Op =:= 'orelse' ->
    %% R11B: andalso/orelse are now allowed in guards.
    [{gexpr, lens_r(4)}, {gexpr, lens_r(5)}];
gexpr({op,_Line,Op,_L0,_R0}) ->
    case erl_internal:arith_op(Op, 2) or
	  erl_internal:bool_op(Op, 2) or 
	  erl_internal:comp_op(Op, 2) of
	true ->
            [{gexpr, lens_r(4)}, {gexpr, lens_r(5)}]
    end.

%% -type gexpr_list([GuardExpr]) -> [GuardExpr].
%%  These expressions are processed "in parallel" for purposes of variable
%%  definition etc.

gexpr_list(GExprList) when is_list(GExprList) ->
    children(gexpr_list, gexpr, GExprList).

grecord_inits(Inits) when is_list(Inits) ->
    children(grecord_inits, grecord_init, Inits).

grecord_init({record_field,_Lf,{atom,_La,_F},_Val0}) ->
    [{gexpr, lens_r(4)}];
grecord_init({record_field,_Lf,{var,_La,'_'},_Val0}) ->
    [{gexpr, lens_r(4)}].

%% -type exprs([Expression]) -> [Expression].
%%  These expressions are processed "sequentially" for purposes of variable
%%  definition etc.

exprs(Exprs) when is_list(Exprs) ->
    children(exprs, expr, Exprs).
%% -type expr(Expression) -> Expression.

expr({var,_Line,_V}) -> [];
expr({integer,_Line,_I}) -> [];
expr({float,_Line,_F}) -> [];
expr({atom,_Line,_A}) -> [];
expr({string,_Line,_S}) -> [];
expr({char,_Line,_C}) -> [];
expr({nil,_Line}) -> [];
expr({cons,_Line,_H0,_T0}) ->
    [{expr, lens_r(3)}, {expr, lens_r(4)}];
expr({lc,_Line,_E0,_Qs0}) ->
    [{expr, lens_r(3)}, {lc_bc_quals, lens_r(4)}];
expr({bc,_Line,_E0,_Qs0}) ->
    [{expr, lens_r(3)}, {lc_bc_quals, lens_r(4)}];
expr({tuple,_Line,_Es0}) ->
    [{expr_list, lens_r(3)}];
expr({map,_Line,_Map0,_Es0}) ->
    [{expr, lens_r(3)}, {exprs, lens_r(4)}];
expr({map,_Line,_Es0}) ->
    [{exprs, lens_r(3)}];
expr({map_field_assoc,_Line,_K,_V}) ->
    [{expr, lens_r(3)}, {expr, lens_r(4)}];
expr({map_field_exact,_Line,_K,_V}) ->
    [{expr, lens_r(3)}, {expr, lens_r(4)}];
expr({record_index,_Line,_Name,_Field0}) ->
    [{expr, lens_r(4)}];
expr({record,_Line,_Name,_Inits0}) ->
    [{record_inits, lens_r(4)}];
expr({record_field,_Line,_Rec0,_Name,_Field0}) ->
    [{expr, lens_r(3)}, {expr, lens_r(5)}];
expr({record,_Line,_Rec0,_Name,_Upds0}) ->
    [{expr, lens_r(3)}, {record_updates, lens_r(5)}];
expr({record_field,_Line,_Rec0,_Field0}) ->
    [{expr, lens_r(3)}, {expr, lens_r(4)}];
expr({block,_Line,_Es0}) ->
    %% Unfold block into a sequence.
    [{exprs, lens_r(3)}];
expr({'if',_Line,_Cs0}) ->
    [{icr_clauses, lens_r(3)}];
expr({'case',_Line,_E0,_Cs0}) ->
    [{expr, lens_r(3)}, {icr_clauses, lens_r(4)}];
expr({'receive',_Line,_Cs0}) ->
    [{icr_clauses, lens_r(3)}];
expr({'receive',_Line,_Cs0,_To0,_ToEs0}) ->
    [{icr_clauses, lens_r(3)}, {expr, lens_r(4)}, {exprs, lens_r(5)}];
expr({'try',_Line,_Es0,_Scs0,_Ccs0,_As0}) ->
    [{exprs, lens_r(3)}, {icr_clauses, lens_r(4)}, {icr_clauses, lens_r(5)}, {exprs, lens_r(6)}];
expr({'fun',_Line, _Body}) ->
    [{function_body, lens_r(3)}];
expr({named_fun,_Loc,_Name,_Cs}) ->
    [{fun_clauses, lens_r(4)}];
expr({call,_Line,_F0,_As0}) ->
    %% N.B. If F an atom then call to local function or BIF, if F a
    %% remote structure (see below) then call to other module,
    %% otherwise apply to "function".
    [{expr, lens_r(3)}, {expr_list, lens_r(4)}];
expr({'catch',_Line,_E0}) ->
    %% No new variables added.
    [{expr, lens_r(3)}];
expr({match,_Line,_P0,_E0}) ->
    [{pattern, lens_r(3)}, {expr, lens_r(4)}];
expr({bin,_Line,_Fs}) ->
    [{pattern_grp, lens_r(3)}];
expr({op,_Line,_Op,_A0}) ->
    [{expr, lens_r(4)}];
expr({op,_Line,_Op,_L0,_R0}) ->
    [{expr, lens_r(4)}, {expr, lens_r(5)}];
%% The following are not allowed to occur anywhere!
expr({remote,_Line,_M0,_F0}) ->
    [{expr, lens_r(3)}, {expr, lens_r(4)}].

function_body({clauses,_Cs0}) ->
    [{fun_clauses, lens_r(2)}];
function_body({function,_F,_A}) ->
    [];
function_body({function,M,F,A}) when is_atom(M), is_atom(F), is_integer(A) ->
    %% R10B-6: M:F/A. (Backward compatibility)
    [];
function_body({function,_M0,_F0,_A0}) ->
    %% R15: M:F/A with variables.
    [{expr, lens_r(N)} || N <- [2,3,4]].

%% -type expr_list([Expression]) -> [Expression].
%%  These expressions are processed "in parallel" for purposes of variable
%%  definition etc.

expr_list(ExprList) when is_list(ExprList) ->
    children(expr_list, expr, ExprList).

%% -type record_inits([RecordInit]) -> [RecordInit].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

record_inits(Inits) when is_list(Inits) ->
    children(record_inits, record_init, Inits).

record_init({record_field,_Lf,{atom,_La,_F},_Val0}) ->
    [{expr, lens_r(4)}];
record_init({record_field,_Lf,{var,_La,'_'},_Val0}) ->
    [{expr, lens_r(4)}].

%% -type record_updates([RecordUpd]) -> [RecordUpd].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

record_updates(Updates) when is_list(Updates) ->
    children(record_updates, record_update, Updates).

record_update({record_field,_Lf,{atom,_La,_F},_Val0}) ->
    [{expr, lens_r(4)}].

%% -type icr_clauses([Clause]) -> [Clause].

icr_clauses(Clauses) when is_list(Clauses) ->
    children(icr_clauses, clause, Clauses).

lc_bc_quals(Quals) when is_list(Quals) ->
    children(lc_bc_quals, lc_bc_qual, Quals).

%% -type lc_bc_quals([Qualifier]) -> [Qualifier].
%%  Allow filters to be both guard tests and general expressions.

lc_bc_qual({generate,_Line,_P0,_E0}) ->
    [{pattern, lens_r(3)}, {expr, lens_r(4)}];
lc_bc_qual({b_generate,_Line,_P0,_E0}) ->
    [{pattern, lens_r(3)}, {expr, lens_r(4)}];
lc_bc_qual(_Quals) ->
    [{expr, lens_id()}].

%% -type fun_clauses([Clause]) -> [Clause].

fun_clauses(Clauses) when is_list(Clauses) ->
    children(fun_clauses, clause, Clauses).

function_type_list(List) when is_list(List) ->
    children(function_type_list, function_type, List).

function_type({type,_Line,bounded_fun,[_Ft,_Fc]}) ->
    [{Type, c(lens_r(4), lens_l(N))} || {Type, N} <- [{function_type, 1}, {function_constraint, 2}]];
function_type(_Ft) ->
    [{function_type1, lens_id()}].

function_type1({type,_Line,'fun',[{type,_Lt,product,_As},_B]}) ->
    [{type_list, c(lens_r(4), c(lens_l(1), lens_r(4)))}, {type, c(lens_r(4), lens_l(2))}].

function_constraint(Constraints) when is_list(Constraints) ->
    children(function_constraint, constraint, Constraints).

constraint({type,_Line, constraint,[{atom,_L,_A},[_V,_T]]}) ->
    [{type, c(lens_r(4), c(lens_l(2), lens_l(N)))} || N <- [1, 2]].

type({ann_type,_Line,[{var,_Lv,_V},_T]}) ->
    [{type, c(lens_r(3), lens_l(2))}];
type({atom,_Line,_A}) ->
    [];
type({integer,_Line,_I}) ->
    [];
type({op,_Line,_Op,_T}) ->
    [{type, lens_r(3)}];
type({op,_Line,_Op,_L,_R}) ->
    [{type, lens_r(4)}, {type, lens_r(5)}];
type({type,_Line,binary,[_M,_N]}) ->
    [{type, c(lens_r(4), lens_l(N))} || N <- [1, 2]];
type({type,_Line,'fun',[]}) ->
    [];
type({type,_Line,'fun',[{type,_Lt,any},_B]}) ->
    [{type, c(lens_r(4), lens_l(2))}];
type({type,_Line,range,[_L,_H]}) ->
    [{type, c(lens_r(4), lens_l(N))} || N <- [1, 2]];
type({type,_Line,map,any}) ->
    [];
type({type,_Line,map,_Ps}) ->
    [{map_pair_types, lens_r(4)}];
type({type,_Line,record,[{atom,_La,_N}|_Fs]}) ->
    [{field_types, c(lens_r(4), lens_lt())}];
type({remote_type,_Line,[{atom,_Lm,_M},{atom,_Ln,_N},_As]}) ->
    [{type_list, c(lens_r(3), lens_l(3))}];
type({type,_Line,tuple,any}) ->
    [];
type({type,_Line,tuple,_Ts}) ->
    [{type_list, lens_r(4)}];
type({type,_Line,union,_Ts}) ->
    [{type_list, lens_r(4)}];
type({var,_Line,_V}) ->
    [];
type({user_type,_Line,_N,_As}) ->
    [{type_list, lens_r(4)}];
type({type,_Line,_N,_As}) ->
    [{type_list, lens_r(4)}].

map_pair_types(Types) when is_list(Types) ->
    children(map_pair_types, map_pair_type, Types).

map_pair_type({type,_Line,map_field_assoc,[_K,_V]}) ->
    [{type, c(lens_r(4), lens_l(N))} || N <- [1,2]];
map_pair_type({type,_Line,map_field_exact,[_K,_V]}) ->
    [{type, c(lens_r(4), lens_l(N))} || N <- [1,2]].

field_types(Types) when is_list(Types) ->
    children(field_types, field_type, Types).

field_type({type,_Line,field_type,[{atom,_La,_A},_T]}) ->
    [{type, c(lens_r(4), lens_l(2))}].

type_list(Types) when is_list(Types) ->
    children(type_list, type, Types).
