-module(ehe_vsn5).

-uuid("c4cce5b2-5a6e-473f-a795-cac0eeabe065").

-tags([ehe,scripting,erlang]).

-description("Erlang scripting and templating engine").

%%-compile(export_all).

%% a simple template language
%% add caching????

-export([tests/0, test/0, test/1, 
	 def/3, 
	 expand_binary/2, 
	 expand_file/2, 
	 expand_string/2, 
	 expand_tokens/2, 
	 parse_string/1, 
	 parse_tokens/1,
	 parse_ehe_form/2,
	 time_expand_string/2,
	 eval/2]).

%% The two main entry points are:
%%   parse_string(Str) -> parse_tree()
%%   eval(parse_tree(), Bindings) -> {B1, Str}

%% The other exported functions are for testing purposes
%% and provide alternative inputs to parse_string
%%   expand_file, expand_string, expand_binary
%%   just just provide an interface to parse_string and eval

-import(lists, [reverse/1, reverse/2]).

tests()->
    expand1(no_ehe, "abc", "abc"),
    expand1(string1, "abcd", "<?\"abcd\"?>"),
    expand1(vars, "axb123c", "a<?N=\"123\",[$x]?>b<?N?>c"),
    expand1(if1,"t","<?if true?>t<?else?>f<?endif?>"), 
    expand1(if2,"t","<?  if true?>t<? else ?>f<? endif  ?>"), 
    expand1(if2,"f","<?  if false?>t<? else ?>f<? endif  ?>"), 
    expand1(if2,"f","<?  if 1 ?>t<? else ?>f<? endif  ?>"), 
    expand1(if3,"a1","<? N = 1, [] ?>a<? if N==1 ?>1<?endif?>"),
    expand1(if4,"a2","<? N = 2, [] ?>a<? if N==1 ?>1<?elseif N==2?>2<?endif?>"),
    expand1(if_no_clauses_match,
	    "a","<? N = 3, [] ?>a<? if N==1 ?>1<?elseif N==2?>2<?endif?>"),
    expand1(if_else_clause_matches,
	    "ayes",
	    "<? N = 3, [] ?>a<? if N==1 ?>1<?elseif N==2?>2<?else?>yes<?endif?>"),
    expand1(for1,"a1ba2b","<?for I in [[$1],[$2]]?>a<?I?>b<?endfor?>"),
    expand1(for2,"a1ba2b","<?for I in lists:seq(1,2) ?>a<?I?>b<?endfor?>"),
    expand1(bug1,"abc","<? \"abc\" ?>"),
    all_test_work_so_i_hope_the_program_is_ok.

expand1(N, Expect, Str) ->
    io:format("test:~p~n",[N]),
    {_, Result} = expand_string(Str, []),
    case Result of
	Expect ->
	    ok;
	Other ->
	    io:format("failed test:~p got:~p~n",[N, Other]),
	    exit(tests)
    end.

test() ->
    {_,Str} = test("ehe_tests_vsn5.ehe"),
    file:write_file("ehe_tests_vsn5.html",Str),
    io:format("written ehe_tests_vsn5.html~n"),
    true.

test(F) ->
    SYS = ehe_sys:make(#{file => F}),
    expand_file(F, [{'SYS',SYS}]).

def(Name, Arity, F) ->
    io:format("let:~p ~p ~p~n",[Name, Arity, F]),
    put({func,Name,Arity}, F),
    "ok".

expand_file(File, Bs) ->
    io:format("expand_file:~p~n",[File]),
    {ok, Bin} = file:read_file(File),
    expand_string(binary_to_list(Bin), Bs).

expand_binary(Bin, Bs) ->
    {Bs1, Bin1} = expand_string(binary_to_list(Bin), Bs),
    {Bin1, Bs1}.

time_expand_string(Str, Bs) ->
    {T1, Parse} = timer:tc(?MODULE, parse_string, [Str]),
    {T2, {B2,Val}} = timer:tc(?MODULE, eval, [Parse,Bs]),
    {T1,T2,B2,Val}.

expand_tokens(Toks, Bs) ->
    {ok, Parse} = parse_tokens(Toks),
    eval(Parse,Bs).

expand_string(Str, Bs) ->
    {ok, Parse} = parse_string(Str),
    eval(Parse,Bs).

eval(Parse, Bs) ->
    B1 = make_bindings(Bs, erl_eval:new_bindings()),
    %% io:format("Parse=~p~n",[Parse]),
    {B2, L} = eval_ehe(Parse, B1, []),
    {B2, iolist_to_binary(reverse(L))}.

%%----------------------------------------------------------------------
%% all the code for parse_string below
%%

parse_string(Str) ->
    %% io:format("Parse_string::|~s|~n",[Str]),
    Toks = scan(Str),
    %% io:format("Toks=~p~n",[Toks]),
    parse_tokens(Toks).

parse_tokens(Toks) ->
    Toks1 = [parse_scanned_token(I) || I<- Toks],
    %% io:format("Toks1=~p~n",[Toks1]),
    case (catch ehe_parser_vsn5:parse(Toks1)) of
	{ok, Parse} ->
	    trace({parse,ok}),
	    {ok, Parse};
	{error, {Ln,Mod,X}} ->
	    ErrStr = Mod:format_error(X),
	    io:format("uugh: Ln:~p Cause:~s~n",[Ln,ErrStr]),
	    {error, parse};
	{'EXIT', Why} ->
	    trace({'**bad**', internal_error, Why}),
	    %% the program show be fixed - don't bother with 
	    %% a fancy return value the claeer won't be ablwe to
	    %% do anything with  it anywhay
	    {error, internal};
	OO ->
	    io:format("cannot parse tokens:~p~n",[OO]),
	    {error, tokens}
    end.

trace(_X) ->
    %% io:format("~p:~p ~p~n",[?MODULE,?LINE,X]).
    true.
    

%%----------------------------------------------------------------------

-spec scan(string()) -> [{form,Line::integer(), string()} |
			 {str, Line::integer(), string()}].

scan(Str) -> scan(Str, 1, []).

scan([], _, L) ->
    reverse(L);
scan("<?" ++ T, Ln, L) ->
    {Str, Ln1, T1} = collect_form(T, Ln, []),
    scan(T1, Ln1, [{form,Ln,Str}|L]);
scan(T, Ln, L) ->
    {Str, Ln1, T1} = collect_str(T, Ln, []),
    scan(T1, Ln1, [{str,Ln,Str}|L]). 

collect_form("?>" ++ T, Ln, L) -> {reverse(L), Ln, T};
collect_form([$\n|T], Ln, L)   -> collect_form(T, Ln+1, [$\n|L]);
collect_form([H|T], Ln, L)     -> collect_form(T, Ln, [H|L]);
collect_form([], Ln, L)        -> {reverse(L), Ln, []}.

collect_str([$\n|T], Ln, L)     -> collect_str(T, Ln+1, [$\n|L]);
collect_str([], Ln, L)          -> {reverse(L), Ln, []};
collect_str("<?" ++ _=T, Ln, L) -> {reverse(L), Ln, T};
collect_str([H|T], Ln, L)       -> collect_str(T, Ln, [H|L]).

%%----------------------------------------------------------------------

parse_scanned_token({form,Ln,Str}) -> parse_ehe_form(Str, Ln);
parse_scanned_token({str,_,_}=S)   -> S.

parse_ehe_form(Str, Ln) ->
    case erl_scan:string(Str ++ ".", Ln) of
	{ok, Toks, _} ->
	    %% io:format("parse_form Ln::~p Toks=~p~n",[Ln, Toks]),
	    parse_erl_toks(Toks);
	Other ->
	    io:format("cannot tokenise:~p Line:~p Reason=~p~n",[Str,Ln, Other]),
	    exit({badForm, Ln})
    end.

parse_erl_toks([{atom,Ln,for},{var,_,Var},{atom,_,in} | Toks]) ->
    {forToken,Ln,{Var, parse_erl_exprs(Toks)}};
parse_erl_toks([{'if',Ln}|Toks])           -> {ifToken,Ln, parse_erl_exprs(Toks)};
parse_erl_toks([{atom,Ln,elseif}|Toks])    -> {elseifToken, Ln, parse_erl_exprs(Toks)};
parse_erl_toks([{atom,Ln,set},{var,_,Var}, {dot,_}])    -> {setToken, Ln, Var};
parse_erl_toks([{atom,Ln,endSet},{dot,_}]) -> {endSetToken, Ln};
parse_erl_toks([{atom,Ln,endif},{dot,_}])  -> {endIfToken, Ln};
parse_erl_toks([{atom,Ln,endfor},{dot,_}]) -> {endForToken, Ln};
parse_erl_toks(Toks)                       -> {exprToken, 
					       line_number(Toks), 
					       parse_erl_exprs(Toks)}.

parse_erl_exprs(Toks) ->
    Ln = line_number(Toks),
    case erl_parse:parse_exprs(Toks) of
	{ok, Exprs} -> 
	    Exprs;
	Other ->
	    io:format("cannot parse:~p line:~p Reason=~p~n",
		      [Toks,Ln,Other]),
	    %% If we get this far we'll just abort and expect the
	    %% user to correct the source
	    %% The Error is NOT in terms of
	    %% the ehe source which might be confusing - but we'll live
	    %% with this for the moment.
	    exit({badForm, Ln})
    end.

%% Now we're doen with parsing
%%----------------------------------------------------------------------


%% Section: EVAL


make_bindings([{K,V}|T], B) -> make_bindings(T, erl_eval:add_binding(K,V,B));
make_bindings([], B)        -> B.

add_to_output(B) when is_binary(B)  -> binary_to_list(B);
add_to_output(N) when is_integer(N) -> integer_to_list(N);
add_to_output(A) when is_atom(A)    -> atom_to_list(A);
add_to_output(F) when is_float(F)   -> float_to_list(F);
add_to_output(Pid) when is_pid(Pid) -> quote(pid_to_list(Pid));
add_to_output([])                   -> [];
add_to_output(L)                    ->  L.

eval_ehe({eheFor,X,Y}, B0, L)      -> eval_ehe_for(X, Y, B0, L);
eval_ehe({eheSeq,S}, B0, L)        -> eval_ehe_seq(S, B0, L);
eval_ehe({eheSet,Var, Seq}, B0, L) -> eval_ehe_set(Var, Seq, B0, L);
eval_ehe({eheIf,Expr,Else}, B0, L) -> eval_ehe_if(Expr, Else, B0, L);
eval_ehe({eheStr,S}, B, L)         -> {B, reverse(S, L)};
eval_ehe([], B, L)                 -> {B, L};
eval_ehe({eheExpr, E}, B0, L)      ->
    {value, Value, B1} = eval_erl(E, B0),
    {B1, reverse(add_to_output(Value), L)};
eval_ehe(X, _, _) ->
    %% Program is fucked - bail out - 
    exit({ohDearTheProgramNeedsFixing,X}).

eval_ehe_set(Var, Seq, B0, L) ->
    {B1, L1} = eval_ehe(Seq, B0, []),
    L2 = iolist_to_binary(reverse(L1)),
    B2 = [{Var,L2}|B1],
    {B2, L}.
    
%% for loops use same bindings at entry
%% exported bindings are thrown away

eval_ehe_for({Var, Expr}, Seq, B0, L) ->
    {value, Vals, _B1} = eval_erl(Expr, B0),
    eval_ehe_for1(Var, Vals, Seq, B0, L).

eval_ehe_for1(Var, [H|T], Seq, B0, L) ->
    %% add a binding
    B1 = [{Var,H}|B0],
    {_B1, L1} = eval_ehe(Seq, B1, L),
    eval_ehe_for1(Var, T, Seq, B0, L1);
eval_ehe_for1(_Var, [], _Seq, B, L) ->
    {B, L}.

eval_ehe_seq([H|T], B0, L) ->
    {B1, L1} = eval_ehe(H, B0, L),
    eval_ehe_seq(T, B1, L1);
eval_ehe_seq([], B, L) ->
    {B, L}.

eval_ehe_if([{test,Expr,Rhs}|T], Else, B0, L) ->
    {value, Val, _B1} = eval_erl(Expr, B0),
    case Val of
	true ->
	    eval_ehe(Rhs, B0, L);
	_ ->
	    %% UUmm not true = false
	    %% not sure but I'll be lispy at the moment
	    eval_ehe_if(T, Else, B0, L)
    end;
eval_ehe_if([], no, B, L) ->
    %% there was no else clause
    {B, L};
eval_ehe_if([], {yes,Expr}, B, L) ->
    %% there was an else bit
    eval_ehe(Expr, B, L).

line_number([{_Tag,Ln}|_])      -> Ln;
line_number([{_Tag,Ln,_Val}|_]) -> Ln;
line_number(_)                  -> -1.

eval_erl(Exprs, B0) ->
    case (catch erl_eval:exprs(Exprs, B0, {eval, fun local/3})) of
	{'EXIT', Why} ->
	    io:format("Error:~p~n",[Why]),
	    {value, "<p>** error see the log for why **</p>", B0};
	{redirect, X} ->
	    throw({redirect,X});
	Other ->
	    Other
    end.

local(def,[{atom,_,Name},{integer,_,Arity},{'fun',_,{clauses,Clauses}}], B0) ->
    put({func,Name,Arity}, Clauses),
    {value, true, B0};
local(bindings, [], B0) ->
    {value, B0, B0};
local(pre, [Expr], B0) ->
    {value, Val, B1} = eval_erl([Expr], B0),
    {value, pre(Val), B1};
local(show_all_bindings,[],B0) ->
    {value, pre(B0), B0};
local(show_bindings,[],B0) ->
    B1 = lists:sort(lists:keydelete('SYS', 1, B0)),
    {value, ["<h1>Bindings</h1>",pre(B1)], B0};
local(redirect, [Expr], B0) ->
    {value, Val, _B1} = eval_erl([Expr], B0),
    io:format("Redirecting to:~p~n",[Val]),
    throw({redirect, Val});
local(Name, Args, B0) ->
    Arity = length(Args),
    Clauses = get({func,Name,Arity}),
    LF = {eval, fun local/3},
    {Vals, _B1} =  eval1(Args, B0, LF),
    case erl_eval:match_clause(Clauses, Vals, [], LF) of
	{Body, Bs1} ->
	    {value, ZZ, _} = erl_eval:exprs(Body, Bs1, LF),
	    {value, ZZ, B0};
	nomatch ->
	    erlang:error({function_clause,[{local,Name,Args}]})
    end.

eval1([H|T], B0, LF) ->
    {value, H1, B1} = erl_eval:expr(H, B0, LF),
    {T1, B2} = eval1(T, B1, LF),
    {[H1|T1], B2};
eval1([], B, _) ->
    {[], B}.

%% {value, (get({func, fac,1}))(10), B0}.
%% {value, pre({missing,Name,Args}),  B0}.
    
pre(X) ->
    ["<pre>\n",quote(lists:flatten(io_lib:format("~p",[X]))), "</pre>"].

quote("<" ++ T) -> "&lt;" ++ quote(T);
quote("&" ++ T) -> "&amp;" ++ quote(T);
quote([H|T])    -> [H|quote(T)];
quote([])       -> [].

    

