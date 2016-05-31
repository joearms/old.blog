-module(md_scan).
%% -compile(export_all).
-export([test/0, file/1]).
-import(lists, [reverse/1]).

test() ->
    file("../_inputs/2016-04-23-New-Blog-Engine.org").

file(F) ->
    {ok, Bin} = file:read_file(F),
    scan_header(binary_to_list(Bin), #{}).

%% when scan_header is called a nl has just been consumed

scan_header("@" ++ _ = A, M) ->
    L = scan_body("\n" ++ A, []),
    {M, L};
scan_header(X, M) ->
    {Line, X1} = mymd_vsn2:get_line(X, []),
    %% io:format("Line=~p~n",[Line]),
    {Tag, Arg} = split_tag(Line, []),
    scan_header(X1, maps:put(Tag, Arg, M)).

scan_body("\n@" ++ T, L) ->
    {Line, T1} = mymd_vsn2:get_line(T, []),
    {Data, T2} = grab_data(T1, []),
    %% io:format("Data with tag:~p found~n",[Line]),
    scan_body(T2, [{data,Line,Data}|L]);
scan_body([], L) ->
    reverse(L).

grab_data("\n@" ++ _ = T, L) -> {reverse(L), T};
grab_data([H|T], L)          -> grab_data(T, [H|L]);
grab_data([], L)             -> {reverse(L), []}.
    
split_tag(":"++ T, L) -> {list_to_atom(reverse(L)), mymd_vsn2:trim(T)};
split_tag([H|T], L)   -> split_tag(T, [H|L]).

