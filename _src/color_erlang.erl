-module(color_erlang).
-compile(export_all).

scan_string(Str) ->
    {ok, Toks,_} = erlide_scan:string_ws(Str),
    [fix(xform(I)) || I <- Toks].

string2latex(S) ->
    Toks = scan_string(S),
    Latex = [to_latex(I) || I <- Toks],
    %% io:format("L=~p~n",[lists:zip(L,L1)]),
    Latex.

xform({Tag,_,_,A}) -> {Tag, A};
xform({Tag,_,A})   -> {Tag, A};
xform({P,_Y})       -> 
    case reserved_word(P) of
	true ->
	    {keyword,P};
	false ->
	    {punc, P}
    end.

fix({punc,dot}) -> {punc,"."};
fix({Tag,A}) when is_atom(A) -> {Tag, atom_to_list(A)};
fix(X)                       -> X.

reserved_word('case') -> true;
reserved_word('if') -> true;
reserved_word('receive') -> true;
reserved_word('end') -> true;
reserved_word('try') -> true;
reserved_word('catch') -> true;
reserved_word(_) ->  false.

to_latex({keyword,X}) ->
    %% ["(*@{\\bf ",X,"}@*)"];
    X;
to_latex({ws,X}) -> X;
to_latex({Tag,X}) -> 
    ["(*@\\textcolor{",atom_to_list(Tag),"color}{",latex_quote(X),"}@*)"].

    
latex_quote("{" ++ T) -> "\\{" ++ latex_quote(T);
latex_quote("}" ++ T) -> "\\}" ++ latex_quote(T);
latex_quote("%" ++ T) -> "\\%" ++ latex_quote(T);
latex_quote("_" ++ T) -> "\\_" ++ latex_quote(T);
latex_quote("$" ++ T) -> "\\$" ++ latex_quote(T);
latex_quote("#" ++ T) -> "\\#" ++ latex_quote(T);
latex_quote("~" ++ T) -> "\\textasciitilde{}" ++ latex_quote(T);
latex_quote("\\" ++ T) -> "$\\backslash$" ++ latex_quote(T);
latex_quote([H|T])    -> [H|latex_quote(T)];
latex_quote([])       ->  [].
