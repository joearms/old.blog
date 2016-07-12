-module(mymd_vsn2).
%% -compile(export_all).

-import(lists, [reverse/1, reverse/2]).

-export([make_blog/0,
	 get_line/2,
	 trim/1,
	 test_expand/0,
	 live/0,
	 watcher/0
	]).

live() ->
    make_blog(),
    spawn(fun() -> watcher() end).

make_blog() ->
    io:format("Running make_blog~n"),
    process("_inputs/????-??-??-*.org", "./", 
	    "index.html", "post.ehe"),
    process("_drafts/????-??-??-*.org", "./_draft_site/", 
	    "index_drafts.html", "draft_post.ehe").

process(Pattern, Site_dir, IndexFile, PostTemplate) ->
    Files = filelib:wildcard(Pattern),
    Files1 = lists:reverse(lists:sort(Files)),
    %% io:format("Files=~p~n",[Files1]),
    [convert_if_out_of_date0(I, Site_dir, PostTemplate) || I <- Files1],
    make_index_and_feed(Files1, Site_dir, IndexFile).
    
convert_if_out_of_date0(In, SiteDir, PostTemplate) ->
    case (catch convert_if_out_of_date(In, SiteDir, PostTemplate)) of
	{'EXIT', Why} ->
	    io:format("Error converting:~p~nReason:~p~n",[In, Why]);
	_ ->
	    ok
    end.

convert_if_out_of_date(In, SiteDir, PostTemplate) ->
    Out = SiteDir ++ outname(In),
    %% ok = filelib:ensure_dir(Path),
    %% io:format("Out=~p~n",[Out]),
    case out_of_date(In, Out) of
	true ->
	    convert(In, Out, PostTemplate);
	false ->
	    void
    end.

test_expand() ->
    %% expand("2016-04-23-New-Blog-Engine.org").
    Val = top_expand("_inputs/2016-05-27-Tiny-Test.org", #{}),
    io:format("Finally:~p~n",[Val]),
    must_write_file("final_tmp.html",[Val]),
    io:format("wrote final_tmp.html"),
    init:stop().

convert(In, Out, Template) ->
    io:format("converting In:~p~n",[In]),
    %% Out is something like 2016/04/23/File-Name.html
    Id = id(filename:rootname(Out)),
    OutDir = filename:dirname(Out) ++ "/",
    ok = filelib:ensure_dir(OutDir),
    Bin = top_expand(In, #{id => Id,
			   layout => Template,
			   outfile => Out}),
    must_write_file(Out, Bin).

id("./" ++ T) -> T;
id(X) -> X. 
    

top_expand(In, M0) ->
    M1 = expand(In, M0),
    iterate7(M1).

iterate7(#{layout := X} = M) ->
    %% io:format("iterate7 X=~p~n",[X]),
    File = "_templates/" ++ X,
    M1 = expand(File, M),
    case maps:get(layout, M1) of
	"$final" ->
	    maps:get(content, M1);
	_ ->
	    iterate7(M1)
    end.

expand(F, M) ->
    case (catch expand_org0(F, M)) of
	{'EXIT', Why} ->
	    io:format("Error expanding:~p~nReason:~p~n",[F, Why]);
	Ok ->
	    Ok
    end.

expand_org0(File, M0) ->
    {Header, Regions} = md_scan:file(File),
    K = lists:flatten([expand_region(I, M0) || I <- Regions]),
    M1 = add_to_map(K, Header),
    M2 = maps:merge(M0, M1),
    %% io:format("expand0 file:~p~nResult:~p~n",[File, M2]),
    M2.

add_to_map([H|T], M) -> add_to_map(T, maps:merge(H, M));
add_to_map([], M)     -> M.

%% expand_region({date, Type::string, Content::string}) -> [Map]
%% to be added to the global map

expand_region({data,"md:content",Str}, Map) ->
    Bind = [{'Env', Map}],
    Result = ehe_vsn5:expand_string(Str, Bind),
    {_Bind1, Stuff} = Result,
    %% io:format("after ehe expansion:~p~n",[Stuff]), 
    %% dump_bin(Stuff),
    %% header tags are #head_1 #head_2 etc
    Parse = parse_md_body(binary_to_list(Stuff)),
    {Parse1, Headers} = make_headers(Parse),
    HTML  = render_as_html(Parse1),
    Index = render_as_html(Headers),
    [#{content => HTML, right => Index}];
expand_region({data,"md:" ++ Tag,Str}, Map) ->
    Bind = [{'Env', Map}],
    Result = ehe_vsn5:expand_string(Str, Bind),
    {_Bind1, Stuff} = Result,
    %% io:format("after ehe expansion:~p~n",[Stuff]), 
    %% dump_bin(Stuff),
    Parse = parse_md_body(binary_to_list(Stuff)),
    HTML  = render_as_html(Parse),
    [#{list_to_atom(Tag) => HTML}];
expand_region({data,Tag,Str}, Map) ->
    Bind = [{'Env', Map}],
    %% Now convert to the internal forms used by ehe
    Result = ehe_vsn5:expand_string(Str, Bind),
    {_Bind1, Stuff} = Result, 
    [#{list_to_atom(Tag) => Stuff}].


make_headers(Parse) ->
    number_headers(Parse, 1, [], []).

number_headers([{h1,X}=H|T], N, L1, L2) ->
    Tag = "head_" ++ integer_to_list(N),
    number_headers(T, N+1, 
		   [H,{anchor,Tag}|L1],
		   [{linkTo,Tag,X}|L2]);
number_headers([H|T], N, L1, L2) ->
    number_headers(T, N, [H|L1], L2);
number_headers([], _, L1, L2) ->
    {reverse(L1), reverse(L2)}.

watcher() ->
    timer:sleep(5000),
    make_blog(),
    watcher().

outname([$_,$i,$n,$p,$u,$t,$s,$/,Y1,Y2,Y3,Y4,$-,M1,M2,$-,D1,D2,$-|File]) ->
    Path = [Y1,Y2,Y3,Y4,$/,M1,M2,$/,D1,D2,$/],
    Root = Path ++ filename:rootname(File),
    Root ++ ".html";
outname([$_,$d,$r,$a,$f,$t,$s,$/,Y1,Y2,Y3,Y4,$-,M1,M2,$-,D1,D2,$-|File]) ->
    Path = [Y1,Y2,Y3,Y4,$/,M1,M2,$/,D1,D2,$/],
    Root = Path ++ filename:rootname(File),
    Root ++ ".html".

%% Main API
%%  ehe_vsn5:expand_binary(Bin, Bin) -> {Bout, Bout}


trim(S) ->
    remove_leading_and_trailing_whitespace(S).
    
remove_leading_and_trailing_whitespace(X) -> 
    remove_leading_whitespace(remove_trailing_whitespace(X)).

remove_leading_whitespace([$\n|T]) -> remove_leading_whitespace(T);
remove_leading_whitespace([$\r|T]) -> remove_leading_whitespace(T);
remove_leading_whitespace([$\s|T]) -> remove_leading_whitespace(T);
remove_leading_whitespace([$\t|T]) -> remove_leading_whitespace(T);
remove_leading_whitespace(X) -> X.

remove_trailing_whitespace(X) ->
    reverse(remove_leading_whitespace(reverse(X))).

render_as_html(P) ->
    [render_line(I) || I <- P].

render_line({anchor,X}) ->
    ["<a name='",X,"'></a>"];

render_line({blockquote,P}) ->
    ["<blockquote>", render_inlines(P), "</blockquote>\n"];

render_line({ehe,_}=X) ->
    X;

render_line({para,P}) ->
    ["<p>", render_inlines(P), "</p>\n"];
render_line({pre, P}) ->
    ["<div class='pre'>", P, "</div>\n"];
render_line({code_block, erlang, Toks}) ->
    ["<div class='erlang'>",
     erl_toks_to_html(Toks),
     "</div>\n"];
render_line({fenced, _Tag, P}) ->
    ["<div class='pre'>", P, "</div>\n"];
render_line({h1, P}) ->
    ["<h1>", P, "</h1>\n"];
render_line({h2, P}) ->
    ["<h2>", P, "</h2>\n"];
render_line({item, P}) ->
    ["<li>", render_inlines(P), "</li>\n"];
render_line({linkTo,Tag,Text}) ->
    ["<a href='#",Tag,"'>", Text, "</a><br>"].

render_inlines(L) ->
    [render_inline(I) || I<- L].

render_inline({anchor,C}) ->
    ["<a name='", C, "'></a>"];
render_inline({code,C}) ->
    ["<span class='code'>", C, "</span>"];
render_inline({strike,C}) ->
    ["<strike>", C, "</strike>"];
render_inline({bold,C}) ->
    ["<b>", C, "</b>"];
render_inline({emph,C}) ->
    ["<span class='emph'>", C, "</span>"];
render_inline({url,{A,B}}) ->
    ["<a href='",A,"'>",B,"</a>"];
render_inline(C) when is_integer(C) ->
    C;
render_inline(start_quote) -> "&ldquo;";
render_inline(end_quote) ->   "&rdquo;".

parse_md_body(S) ->
    S0 = pass0(S, []),
    S1 = pass1(S0, []),
    S2 = pass2(S1, []),
    S3 = pass3(S2, []),
    pass4(S3).

%%----------------------------------------------------------------------
%% pass0 extracts <? ... ?>

pass0("<? " ++ T, L) ->
    {X, T1} = get_ehe(T, []),
    pass0(T1, [{ehe, X}|L]);
pass0([H|T], L) ->
    pass0(T, [H|L]);
pass0([], L) ->
    reverse(L).

get_ehe(" ?>" ++ T, L) ->
    {reverse(L), T};
get_ehe([H|T], L) ->
    get_ehe(T, [H|L]);
get_ehe([], L) ->
    exit({unexpected,eof,in,ehe,starting, reverse(L)}).

%%----------------------------------------------------------------------
%% pass1 extracts fenced blocks

pass1("\n```" ++ T, L) ->
    {Arg, T1}  = get_line(T, []),
    {Body, T2} = get_to_stop(T1, "```", []),
    Val = process_fenced(trim(Arg), Body),
    pass1(T2, [Val|L]);
pass1([H|T], L) ->
    pass1(T, [H|L]);
pass1([], L) ->
    reverse(L).

%%----------------------------------------------------------------------
%% pass2 extracts drawers 

pass2("\n#+BEGIN" ++ T, L) ->
    {Tag, T1} = get_line(T, []),
    Stop = "\n#+END" ++ Tag,
    %% io:format("Stop:~p~n",[Stop]),
    {Body, T2} = get_to_stop(T1, Stop, []),
    %% io:format("Body:~p~n",[Body]),
    pass2(T2, [{drawer, Tag, Body}|L]);
pass2([H|T], L) ->
    pass2(T, [H|L]);
pass2([], L) ->
    reverse(L).

%%----------------------------------------------------------------------
%% pass3 extracts one line h1 and h2
%% which start *... and **...  +

pass3("\n** " ++ T, L) ->
    {Arg, T1}  = get_line(T, []),
    pass3(T1, [{h2,Arg}|L]);
pass3("\n* " ++ T, L) ->
    {Arg, T1}  = get_line(T, []),
    pass3(T1, [{h1,Arg}|L]);
pass3("\n+ " ++ T, L) ->
    {Arg, T1}  = get_list_item(T),
    Parsed = scan_inlines(Arg, []),
    pass3(T1, [{item,Parsed}|L]);
pass3([H|T], L) ->
    pass3(T, [H|L]);
pass3([], L) ->
    reverse(L).

%%----------------------------------------------------------------------
%% pass4 collects sequence of integers

pass4([H|_] = A) when is_integer(H) ->
    {L1, L2} = lists:splitwith(fun is_int/1, A),
    scan_content(L1, []) ++ pass4(L2);
pass4([H|T]) ->
    [H|pass4(T)];
pass4([]) ->
    [].

%%----------------------------------------------------------------------
%% scan_content(Str, [])
%% Content has preformated (sets of lines starting with four blanks)
%% Paras separated blank lines

scan_content([], L) ->
    reverse(L);
scan_content("    " ++ _ = T, L) ->
    {Pre, T1} = scan_pre(T, []),
    scan_content(T1, [{pre, Pre}|L]);
scan_content(T, L) ->
    {Line, T1} = get_line(T, []),
    case is_blank(Line) of
	true  -> scan_content(T1, L);
	false ->
	    {Para, T2} = scan_para(T1, [$\n|reverse(Line)]),
	    Content = scan_inlines(Para, []),
	    Obj = para_or_blockquote(Content),
	    scan_content(T2, [Obj|L])
    end.

para_or_blockquote("> " ++ T) -> {blockquote, T};
para_or_blockquote(X)         -> {para, X}.

scan_pre("    " ++ T, L) ->
    {Line, T1} = get_line(T, []),
    scan_pre(T1, [$\n|reverse(Line,L)]); 
scan_pre(X, L) ->
    {reverse(L), X}.

scan_para([], L) ->
    {reverse(L), []};
scan_para(T, L) ->
    {Line, T1} = get_line(T, []),
    case is_blank(Line) of
	true ->
	    {reverse(L), T1};
	false ->
	    scan_para(T1, [$\n|reverse(Line,L)])
    end.

scan_inlines("<<" ++ T, L) ->
    {Code, T1} = get_anchor(T, []),
    scan_inlines(T1, [{anchor, Code}|L]);
scan_inlines("~~" ++ T, L) ->
    {Code, T1} = get_strike(T, []),
    scan_inlines(T1, [{strike, Code}|L]);
scan_inlines("``" ++ T, L) ->
    scan_inlines(T, [start_quote|L]);
scan_inlines("''" ++ T, L) ->
    scan_inlines(T, [end_quote|L]);
scan_inlines("`" ++ T, L) ->
    {Code, T1} = get_code(T, []),
    scan_inlines(T1, [{code, Code}|L]);
scan_inlines("__" ++ T, L) ->
    {Code, T1} = get_emph(T, []),
    scan_inlines(T1, [{emph, Code}|L]);
scan_inlines("**" ++ T, L) ->
    {Code, T1} = get_bold(T, []),
    scan_inlines(T1, [{bold, Code}|L]);
scan_inlines("[[" ++ T, L) ->
    {Inner, T1} = get_link(T, []),
    U = split_url(Inner, []),
    scan_inlines(T1, [{url, U}|L]);
scan_inlines([H|T], L) ->
    scan_inlines(T, [H|L]);
scan_inlines([], L) ->
    reverse(L).


get_anchor(">>" ++ T, L) -> {reverse(L), T};
get_anchor([H|T], L)     -> get_anchor(T, [H|L]);
get_anchor([], L)        -> {reverse(L), []}.

get_code("`" ++ T, L) -> {reverse(L), T};
get_code([H|T], L)    -> get_code(T, [H|L]);
get_code([], L)       -> {reverse(L), []}.

get_emph("__" ++ T, L) -> {reverse(L), T};
get_emph([H|T], L)     -> get_emph(T, [H|L]);
get_emph([], L)        -> {reverse(L), []}.

get_bold("**" ++ T, L) -> {reverse(L), T};
get_bold([H|T], L)     -> get_bold(T, [H|L]);
get_bold([], L)        -> {reverse(L), []}.

get_strike("~~" ++ T, L) -> {reverse(L), T};
get_strike([H|T], L)     -> get_strike(T, [H|L]);
get_strike([], L)        -> {reverse(L), []}.
    
get_link("]]" ++ T, L) -> {reverse(L), T};
get_link([H|T], L)     -> get_link(T, [H|L]);
get_link([], L)        -> {reverse(L), []}.
    
split_url("][" ++ T, L) -> {reverse(L), T};
split_url([H|T], L)     ->  split_url(T, [H|L]);
split_url([], L)        -> {reverse(L), reverse(L)}.

is_int(X) when is_integer(X) -> true;
is_int(_) -> false. 

get_to_stop([H|T] = X, Stop, L) -> 
    case is_stop(X, Stop) of
	{yes, X1} -> {reverse(L), X1};
	no -> get_to_stop(T, Stop, [H|L])
    end;
get_to_stop([], Stop, L) ->
    io:format("** Fatal EOF
Searching for stop symbol:~p~n Clue:~p~n",[Stop,lists:reverse(L)]),
    exit({eBadInput,noBacktickStopSymbol}).

is_stop(X, []) ->
    {Line, X1} = get_line(X, []),
    case is_blank(Line) of
	true ->
	    void;
	false ->
	    io:format("*** Warning non bank after stop~p:~n",[Line])
    end,
    {yes, X1};
is_stop([H|T], [H|T1]) ->
    is_stop(T, T1);
is_stop(_, _) ->
    no.

is_blank([$\s|T]) -> is_blank(T);
is_blank([])      -> true;
is_blank(_)       -> false.
    
%% get_line always consumes the nl

get_line("\n" ++ T, L) -> {reverse(L), T};
get_line([H|T], L)     -> get_line(T, [H|L]); 
get_line([],L)         -> {reverse(L), []}. 

out_of_date(In, Out) ->
    %% io:format(" IN:~p~nOUT:~p~n",[In,Out]),
    case filelib:is_file(In) of
	true ->
	    case filelib:is_file(Out) of
		true ->
		    %% check the time stamps
		    Tsrc  = filelib:last_modified(In),
		    Tdest = filelib:last_modified(Out),
		    if Tsrc > Tdest -> true;
		       true         -> false
		    end;
		false ->
		    %% no output so we have to recompile
		    true
	    end;
	false ->
	    %% error input cannot be found
	    %% not sure why this would be called
	    exit({out_of_date,no_input,In})
    end.

make_index_and_feed(Files, Site, Dest) ->
    L1 = [begin
	      Link = Site ++ outname(I),
	      Title = must_get_title_from_file(I),
	      Date = date_from_filename(I),
	      {Link, Title, Date}
	  end || I <- Files],
    L2= [
	 ["<li>",Date," &raquo; <a href='",Link,"'>", Title,"</a></li>\n"]
	 || {Link,Title,Date} <- L1
	],
    L3 = iolist_to_binary(L2),
    %% io:format("making index L1=~p~~n",[L1]),
    %% file:write_file("index.html", [L1]).
    Val = top_expand("_templates/index.ehe", #{content => L3}),
    must_write_file(Dest, Val),
    io:format("~s created~n", [Dest]),
    %% XML Feed
    make_feed(Site, L1).

fix_link("./", "./" ++ Link) ->
    "http://joearms.github.io/" ++ Link;
fix_link(_, Link) ->
    Link.

make_feed(Site, L1) ->
    L2 = [{fix_link(Site,Link), Title} || {Link, Title,_} <- L1],
    Items = [
	  ["<item>\n",
	   "  <title>", Title, "</title>\n",
	   "  <link>", Link, "</link>\n",
	   "  <description>", Title, "</description>\n",
	   "</item>\n"] ||
	     {Link,Title} <- L2
	 ],
    L5 = ["<?xml version='1.0' encoding='UTF-8' ?>
<rss version='2.0'>
  <channel>
   <title>Joe Armstrong - Erlang and other stuff</title>
     <link>http://joearms.github.io/index.html</link>
     <description>Miscellaneous writing</description>\n",
	  Items,"
  </channel>
</rss>
"],
    L6 = iolist_to_binary(L5),
    Feed = Site ++ "feed.xml",
    must_write_file(Feed, L6),
    io:format("~s created~n", [Feed]).

%% <?xml version="1.0" encoding="UTF-8" ?>
%% <rss version="2.0">

%% <channel>
%%   <title>W3Schools Home Page</title>
%%   <link>http://www.w3schools.com</link>
%%   <description>Free web building tutorials</description>
%%   <item>
%%     <title>RSS Tutorial</title>
%%     <link>http://www.w3schools.com/xml/xml_rss.asp</link>
%%     <description>New RSS tutorial on W3Schools</description>
%%   </item>
%%   <item>
%%     <title>XML Tutorial</title>
%%     <link>http://www.w3schools.com/xml</link>
%%     <description>New XML tutorial on W3Schools</description>
%%   </item>
%% </channel>

%% </rss>


 
date_from_filename("_inputs/" ++ T) -> date_from_filename1(T);
date_from_filename("_drafts/" ++ T) -> date_from_filename1(T).

date_from_filename1([Y1,Y2,Y3,Y4,$-,M1,M2,$-,D1,D2,$-|_]) ->
    Day = [D1,D2],
    Year = [Y1,Y2,Y3,Y4],
    Month = monthname([M1,M2]),
    [Day,$\s,Month,$\s,Year,$\s].


monthname("01") -> "Jan";
monthname("02") -> "Feb";
monthname("03") -> "Mar";
monthname("04") -> "Apr";
monthname("05") -> "May";
monthname("06") -> "Jun";
monthname("07") -> "Jul";
monthname("08") -> "Aug";
monthname("09") -> "Sep";
monthname("11") -> "Nov";
monthname("12") -> "Dec".

process_fenced("erlang", Body) ->
    Toks = color_erlang:scan_string(Body),
    {code_block,erlang, Toks};
process_fenced(Tag, Body) ->
    {fenced, Tag, Body}.

erl_toks_to_html(L) ->
    [span(Tag, Val) || {Tag, Val} <- L].

span(A, V) when is_atom(A) ->
    ["<span class='",a2s(A),"'>",V,"</span>"].

a2s(A) ->
    atom_to_list(A).

count_blanks([$\s|T], N) -> count_blanks(T, N+1);
count_blanks(X, N)       -> {N, X}. 
    

get_list_item(Str) ->
    {Level, Str1} = count_blanks(Str, 2),
    %% io:format("Level=~p~n",[Level]),
    {Line, Str2} = get_line(Str1, []),
    get_list_item(Level, Str2, reverse(Line)).

get_list_item(_Level, "+" ++ _ = S, L) ->
    {reverse(L), [$\n|S]};
get_list_item(Level, Str, L) ->
    case is_blank(Str) of
	true ->
	    {reverse(L), Str};
	false ->
	    {Line, Str1} = get_line(Str, []),
	    {C, Str2} = count_strip_blanks(Line, 0),
	    %% io:format("Count=~p~n",[C]),
	    if
		C == Level ->
		    get_list_item(Level, Str1, reverse(Str2, [$\s|L]));
		true ->
		    {reverse(L), Str}
	    end
    end.
	    
count_strip_blanks([$\s|T], N) ->
    count_strip_blanks(T, N+1);
count_strip_blanks(T, N) ->
    {N, T}.

must_write_file(File, Bin) ->
    case file:write_file(File, Bin) of
	ok -> ok;
	{error, Why} -> exit({cannot_write_file,File,reason,Why})
    end.

%%----------------------------------------------------------------------
%% musts

must_read_file(F) ->
    case file:read_file(F) of
	{ok, Bin}  -> Bin;
	{error, Why} -> exit({fatal,no_file,F,Why})
    end.

must_get_title_from_file(F) ->
    Bin = must_read_file(F),
    must_get_title(binary_to_list(Bin), F).

must_get_title("title:" ++ T, _) ->
    {Line, _} = get_line(T, []),
    trim(Line);
must_get_title([_|T], F) ->
    must_get_title(T, F);
must_get_title([], F) ->
    exit({file_has_no_title, F}).

