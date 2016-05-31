-module(webserver_vsn10).
-compile(export_all).

batch() ->
    %% start 3 register processes
    %% midi_event_gen:start(),
    %% midi_test:send_notes(),
    %% midi_composer:start(),
    %% page_register:start(),
    start(),
    receive
    after
	infinity ->
	    stop
    end.

start() ->
    Info = state,
    F = fun(Client) -> start(Client, Info) end,
    webserver_core_vsn10:start_link(test1, 8080, F).

stop() ->
    webserver_core_vsn10:stop(test1).

start(Client, Info) -> 
    io:format("server starting ~p ~p~n",[Client, Info]),
    loop(1, Client).

remap("/app/" ++ X) ->
    X1 = "/onlinesequencer-master/app/" ++ X,
    io:format("** map:~p => ~p~n",[X,X1]),
    X1;
remap(X) ->
    X.

do_json_rpc(#{mod:=Mod,func :=Func}, Map) ->
    AMod  = list_to_atom(Mod),
    AFunc = list_to_atom(Func),
    case (catch apply(AMod, AFunc, [Map])) of
	{'EXIT', Why} ->
	    io:format("*** oops do_json_rpc:~p~n => ~p~n",[{AMod,AFunc,Map}, Why]),
	    {html, <<"<h1>error</h1>">>};
	Ok ->
	    Ok
    end.
		   
do_cgi({_,#{mod:=Mod,func :=Func}=X}, Body) ->
    AMod = list_to_atom(Mod),
    AFunc = list_to_atom(Func),
    case (catch apply(AMod, AFunc, [X, Body])) of
	{'EXIT', Why} ->
	    io:format("oops:~p~n => ~p~n",[{AMod,AFunc,[X]}, Why]),
	    {html, <<"<h1>error</h1>">>};
	Ok ->
	    Ok
    end.

loop(N, Client) ->
    receive
	{Client,{request,["GET", "/cgi?"++_=URI,_], _Headers, Body}} ->
	    io:format("GET CGI ~p ~p~n",[URI,Body]),
	    A = parse_uri(URI),
	    io:format("Args=~p~n",[A]),
	    {Type, Val} = do_cgi(A, <<>>),
	    %% io:format("response:~p~n",[{Type,Val}]),
	    Response = make_response(Type, Val),
	    Client ! {send, Response},
	    loop(N+1, Client);
	
	%% JSON RPC
	%% Body contains an arbirary term
	%% URI contains mod and fuunc
	
	{Client,{request,["POST", "/json_rpc?"++ URI,_], _Headers, Body}} ->
	    io:format("POST CGI ~p ~p~n",[URI,Body]),
	    A = parse_uri_args(URI),
	    io:format("Args=~p~n",[A]),
	    Map = bin_to_map(Body),
	    {Type, Val} = do_json_rpc(A, Map),
	    %% io:format("response:~p~n",[{Type,Val}]),
	    Response = make_response(Type, Val),
	    Client ! {send, Response},
	    loop(N+1, Client);


	{Client,{request, ["POST", "/cgi",_], _Headers, Body}} ->
	    io:format("POST CGI ~p~n",[Body]),
	    A = bin_to_map(Body),
	    io:format("A=~p~n",[A]),
	    Res = compute(A),
	    io:format("res=~p~n",[Res]),
	    Client ! {send, make_response(html, Res)},
	    loop(N+1, Client);

	{Client,{request, ["POST", "/cgi?"++_=URI,_], _Headers, Body}} ->
	    io:format("POST CGI ~p body size:~p ~n",[URI, size(Body)]),
	    A = parse_uri(URI),
	    io:format("A=~p~n",[A]),
	    {Type, Val} = do_cgi(A, Body),
	    %% io:format("response:~p~n",[{Type,Val}]),
	    Response = make_response(Type, Val),
	    Client ! {send, Response},
	    loop(N+1, Client);

	{Client,{request, ["GET", File0,_], _Headers, _Body}} ->
	    io:format("Here GET:~s~n",[File0]),
	    File = remap(File0),
	    File1 = "." ++ File,
	    io:format("get:~p ~p => ~p~n",[N, File, File1]),
	    Reply = case is_expandable(File1) of
			{yes, Mod} ->
			    Mod:expand(File1);
			no ->
			    fetch_file_or_list_dir(File1)
		    end,
            %% io:format("Reply:~p~n",[Reply]),
            Client ! {send, Reply},
     	    loop(N+1, Client);
	Any ->
	    io:format("client got:~p ~p~n",[Client, Any]),
	    loop(N+1, Client)
    end.

fetch_file_or_list_dir(File1) ->
    case file:read_file(File1) of
	{ok, Bin} ->
	    make_response(classify(File1), Bin);
	{error, _} ->
	    case filelib:is_dir(File1) of
		true ->
		    simple_server_lib:list_dir(File1);
		false ->
		    io:format("*** NO FILE:~p~n",[File1]),
		    make_response(html, 
				  io_list(
				    ["<h1>no file</h1><p>",
				     File1,
				     "<p><a href='/index.html'>index</a>"]))
	    end
    end.


make_response(Tag, B1) ->
    Len = size(B1),
    Mime = mime_type(Tag),
    ["HTTP/1.1 200 Ok\r\n", content_type(Mime),
	      "Content-Length: ", integer_to_list(Len), "\r\n\r\n",
	      B1].
    
    
io_list(I) ->
    list_to_binary(I).


compute(#{func := <<"fac">>, arg := B}) ->
    N = list_to_integer(binary_to_list(B)),
    R = fac(N),
    list_to_binary(integer_to_list(R)).

fac(0) -> 1;
fac(N) -> N*fac(N-1).
 
bin_to_map(Bin) ->
    {struct,L1} = mochijson2:decode(binary_to_list(Bin)),
    L2 = [{b2a(K),V} || {K,V} <- L1],
    maps:from_list(L2).

b2a(B) ->
    list_to_atom(binary_to_list(B)).


mime_type(css)               -> "text/css";
mime_type(gif)               -> "image/gif";
mime_type(html)              -> "text/html";
mime_type(jpg)               -> "image/jpeg";
mime_type(js)                -> "application/x-javascript";
mime_type(json)              -> "application/json";
mime_type(mp3)               -> "audio/mp3";
mime_type(plain)             -> "text/plain";
mime_type(png)               -> "image/png";
mime_type(svg)               -> "image/svg+xml";
mime_type(swf)               -> "application/x-shockwave-flash";
mime_type(wav)               -> "audio/x-wav";
mime_type(xhtml)             -> "application/xhtml+xml";
mime_type(xul)               -> "application/vnd.mozilla.xul+xml";
mime_type(xml)               -> "application/xml";
mime_type(X) when is_atom(X) -> mime_type(html);
mime_type(FileName)          -> mime_type(classify(FileName)).

classify(FileName) ->
    case string:to_lower(filename:extension(FileName)) of
	".css"  -> css;
	".gif"  -> gif;
	".htm"  -> html;
	".html" -> html;
	".jpeg" -> jpg;
	".jpg"  -> jpg;
	".js"   -> js;
	".mp3"  -> mp3;
	".svg"  -> svg;
	".wav"  -> wav;
	".xhtml" -> xhtml;
	".xml"   -> xml;
	".xul"  -> xul;
	_       -> plain
    end.

content_type(X) ->
    ["Content-Type: ", X, "\r\n"].

pre(X) ->
    ["<pre>\n",quote(lists:flatten(io_lib:format("~p",[X]))), "</pre>"].


quote("<" ++ T) -> "&lt;" ++ quote(T);
quote("&" ++ T) -> "&amp;" ++ quote(T);
quote([H|T]) -> [H|quote(T)];
quote([]) -> [].

s2a(L) ->
    list_to_atom(L).

is_expandable(File) ->
    case filename:extension(File) of
	".php" -> {yes, expand_php};
	".org" -> {yes, expand_org};
	_ -> no
    end.
		      
	    
%% A typical URI looks
%% like
%% URI = "/a/b/c?password=aaa&invisible=Ahidden+value"+

parse_uri(URI) ->
    case string:tokens(URI, "?") of
	[Root] ->
	    {Root, []};
	[Root, Args] ->
	    {Root, parse_uri_args(Args)}
    end.

parse_uri_args(Args) ->
    Args1 = string:tokens(Args, "&;"),
    Args2 = lists:map(fun(KeyVal) ->
			      case string:tokens(KeyVal, "=") of
				  [Key, Val] ->
				      {urlencoded2str(Key), urlencoded2str(Val)};
				  [Key] ->
				      {urlencoded2str(Key), ""};
				  _ ->
				      io:format("Invalid str:~p~n",[KeyVal]),
				      {"error", "error"}
			      end
		      end, Args1),
    Args3 = [{list_to_atom(I),J} || {I,J} <- Args2],
    maps:from_list(Args3).

urlencoded2str([$%,Hi,Lo|T]) -> [decode_hex(Hi, Lo)|urlencoded2str(T)];
urlencoded2str([$+|T])       -> [$\s|urlencoded2str(T)];
urlencoded2str([H|T])        -> [H|urlencoded2str(T)];
urlencoded2str([])           -> [].


%% decode_hex ...

decode_hex(Hex1, Hex2) ->
    hex2dec(Hex1)*16 + hex2dec(Hex2).

hex2dec(X) when X >=$0, X =<$9 -> X-$0;
hex2dec($A) -> 10;
hex2dec($B) -> 11;
hex2dec($C) -> 12;
hex2dec($D) -> 13;
hex2dec($E) -> 14;
hex2dec($F) -> 15;
hex2dec($a) -> 10;
hex2dec($b) -> 11;
hex2dec($c) -> 12;
hex2dec($d) -> 13;
hex2dec($e) -> 14;
hex2dec($f) -> 15.


