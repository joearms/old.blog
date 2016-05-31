-module(simple_server_lib).

-export([
	 bin_to_websocket_frame/1,
	 classify/1,
	 deframe/1, 
	 list_dir/1,
	 make_response/2,
	 make_websocket_handshake/1,
	 normalise_args/1,
	 parse_request/2, 
	 parse_uri_args/1,
	 parse_uri/1,
	 pre/1, 
	 quote/1
	]).

-import(lists, [reverse/1]).


%%----------------------------------------------------------------------    

parse_request({more,{Req,N,B0}}, B1) ->
    Bin2 = <<B0/binary, B1/binary>>,
    collect_request_content(N, Req, Bin2);
parse_request(Bin0, Bin1) ->
    Bin2 = <<Bin0/binary, Bin1/binary>>,
    case binary:split(Bin2, <<"\r\n\r\n">>) of
	[Before, After] ->
	    {Cmd, Headers} =  parse_request_cmd(Before),
	    case maps:find('Content-Length', Headers) of
		error ->
		    %% we have a complete
		    %% request
		    Req =  {request, Cmd, Headers, <<>>},
		    {ok, Req, After};
		{ok, Val} ->
		    N = b2i(Val),
		    Req =  {request2, Cmd, Headers},
		    collect_request_content(N, Req, After)
	    end;
	[_] ->
	    Bin2
    end.

parse_request_cmd(Bin) ->
    [Line|Parts] = binary:split(Bin, <<"\r\n">>, [global]),
    Cmd = string:tokens(binary_to_list(Line)," "),
    %% io:format("Cmd::~p~n",[Cmd]),
    Headers = parse_request_headers(Parts),
    %% io:format("Headers::~p~n",[Headers]),
    {Cmd, Headers}.

parse_request_headers(L) ->
    maps:from_list([parse_header(I) || I<- L]).

parse_header(B) ->
    [Key,Val] = binary:split(B, <<": ">>),
    {b2a(Key), Val}.

collect_request_content(N, {request2,Cmd,Headers}, Bin0) when size(Bin0) >= N ->
    {B1, B2} = split_binary(Bin0, N),
    {ok, {request,Cmd,Headers,B1}, B2};
collect_request_content(N, Req, Bin0) ->
    {more, {Req,N,Bin0}}.


classify(FileName) ->
    case string:to_lower(filename:extension(FileName)) of
	".gif"  -> gif;
	".jpg"  -> jpg;
	".jpeg" -> jpg;
	".css"  -> css;
	".js"   -> js;
	".svg"   -> svg;
	".xul"  -> xul;
	".html" -> html;
	".xhtml" -> xhtml;
	".htm"  -> html;
	_       -> html
    end.

content_type(X) ->
    ["Content-Type: ", X, "\r\n"].

pre(X) ->
    list_to_binary(["<pre>\n",quote(lists:flatten(io_lib:format("~p",[X]))), "</pre>"]).

make_response(Tag, B1) ->
    Len = size(B1),
    Mime = mime_type(Tag),
    ["HTTP/1.1 200 Ok\r\n", content_type(Mime),
	      "Content-Length: ", integer_to_list(Len), "\r\n\r\n",
	      B1].


mime_type(gif)               -> "image/gif";
mime_type(jpg)               -> "image/jpeg";
mime_type(png)               -> "image/png";
mime_type(css)               -> "text/css";
mime_type(json)              -> "application/json";
mime_type(swf)               -> "application/x-shockwave-flash";
mime_type(html)              -> "text/html";
mime_type(xhtml)             -> "application/xhtml+xml";
mime_type(xul)               -> "application/vnd.mozilla.xul+xml";
mime_type(js)                -> "application/x-javascript";
mime_type(svg)               -> "image/svg+xml";
mime_type(X) when is_atom(X) -> mime_type(html);
mime_type(FileName)          -> mime_type(classify(FileName)).



quote("<" ++ T) -> "&lt;" ++ quote(T);
quote("&" ++ T) -> "&amp;" ++ quote(T);
quote([H|T]) -> [H|quote(T)];
quote([]) -> [].

make_websocket_handshake(Headers) ->
    %% io:format("websocket connect Headers=~p~n",[Headers]),
    {ok, SecWebSocketKey} = maps:find('Sec-WebSocket-Key',Headers),
    Sha1 = crypto:hash(sha,[SecWebSocketKey, 
			    <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>]),
    Base64 = base64:encode(Sha1),
    Handshake = [
		 <<"HTTP/1.1 101 Switching Protocols\r\n">>,
		 <<"Upgrade: websocket\r\n">>,
		 <<"Connection: Upgrade\r\n">>,
		 <<"Sec-WebSocket-Accept: ">>, Base64, <<"\r\n">>,
		 <<"\r\n">>
		],
    list_to_binary(Handshake).

deframe(<<Fin:1,_Rsv:3,Opcode:4,Mask:1,
	  PayloadLength:7, Rest/binary>>) ->
    Do = {Fin, Opcode},
    case PayloadLength of
	N when N =< 125 ->
	    %% we're done
	    %% no additonal length
	    deframe1(Do, Mask, N, Rest);
	126 ->
	    %% Next 16 bits is the length
	    <<K:16,B1/binary>> = Rest,
	    deframe1(Do, Mask, K, B1);
	127 ->
	    <<K:64,B1/binary>> = Rest,
	    deframe1(Do, Mask, K, B1)
    end.

deframe1(Do, 0, K, B) ->
    %% no mask
    deframe2(Do, no, K, B);
deframe1(Do, 1, Len, <<Mask:4/binary, B1/binary>>) ->
    io:format("Len ~p size:~p Mask:~p B1:~p~n",[Len,size(B1), Mask, B1]),
    deframe2(Do, {yes, Mask}, Len, B1).

deframe2({1,1}, Mask, _Len, Data) ->
    %% final text frame
    Bin = unmask(Data, Mask),
    unicode:characters_to_list(Bin);
deframe2(Do, Mask, Len, Data) ->
    io:format("deframe2: do:~p Mask:~p Length:~p Data:~p~n",
	      [Do, Mask, Len, size(Data)]),
    exit({deframe2,Do}).

unmask(Data, no) ->
    Data;
unmask(Data, {yes, Mask}) ->
    M = binary_to_list(Mask),
    unmask(binary_to_list(Data), M, M, []).

unmask([],_,_,L)              -> reverse(L);
unmask(X, [], M, L)           -> unmask(X, M, M, L);
unmask([H1|T1], [H2|T2], M,L) -> unmask(T1, T2, M, [H1 bxor H2|L]).

bin_to_websocket_frame(Bin) ->
    Size = size(Bin),
    if Size < 126 ->
	    <<1:1, 0:3, 1:4, 0:1, Size:7, Bin/binary>>;
       true ->
	    io:format("*** too large frame !!!!~n"),
	    exit(large)
    end.

normalise_args(Body) ->
    {struct,L1} = mochijson2:decode(binary_to_list(Body)),
    L2 = [{b2a(K),V} || {K,V} <- L1],
    maps:from_list(L2).

b2a(B) ->
    list_to_atom(binary_to_list(B)).

b2i(B) ->
    list_to_integer(binary_to_list(B)).


parse_uri(URI) ->
    case string:tokens(URI, "?") of
	[Root] ->
	    {Root, []};
	[Root, Args] ->
	    {Root, parse_uri_args(Args)}
    end.

parse_uri_args(Args) ->
    Args1 = string:tokens(Args, "&;"),
    L1 = lists:map(fun(KeyVal) ->
			   case string:tokens(KeyVal, "=") of
			       [Key, Val] ->
				   {list_to_atom(urlencoded2str(Key)), 
				    list_to_binary(urlencoded2str(Val))};
			       [Key] ->
				   {list_to_atom(urlencoded2str(Key)), <<>>};
			       _ ->
				   io:format("Invalid str:~p~n",[KeyVal]),
				   {error, <<"error">>}
		      end
	      end, Args1),
    maps:from_list(L1).

urlencoded2str([$%,$u,A,B,C,D|T]) -> [decode_hex(A,B,C,D)|urlencoded2str(T)];
urlencoded2str([$%,Hi,Lo|T])      -> [decode_hex(Hi, Lo)|urlencoded2str(T)];
urlencoded2str([$+|T])            -> [$ |urlencoded2str(T)];
urlencoded2str([H|T])             -> [H|urlencoded2str(T)];
urlencoded2str([])                -> [].

%% decode_hex ...

decode_hex(Hex1, Hex2) -> hex2dec(Hex1)*16 + hex2dec(Hex2).

decode_hex(Hex1, Hex2, Hex3, Hex4) -> 
    hex2dec(Hex1)*4096 + hex2dec(Hex2)*256 + hex2dec(Hex3)*16 + hex2dec(Hex4).

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

list_dir(D) ->
    {ok, L} = file:list_dir(D),
    L2 = [modify(D, I) || I <- L],
    L1 = [["<li>",a(I, I),"\n"] ||  I<- L2],
    make_response(html, list_to_binary(L1)).

modify(D,I) ->
    F = filename:join(D,I),
    case filelib:is_dir(F) of
	true ->
	    I ++ "/";
	false ->
	    I
    end.

a(I,I) ->
    ["<a href='",I,"'>",I,"</a>"].
