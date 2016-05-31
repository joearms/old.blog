-module(webserver_core_vsn10).
-compile(export_all).
-import(lists, [reverse/1]).

 -behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

%% -record(state, {}).

start_link(Name, Port, Fun) ->
    gen_server:start_link({local, Name}, ?MODULE, [Port,Fun], []).

stop(Name) ->
    gen_server:call(Name, stop).

init([Port,Fun]) ->
    process_flag(trap_exit,true),
    S = self(),
    spawn_link(fun() -> start_port(S, Port, Fun) end),
    {ok, 1}.

handle_call(stop, _, _) ->
    {stop,yes,deliberate};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    io:format("web_server3: info:~p~n",[{Info,State}]),
    {noreply, State}.

terminate(Reason, State) ->
    io:format("web_server3: terminate:~p ~p~n",[Reason,State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

start_port(GenServer, Port, Fun) ->
    {ok, Listen} = gen_tcp:listen(Port, [{packet,0}, 
					 binary,
					 {reuseaddr,true},
					 {active, true}]),
    %% io:format("My IDDR:~p~n",[my_ip()]),
    io:format("Got a listener on port:~p~n",[Port]),
    spawn(fun() -> par_connect(GenServer, Listen, Fun) end),
    receive
    after infinity ->
	    true
    end.

my_ip() ->
    ok(fun() ->
	       {ok,L} = inet:getifaddrs(),
	       {value,{_,L1}} = lists:keysearch("en0", 1, L),
	       [Addr] = [{A,B,C,D} || {addr,{A,B,C,D}} <- L1],
	       Addr
       end, unknown).

ok(F,Bad) ->
    case (catch F()) of
	{'EXIT', Why} ->
	    io:format("* error~p~n",[Why]),
	    Bad;
	Ok ->
	    Ok
    end.

par_connect(GenServer, Listen, Fun) ->
    io:format("par_connect me to:~p~n",[GenServer]),
    link(GenServer), %% so we die if the gen_server dies
    io:format("waiting for a connection Listen:~p~n",[Listen]),
    case gen_tcp:accept(Listen) of
	{ok, Socket} ->
	    io:format("starting a new accept socket=~p~n",[Socket]),	    
	    spawn(fun() -> par_connect(GenServer, Listen, Fun) end),
	    S = self(),
	    Pid = spawn_link(fun() -> Fun(S) end),
	    mm(Socket, Pid, <<>>);
	Other ->
	    io:format("par_connect:Other=~p ~p~n",
		      [Other,erlang:get_stacktrace()])
    end.

mm(Socket, Pid, Bin0) ->
    receive
	{tcp, Socket, Bin1} ->
	    Bin2 = <<Bin0/binary, Bin1/binary>>,
	    case binary:split(Bin2, <<"\r\n\r\n">>) of
		[Before, After] ->
		    {Cmd, Headers} =  parse_request(Before, After),
		    case maps:find('Content-Length', Headers) of
			error ->
			    %% we have a complete
			    %% request
			    %% Req =  {request, Cmd, Headers, After},
			    do_request(Cmd, Headers, <<>>, Pid, Socket, After);
			{ok, Val} ->
			    N = b2i(Val),
			    {Body, Rest} = collect_content(Socket, Pid, N, After),
			    do_request(Cmd, Headers, Body, Pid, Socket, Rest)
		    end;
		_ ->
		    mm(Socket, Pid, Bin2)
	    end;
	{send, Bin} ->
	    gen_tcp:send(Socket, Bin),
	    mm(Socket, Pid, Bin0);
	Any ->
	    io:format("received:~p ~p ~p~n",
		      [Socket,Pid,Any]),
	    mm(Socket, Pid, Bin0)
    end.

do_request(["GET","/websocket/" ++ Mod,_], 
	   Headers, _Body, _Pid, Socket, _Then) ->
    %% upgrade
    connect(Socket, Mod, Headers);
do_request(Cmd, Headers, Body, Pid, Socket, Then) ->
    Pid ! {self(), {request, Cmd, Headers, Body}},
    mm(Socket, Pid, Then).


collect_content(Socket, Pid, N, Bin0) ->
    %% io:format("in collect content N=~p size=~p~n",[N,size(Bin0)]),
    case size(Bin0) of
	K when K >= N ->
	    split_binary(Bin0, K);
	_ ->
	    receive
		{tcp, Socket, Bin1} ->
		    Bin2 = <<Bin0/binary, Bin1/binary>>,
		    collect_content(Socket, Pid, N, Bin2);
		{send, Bin} ->
		    %% io:format("Send:~p~n",[Bin]),
		    gen_tcp:send(Socket, Bin),
		    collect_content(Socket, Pid, N, Bin0);
		Other ->
		    io:format("unexpect message here32:~p~n",[Other]),
		    collect_content(Socket, Pid, N, Bin0)
	    end
    end.

parse_request(Bin, _After) ->
    [Line|Parts] = binary:split(Bin, <<"\r\n">>, [global]),
    Cmd = string:tokens(binary_to_list(Line)," "),
    io:format("Cmd::~p~n",[Cmd]),
    Headers = parse_headers(Parts),
    %% io:format("Headers::~p~n",[Headers]),
    {Cmd, Headers}.

parse_headers(L) ->
    maps:from_list([parse_header(I) || I<- L]).

parse_header(B) ->
    [Key,Val] = binary:split(B, <<": ">>),
    {b2a(Key), Val}.

b2a(B) ->
    list_to_atom(binary_to_list(B)).

connect(Socket, Path, Headers) ->
    io:format("websocket connect Headers=~p~n",[Headers]),
    %% {ok, Origin} = maps:find('Origin', Headers),
    %% {ok, Host} = maps:find('Host', Headers),
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
    %% io:format("Response:~p~n",[Response]),
    gen_tcp:send(Socket, Handshake),
    Pid = start_session(Socket, Path),
    websocket_mm(Socket, Pid, <<>>, 0, 0).

websocket_mm(Socket, Pid, Buff, Rec, Sent) ->
    receive
	{tcp, Socket, Bin} -> 
	    Str = deframe(Bin),
	    Pid ! {self(), Str},
	    websocket_mm(Socket, Pid, Buff, Rec+1, Sent);
	{tcp_closed, Socket} ->
	    exit(Pid, browser);
	{'EXIT', Pid, Why} ->
	    io:format("Exception:~p ~n",[Why]),
	    get_tcp:closed(Socket);
	Map ->
	    Z = list_to_binary(mochijson2:encode([{struct,maps:to_list(Map)}])),
	    Z1 = unicode:characters_to_binary(Z, latin1, utf8),
	    %% io:format("send_websocket:~p~n",[Z1]),
	    send_client(Socket, Z1),
	    websocket_mm(Socket, Pid, Buff, Rec, Sent+1)
    end.

%% reformat ...
send_client(Socket, Bin) ->
    Size = size(Bin),
    if Size < 126 ->
	    Data = <<1:1, 0:3, 1:4, 0:1, Size:7, Bin/binary>>,
	    gen_tcp:send(Socket, Data);
       true ->
	    io:format("*** too large frame !!!!~n")
	    
    end.
	
start_session(Socket, ModStr) ->
    io:format("Handler in module ~p connected to socket ~p~n",[ModStr, Socket]),
    Mod = list_to_atom(ModStr),
    S = self(),
    spawn_link(fun() -> Mod:start(S) end).

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
    Bla = unicode:characters_to_list(Bin),
    io:format("**** Received:~ts ~p~n",[Bin,Bla]),
    X = mochijson2:decode(Bla),
    struct_to_map(X);
deframe2(Do, Mask, Len, Data) ->
    io:format("deframe2: do:~p Mask:~p Length:~p Data:~p~n",
	      [Do, Mask, Len, size(Data)]).

unmask(Data, no) ->
    Data;
unmask(Data, {yes, Mask}) ->
    M = binary_to_list(Mask),
    unmask(binary_to_list(Data), M, M, []).

unmask([],_,_,L)              -> reverse(L);
unmask(X, [], M, L)           -> unmask(X, M, M, L);
unmask([H1|T1], [H2|T2], M,L) -> unmask(T1, T2, M, [H1 bxor H2|L]).

b2i(B) ->
    list_to_integer(binary_to_list(B)).

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
    lists:map(fun(KeyVal) ->
		      case string:tokens(KeyVal, "=") of
			  [Key, Val] ->
			      {urlencoded2str(Key), urlencoded2str(Val)};
			  [Key] ->
			      {urlencoded2str(Key), ""};
			  _ ->
			      io:format("Invalid str:~p~n",[KeyVal]),
			      {"error", "error"}
		      end
	      end, Args1).



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

%% parse_request(Cont, Bin) -> {ok, Result, Cont'} | {more, Cont'}
%% the origonal Cont is <<>>

struct_to_map({struct,L}) ->				     
    S1 = [{b2a(K), struct_to_map(V)} || {K,V} <- L],
    maps:from_list(S1);
struct_to_map(L) when is_list(L) ->
    [struct_to_map(I) || I <- L];
struct_to_map(K) ->
    K.

map_to_struct(Map) when is_map(Map) ->
    L = maps:to_list(Map),
    {struct, [{K,map_to_struct(V)} || {K,V} <- L]}; 
map_to_struct(L) when is_list(L) ->
    [map_to_struct(I) || I<- L];
map_to_struct(X) ->
    X.

json_bin_to_map(Bin) when is_binary(Bin) ->
    Str = urlencoded2str(binary_to_list(Bin)),
    io:format("From server:Str:~p~n",[Str]),
    Term = mochijson2:decode(Str),
    struct_to_map(Term).


return_json(Map) ->
    Json = map_to_struct(Map),
    Encoded = mochijson2:encode(Json),
    Bin = list_to_binary(Encoded),
    {json, Bin}.

 



