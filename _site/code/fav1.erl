-module(fav1).
-export([test/0]).

test() ->
    Pid = spawn(fun universal_server/0),
    Pid ! {become, fun fac_server/0},
    Pid ! {self(), 50},
    receive
	X -> X
    end.

universal_server() ->
    receive
	{become, F} -> F()
    end.

fac_server() ->
    receive
	{From, N} ->
	    From ! factorial(N),
	    fac_server()
    end.

factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).

    
