-module(mymd).

-export([getvar/2, include_preformatted/1]).

include_preformatted(File) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    iolist_to_binary(["```\n",Bin,"\n```\n"]);
	_ ->
	    iolist_to_binary(["```\nCannot read:",File,"\n```\n"])
    end.



%% things here should in principle *never* change

getvar(Tag,Map) ->
    case maps:find(Tag, Map) of
	{ok,Val} ->
	    Val;
	error ->
	    io:format("*** No variable::~p~n",[Tag]),
	    io_lib:format("*** No variable::~p~n",[Tag])
    end.
