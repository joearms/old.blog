-module(ehe_sys).

-uuid("1e9801c7-e80b-4fa3-a3ec-e8329b281b4a").

-tags([ehe,sys,system,parameterised_module]).

-description("Provide the SYS module used inside EHE code").


%% these are the SYS calls
-compile(export_all).

make(X) ->
    {ehe_sys, X}.

help(E) ->
    io:format("Help:~p~n",[E]),
    "help me". %% IO list

banner(E) ->
    io:format("Banner:~p~n",[E]),
    "banner".

include_page(File, {?MODULE,Port,_Page,Args,ZipHandle}) ->
    io:format("get_page::~p~n",[File]),
    case esimple_http_handler:get_file_from_somewhere(File, ZipHandle, Args, Port) of
	{ok, Bin} ->
	    Bin;
	{error, _} ->
	    ["** no such page :: ", File]
    end.

get_args({?MODULE,_Port,_Page,Args,_Ziphandle}) ->
    Args.

print_args(E) ->
    ["args",io_lib:format("<pre>~p~n</pre>\n",[E]) ].

auth_user_name(_) ->
    "Fred".

list_files(_) ->
    {ok, L} = file:list_dir("."),
    [["<li>",a(I, I),"\n"] ||  I<- L].

a(I,I) ->
    ["<a href='",I,"'>",I,"</a>"].
