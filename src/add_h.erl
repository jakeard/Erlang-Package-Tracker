-module(add_h).

-export([init/2]).

init(Req0, Opts) ->
        {ok,Data,_} = cowboy_req:read_body(Req0),
        All_info = jsx:decode(Data),
        [Id] = maps:keys(All_info),
        [Info] = maps:values(All_info),
        Pid = rr:change(add_rr),
        add_package:add(Pid,Id,Info),
        Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/json">>
        }, "[\"done\"]", Req0),
        {ok, Req, Opts}.