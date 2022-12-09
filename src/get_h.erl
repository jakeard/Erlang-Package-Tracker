-module(get_h).

-export([init/2]).

init(Req0, Opts) ->
        {ok,Data,_} = cowboy_req:read_body(Req0),
        Id = jsx:decode(Data),
        Pid = rr:change(get_rr),
        Info = jsx:encode(query_package:get_package(Pid, Id)),
        Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"text/json">>
        }, Info, Req0),
        {ok, Req, Opts}.