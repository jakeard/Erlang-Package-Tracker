-module(package_app).
-behaviour(application).
%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
        Dispatch = cowboy_router:compile([
                {'_', [
                        {"/", toppage_h, []},
                        {"/add", add_h, []},
                        {"/query", query_h, []}
                        % {"/get_location", get_location_h, []},
                        % {"/get_path", get_path_h, []},
                        % {"/delivered", delivered_h, []}

                ]}
        ]),
        PrivDir = code:priv_dir(hello),
        {ok, _} = cowboy:start_tls(https_listener, [
                {port, 443},
                {cacertfile, PrivDir ++ "/ssl/cowboy-ca.crt"},
                {certfile, PrivDir ++ "/ssl/fullchain.pem"},
                {keyfile, PrivDir ++ "/ssl/privkey.pem"}
        ], #{env => #{dispatch => Dispatch}}),
        package_sup:start_link().

stop(_State) ->
        ok = cowboy:stop_listener(https_listener).