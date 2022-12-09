%%%-------------------------------------------------------------------
%%% @author Jake Ard
%%% @copyright © 2022, Jake Ard
%%% @reference Licensed under the
%%% <a href="http://creativecommons.org/licenses/by/4.0/">
%%% Creative Commons Attribution 4.0 International License</a>.
%%%
%%% @doc
%%% This is a round robin balancer. Given a set of module-id pairs, this balancer
%%% will distribute work in a
%%% <a href="https://www.techtarget.com/whatis/definition/round-robin">
%%% round-robin</a> fashion.
%%%
%%% To use this round robin balancer, the balanced worker item must have a
%%% locally or globally registered name. The registered name is used
%%% to add the item to a balancer.
%%%
%%%
%%%
%%% Be aware that a worker item can, via its ID, be added to more than
%%% one rr_balancer. This is by design, not by accident.
%%% @end

%%% Created : 24 June 2022 by Lee Barney <barney.cit@gmail.com>
%%%-------------------------------------------------------------------
-module(rr).
-behaviour(gen_statem).

%% Only include the eunit testing libary
%% in the compiled code if testing is
%% being done.
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/1,stop/1,change/1,add_process/2]).

%% Supervisor Callbacks
-export([terminate/3,code_change/4,init/1,callback_mode/0]).

%% State Callbacks
-export([handle_event/4]).


%%%===================================================================
%%% Public API functions
%%%===================================================================
% -spec change(atom()) -> {ok, atom()}.
change(Name) ->
    gen_statem:call(Name, next).

add_process(Name, Pid) ->
    gen_statem:call(Name, {add, Pid}).

%%--------------------------------------------------------------------
%% @doc
%%
%% Documentation goes here.
%%
%%
%% @end
%%--------------------------------------------------------------------
% -spec start(atom(),[atom()]) -> {ok, atom()}.
% start(_, []) ->
%     {error, empty_worker_list};
% start(Statem_name,Initial_state) ->
%     gen_statem:start({local,Statem_name}, ?MODULE, Initial_state, []).

%%--------------------------------------------------------------------
%% @doc
%%
%% Documentation goes here.
%%
%%
%% @end
%%--------------------------------------------------------------------
% -spec start_link(atom(),[atom()]) -> {ok, atom()}.
% start_link(_, []) ->
%     {error, empty_worker_list};
% start_link(_,Initial_state) ->
%     gen_statem:start_link({local,?MODULE}, ?MODULE, Initial_state, []).
% start_link() ->
%     gen_statem:start({local,?MODULE}, ?MODULE, [s1,s2,s3], []).

start_link(Name) ->
    gen_statem:start_link({local, Name}, ?MODULE, [], []).

% do_stuff(Statem_name) ->
%     gen_statem:call(Statem_name, )
% rr(Statem_name, Workers) ->
%     [H | T] = Workers
%     gen_statem:call({call, H}, , )


%%--------------------------------------------------------------------
%% @doc
%% This function gracefully shuts down the balancer.
%%
%% The parameter of stop is an atom that
%% is a registered name of a round robin balancer.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(atom()) -> ok.
stop(Statem_name) ->
    gen_statem:stop(Statem_name).

%% Mandatory callback functions
%% @private
terminate(_Reason, _State, _Data) ->
    void.
%% @private
code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.
%% @private
init(Worker_ids) ->
    %% Set the initial state to be the list of available Worker_ids
    %% and types.
    {ok,ready,Worker_ids}.
%% @private
callback_mode() -> handle_event_function.

%%% state callback(s)

%%
%% Used to select which registered worker is to be used next in
%% a round robin fashion.
%% @private
handle_event({call,From}, next, _, [H|T]) ->
    %Modify the state data and replace State_data below with the modified state data.
    % [H|T] = State_data,
    % io:fwrite("~p~n", [CurState]),
    % io:fwrite("~p~n", [T++[H]]),
    {next_state, H, T++[H], [{reply, From, H}]};
    % {next_state, {ready, H}, T++[H], [{reply,From,Statem_name}]}.
handle_event({call,From}, {add, Pid}, _, State) ->
    {next_state, Pid, State++[Pid], [{reply, From, ok}]}.
    % {add, Pid, State, [{reply, From, ok}]}.