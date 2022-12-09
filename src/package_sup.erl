%%%-------------------------------------------------------------------
%%% @author Lee Barney  <barney.cit@gmail.com>
%%% @copyright © 2022, Lee S. Barney
%%% @reference Licensed under the
%%% <a href="http://creativecommons.org/licenses/by/4.0/">
%%% Creative Commons Attribution 4.0 International License</a>.
%%%
%%% @doc
%%% This is a template for supervisors that start up with no defined set of
%%% OTP behaviors. Behaviors are then added and removed dynamically.
%%%
%%%
%%% @end

%%% Created : 24 October 2022 by Lee Barney <barney.cit@gmail.com>
%%%-------------------------------------------------------------------
-module(package_sup).
-behaviour(supervisor).


%%%===================================================================
%%% Make sure to complete the documentation to match
%%% your supervisor's behavior and requirements.
%%%===================================================================

%%@private
-export([init/1]).
%%API functions
-export([start_link/0,start/2,start/3,add_child/4,remove_child/2]).

%%%===================================================================
%%% Public API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%
%% Used if there is only one such supervisor created
%% and it is registered locally under the module name.
%%
%%
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local,?MODULE},?MODULE,[]).

start(Start_info, Sup_name)->
    supervisor:start_link({local,Sup_name},?MODULE,Start_info).
    % io:fwrite("Hi", []).

%%--------------------------------------------------------------------
%% @doc
%%
%% Used if there can be many supervisors of this type
%% or if the supervisor is to be registered in any way
%% but locally.
%%
%%
%% @end
%%--------------------------------------------------------------------
start(Supervisor_name,Registration_type,Start_info)->
    supervisor:start_link({Registration_type,Supervisor_name},?MODULE,Start_info).

%%--------------------------------------------------------------------
%% @doc
%%
%% Used to dynamically add a child to at run-time.
%%
%%
%% @end
%%--------------------------------------------------------------------
% add_child(Child_name,Child_module,Child_type)->
% supervisor:start_child(?MODULE,generate_spec(Child_module,Child_name,Child_type)).
add_child(Module,Rr_name,Child_name,Child_type)->
            supervisor:start_child(?MODULE,generate_spec(Module,Child_name,Child_type)),
            rr:add_process(Rr_name, Child_name).

remove_child(Supervisor_name,Child_name)->
    % case hlr:lookup_id(Child_name) of
    %     {ok,_PID}->
            supervisor:terminate_child(Supervisor_name,Child_name),
            supervisor:delete_child(Supervisor_name,Child_name).
        % _Not_registered->
            % {error, child_not_registered}
        % end.

%% Mandatory callback functions


%% Modify this function to do appropriate supervision initialization.
%%@private
init(_Start_info) ->
    Children = [generate_spec(rr, add_rr, worker),
                generate_spec(rr, get_rr, worker),
                generate_spec(rr, update_rr, worker)],
    %% This function has a value that is a tuple
    %% consisting of ok and the initial state data.
    {ok, {{one_for_one, 2, 3600}, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Modify this function to include an appropriate shutdown time, etc depending on which portions
%% of the child specification need to be set.
%%@private
generate_spec(Module,Name,Type)->
%%
%% A child Specification is a record with the following mappings.
%%
%% child_spec() = #{id => child_id(),       % mandatory. The name to be registered.
%%                  start => mfargs(),      % mandatory. The module's startup function.
%%                  restart => atom(),              % optional. Options are permanent (restart always), transient (restart only after abnormal termination), and temporary (never restart).
%%                  shutdown => integer()|atom(),   % optional. A number or the atom infinity representing the milliseconds allowed for a soft, normal shutdown before it is killed brutally.
%%                  type => atom(),                 % optional. Options are worker or supervisor.
%%                  modules => [module()]|atom()}   % optional. A list of modules to be considered for upgrading
%%                                                  % when the child's code is upgraded. The dynamic atom is used for when
%%                                                  % such a list is unknown, for example when the child is a
%%                                                  % gen_event manager with some unknown types of gen_event handler
%%                                                  % modules to be added later.
        #{id => Name,
          start => {Module,start_link,[Name]},
          restart => transient,
          shutdown => 2000,
          type => Type,
          modules => [Module]}.