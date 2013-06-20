%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <tristan@heroku.com>
%%% @copyright (C) 2012, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 20 Jun 2013 by Tristan Sloughter <tristan@heroku.com>
%%%-------------------------------------------------------------------
-module(logplex_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/5]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(SyslogHost, SyslogPort, APIHost, Username, Password) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [SyslogHost, SyslogPort, APIHost, Username, Password]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([SyslogHost, SyslogPort, APIHost, Username, Password]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    ChildSpecs = [{logplex_client_syslog, {logplex_client_syslog, start_link,
                                          [SyslogHost, SyslogPort]},
                   permanent, 2000, worker, []}
                 ,{logplex_client_api, {logplex_client_api, start_link,
                                          [APIHost, Username, Password]},
                   permanent, 2000, worker, []}],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
