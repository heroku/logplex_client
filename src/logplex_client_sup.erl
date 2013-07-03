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

start_link(SyslogTcpUrl, SyslogHttpUrl,
           APIUrl, Username, Password) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE,
        {[SyslogTcpUrl],
         [SyslogHttpUrl],
         [APIUrl, Username, Password]}).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init({SyslogArgs, _HttpArgs, APIArgs}) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    ChildSpecs = [{logplex_client_syslog, {logplex_client_syslog, start_link,
                                           SyslogArgs},
                   permanent, 2000, worker, []}
                 ,{logplex_client_api, {logplex_client_api, start_link,
                                        APIArgs},
                   permanent, 2000, worker, []}],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
