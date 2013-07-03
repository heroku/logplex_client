%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2013, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created :  20 June 2013 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(logplex_client_syslog).

-behaviour(gen_server).

%% API
-export([start_link/2
        ,send/7]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("logplex_client.hrl").

-define(SERVER, ?MODULE).

-record(state, {host, port, socket}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Host, Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [unicode:characters_to_list(Host), Port], []).

-spec send(facility(), severity(),
           Time::iolist(), Source::iolist(),
           Process::iolist(), Msg::iolist(), iolist() | binary()) -> ok.
send(Facility, Severity, Time, Source, Process, Msg, Token) ->
    Data =
        logplex_client_syslog_utils:to_framed_rfc5424(Facility, Severity, Time, Source, Token, Process, undefined, undefined, Msg),
    gen_server:call(?SERVER, {send, Data}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Host, Port]) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, true}]),
    {ok, #state{host=Host, port=Port, socket=Socket}}.

handle_call({send, Data}, _From, State=#state{socket=Socket}) ->
    gen_tcp:send(Socket, Data),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp_closed, _Socket}, State) ->
    {stop, tcp_closed, State#state{socket=undefined}};
handle_info({tcp_error, _Socket, Reason}, State) ->
    {stop, {tcp_error, Reason}, State#state{socket=undefined}}.

terminate(Reason, #state{socket=undefined}) ->
    lager:info("at=terminate reason=~p", [Reason]),
    ok;
terminate(Reason, #state{socket=Socket}) ->
    gen_tcp:close(Socket),
    lager:info("at=terminate reason=~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
