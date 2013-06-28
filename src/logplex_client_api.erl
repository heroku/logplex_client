%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2013, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created :  20 June 2013 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(logplex_client_api).

-behaviour(gen_server).

%% API
-export([start_link/3
        ,create_session/1
        ,fetch_logs/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {url, auth}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Host, Username, Password) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Host, Username, Password], []).

fetch_logs(SessionUrl) ->
    gen_server:call(?SERVER, {fetch_logs, SessionUrl}).

create_session(ChannelId) ->
    gen_server:call(?SERVER, {create_session, ChannelId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Host, Username, Password]) ->
    Auth = <<"Basic ", (base64:encode(<<Username/binary, ":", Password/binary>>))/binary>>,
    Url = <<"https://", Host/binary>>,
    {ok, #state{url=Url, auth=Auth}}.

handle_call({create_session, ChannelId}, _From, State=#state{url=Url, auth=Auth}) ->
    SessionUrl = create_session_(Url, Auth, ChannelId),
    {reply, {url, SessionUrl}, State};
handle_call({fetch_logs, SessionUrl}, _From, State=#state{url=Url, auth=Auth}) ->
    Logs = fetch_logs_(Url, Auth, SessionUrl),
    {reply, {ok, Logs}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_session_(Url, Auth, ChannelId) ->
    %% We should probably generalize this client to not only use canaries
    Body = make_request(post, Url, <<"/v2/canary-sessions">>, Auth, jsx:encode([{channel_id, ChannelId}]), 201),
    [{<<"url">>, SessionsUrl}] = jsx:decode(Body),
    SessionsUrl.

fetch_logs_(Url, Auth, SessionUrl) ->
    %% Logs obtained from the stream are line-break delimited
    Logs = make_request(get, Url, <<SessionUrl/binary, "?srv=12345">>, Auth, [], 200),
    binary:split(Logs, <<"\n">>, [global]).

make_request(Method, Url, Path, Auth, Body, ExpectedStatus) ->
    {ok, ExpectedStatus, _RespHeaders, Client} = hackney:request(Method, <<Url/binary, Path/binary>>,
                                                                 [{<<"Authorization">>, Auth}],
                                                                 Body, []),
    lager:info("at=make_request status=~p headers=~p", [ExpectedStatus, _RespHeaders]),
    {ok, Result, _Client1} = hackney:body(Client),
    Result.
