-module(logplex_client).

-export([send/7
        ,create_session/1
        ,fetch_logs/1]).

-include("logplex_client.hrl").

-spec send(facility(), severity(),
           Time::iolist(), Source::iolist(),
           Process::iolist(), Msg::iolist(), iolist() | binary()) -> iolist().
send(Facility, Severity, Time, Source, Process, Msg, Token) ->
    logplex_client_syslog:send(Facility, Severity, Time, Source, Process, Msg, Token).

-spec create_session(string()) -> {url, binary()}.
create_session(ChannelId) ->
    logplex_client_api:create_session(ChannelId).

-spec fetch_logs(binary()) -> {ok, list()}.
fetch_logs(SessionsUrl) ->
    logplex_client_api:fetch_logs(SessionsUrl).
