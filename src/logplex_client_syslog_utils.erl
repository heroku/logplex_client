%% @copyright Heroku (2013)
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @author Fred Hebert <mononcqc@ferd.ca>
%% @doc Syslog message formatting utilities.
%% Basic message format with terminology re-used here:
%%  <PRI>VERSION TIMESTAMP HOSTNAME APP-NAME PROCID MSGID STRUCTURED-DATA MSG
%% @end
-module(logplex_client_syslog_utils).

-export([to_msg/2
        ,fmt/7
        ,from_msg/1
        ,frame/1
        ,datetime/1
        ,logplex_rfc5424/2
        ,rfc5424/1
        ,rfc5424/9
        ,to_framed_rfc5424/9
        ,facilities/0
        ,severities/0
        ]).

-type facility() :: 0..127
                  | 'kernel'
                  | 'user'
                  | 'mail'
                  | 'system'
                  | 'internal'
                  | 'lp'
                  | 'news'
                  | 'uucp'
                  | 'clock'
                  | 'security2'
                  | 'ftp'
                  | 'ntp'
                  | 'audit'
                  | 'alert'
                  | 'clock2'
                  | 'local0'
                  | 'local1'
                  | 'local2'
                  | 'local3'
                  | 'local4'
                  | 'local5'
                  | 'local6'
                  | 'local7'.
-type severity() :: 0..7
                  | 'emergency'
                  | 'alert'
                  | 'critical'
                  | 'error'
                  | 'warning'
                  | 'notice'
                  | 'info'
                  | 'debug'.

-type tstamp() :: iolist() | 'undefined'. % rfc5424 compliant, please!
-type source() :: iolist() | 'undefined'.
-type process() :: iolist() | 'undefined'.
-type msg() :: iolist() | 'undefined'.
-type token() :: iolist() | binary().
%% full msg
-type syslog_msg() :: {facility(), severity(), tstamp(), source(),
                       process(), msg()}.
-export_type([ syslog_msg/0
               ,facility/0
               ,severity/0
               ,tstamp/0
               ,source/0
               ,process/0
               ,msg/0
               ,token/0
             ]).


%% @doc Formats a message to the syslog format required by logplex.
%% Currently just an alias of logplex_rfc5424/2.
-spec to_msg(syslog_msg(), token()) -> iolist().
to_msg({Facility, Severity, Time, Source, Process, Msg}, Token) ->
    logplex_rfc5424({Facility, Severity, Time, Source, Process, Msg}, Token).

%% @doc Generates a basic message that can later be passed to to_msg/2 or
%% logplex_rfc5424/2 to generate a valid log message, but accepts
%% receiving a format string and arguments instead of a flat message.
-spec fmt(facility(), severity(), tstamp(), source(), process(),
          Fmt::string(), Args::[term()]) -> syslog_msg().
fmt(Facility, Severity, Time, Source, Process, Fmt, Args) ->
    {facility_to_int(Facility),
     severity_to_int(Severity),
     datetime(Time),
     Source,
     Process,
     io_lib:format(Fmt, Args)}.

%% @doc Parses a syslog message amd returns the equivalent fields in it,
%% omitting HOSTNAME, MSGID, and STRUCTURED-DATA. Meant to read back from
%% logplex messages.
-spec from_msg(binary()) -> {facility(), severity(), tstamp(), source(),
                             process(), msg()}
                          | {error, bad_syslog_msg}.
from_msg(Msg) when is_binary(Msg) ->
    %% <40>1 2010-11-10T17:16:33-08:00 domU-12-31-39-13-74-02 t.xxx web.1 - - State changed from created to starting
    %% <PriFac>1 Time Host Token Process - - Msg
    case re:run(Msg, "^<(\\d+)>1 (\\S+) (\\S+) (\\S+) (\\S+) \\S+ \\S+ (.*)",
                [{capture, all_but_first, binary}]) of
        {match, [PriFac, Time, Source, Ps, Content]} ->
            <<Facility:5, Severity:3>> =
                << (list_to_integer(binary_to_list(PriFac))):8 >>,
            {Facility, Severity, Time, Source, Ps, Content};
        _ ->
            {error, bad_syslog_msg}
    end.

%% @doc Frames a syslog message so that its length is prefixed, as required to
%% send syslog messages of arbitrary length over TCP.
-spec frame(Msg::iolist()) -> iolist().
frame(Msg) when is_binary(Msg); is_list(Msg) ->
    Length = iolist_size(Msg),
    [ integer_to_list(Length),
      " ",
      Msg ].

%% @doc Format an erlang timestamp to a string that respects the format
%% required by syslog (rfc5424). The erlang timestamp must be either the one
%% returned by now() or os:timestamp() ({Megasec, Sec, Microsec}) or returned
%% by the calendar module ({{Y,M,D},{H,MM,S}}).
%% Can also take the atom 'now' to generate a timestamp with the current time.
-spec datetime('now' | erlang:timestamp() | calendar:datetime()) -> tstamp().
datetime(now) ->
    datetime(os:timestamp());
datetime({_,_,_} = Now) ->
    DT = calendar:now_to_universal_time(Now),
    datetime(DT);
datetime({{Y,M,D},{H,MM,S}}) ->
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B"
                  "Z",
                  [Y,M,D, H,MM,S]).

%% @doc Formats a message to a syslog message format with the additional
%% requirements from Logplex. The items expected are:
%%  - {Facility, Severity, Time, Source, Process, Msg}
%%  - Token
%% where:
%%  - Facility: 0..127, or atoms following the facility() type, representing
%%              the kind of system that generated the message
%%  - Severity: 0..7, or atoms following the severity() type, representing
%%              how important the error is, where 0 (emergency) is the most
%%              severe, and 7 (debug) is the least severe.
%%  - Time: a timestamp in the format dictated by the syslog RFC 5424
%%          (http://tools.ietf.org/html/rfc5424#page-11). A current timestamp
%%          can be generated by calling `logplex_client_syslog_utils:datetime(now)`.
%%          The same function with an actual erlang timestamp() format can be used
%%          to convert appropriately.
%%  - Source: the host name or a string describing the machine the message
%%            originated from, and should have no spaces in it.
%%  - Process: the name of the application that generated the message (without
%%             spaces)
%%  - Msg: The actual message (spaces allowed).
%%  - Token: The "t.xxxx..." logplex token that represents the channel the
%%           message should be forwarded to.
%%
%% Note that in traditional logplex format, `Token` takes the position of the
%% APP-NAME field (representing the source application), and `Process` takes the
%% PROCID field. The MSGID field and the STRUCTURED-DATA segments are ignored, and
%% the VERSION defaults to 1.
%%
%% So the template:
%%  <PRI>VERSION TIMESTAMP HOSTNAME APP-NAME PROCID MSGID STRUCTURED-DATA MSG
%% Gets to be constructed as
%%  <PriFac>1 Time Source Token Process - - Msg
%% Giving an example message such as:
%%  <13>1 2013-06-27T17:52:03.087396Z instance@heroku.com t.ac3128e16-68a7-49c2-95a3-25d432135d33 web.51 - - Msg
%% @end
-spec logplex_rfc5424(syslog_msg(), token()) -> iolist().
logplex_rfc5424({Facility, Severity, Time, Source, Process, Msg}, Token) ->
    rfc5424(Facility, Severity, Time, Source, Token, Process,
            undefined, undefined,
            Msg).

%% @doc Shorthand rfc5424 format that ignores the PROCID, MSGID, and
%% STRUCTURED-DATA fields, as they are optional and not always useful.
%% The version defaults to 1, as per RFC5424.
-spec rfc5424(syslog_msg()) -> iolist().
rfc5424({Facility, Severity, Time, Source, Process, Msg}) ->
    rfc5424(Facility, Severity, Time, Source,
            Process, undefined, undefined, undefined, Msg).

%% @doc rfc5424 format, with fields in order as described by the RFC.
%% The Facility and Severity are merged to the PRI format, and the version
%% defaults to 1, as per RFC5424.
-spec rfc5424(facility(), severity(),
              TIMESTAMP :: tstamp(),
              HOSTNAME :: source(),
              APPNAME :: process(),
              PROCID :: iolist() | 'undefined',
              MSGID :: iolist() | 'undefined',
              STRUCTUREDDATA :: iolist() | 'undefined',
              msg()) -> iolist().
rfc5424(Facility, Severity, Time, Host, AppName, ProcID, MsgID, StructData, Msg) ->
    [ <<"<">>, pri(Facility, Severity), <<">1">>, % PRI VSN
      [ [$\s, nvl(Item)] % rest of header
        || Item <- [Time, Host, AppName, ProcID, MsgID, StructData] ],
      case Msg of
          undefined -> [];
          _ -> [$\s, Msg]
      end
    ].

%% @doc Shorthand to frame a syslog message (adding a prefixed text length) for
%% TCP transmissions.
-spec to_framed_rfc5424(facility(), severity(),
                        TIMESTAMP :: tstamp(),
                        HOSTNAME :: source(),
                        APPNAME :: process(),
                        PROCID :: iolist() | 'undefined',
                        MSGID :: iolist() | 'undefined',
                        STRUCTUREDDATA :: iolist() | 'undefined',
                        msg()) -> iolist().
to_framed_rfc5424(Facility, Severity, Time, Host, AppName, ProcID, MsgID, StructData, Msg) ->
    frame(rfc5424(Facility, Severity, Time, Host, AppName, ProcID, MsgID, StructData, Msg)).

%% @doc returns a list of valid severities and the match-up between their
%% integer value, their atom value, and their description.
-spec facilities() -> [{integer(), atom(), string()},...].
facilities() ->
    [ { 0, kernel, "kernel messages"}
     ,{ 1, user, "user-level messages"}
     ,{ 2, mail, "mail system"}
     ,{ 3, system, "system daemons"}
     ,{ 4, security, "security/authorization messages"}
     ,{ 5, internal, "messages generated internally by syslogd"}
     ,{ 6, lp, "line printer subsystem"}
     ,{ 7, news, "network news subsystem"}
     ,{ 8, uucp, "UUCP subsystem"}
     ,{ 9, clock, "clock daemon"}
     ,{10, security2, "security/authorization messages"}
     ,{11, ftp, "FTP daemon"}
     ,{12, ntp, "NTP subsystem"}
     ,{13, audit, "log audit"}
     ,{14, alert, "log alert"}
     ,{15, clock2, "clock daemon (note 2)"}
     ,{16, local0, "local use 0  (local0)"}
     ,{17, local1, "local use 1  (local1)"}
     ,{18, local2, "local use 2  (local2)"}
     ,{19, local3, "local use 3  (local3)"}
     ,{20, local4, "local use 4  (local4)"}
     ,{21, local5, "local use 5  (local5)"}
     ,{22, local6, "local use 6  (local6)"}
     ,{23, local7, "local use 7  (local7)"}].

%% @doc returns a list of valid facilities and the match-up between their
%% integer value, their atom value, and their description.
-spec severities() -> [{integer(), atom(), string()},...].
severities() ->
    [ {0, emergency, "Emergency: system is unusable"}
     ,{1, alert, "Alert: action must be taken immediately"}
     ,{2, critical, "Critical: critical conditions"}
     ,{3, error, "Error: error conditions"}
     ,{4, warning, "Warning: warning conditions"}
     ,{5, notice, "Notice: normal but significant condition"}
     ,{6, info, "Informational: informational messages"}
     ,{7, debug, "Debug: debug-level messages"}].



%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%

%% converts 'undefined' atoms to NULL values (-) in syslog messages
nvl(undefined) -> $-;
nvl(Val) -> Val.

%% generates the PRI field from facilities and severities, as an iolist
-spec pri(facility(), severity()) -> iolist().
pri(Facility, Severity)
  when is_atom(Facility) ->
    pri(facility_to_int(Facility), Severity);
pri(Facility, Severity)
  when is_integer(Facility), is_atom(Severity) ->
    pri(Facility, severity_to_int(Severity));
pri(Facility, Severity)
  when is_integer(Facility),
       is_integer(Severity),
       0 =< Severity, Severity =< 7 ->
    integer_to_list(Facility * 8 + Severity).

%% Turns a facility to its integer value
-spec facility_to_int(facility()) -> 0..127.
facility_to_int(I)
  when is_integer(I), 0 =< I, I =< 127 ->
    I;
facility_to_int(A) when is_atom(A) ->
    element(1, lists:keyfind(A, 2, facilities())).

%% Turns a severity to its integer value
-spec severity_to_int(severity()) -> 0..7.
severity_to_int(I) when is_integer(I) ->
    I;
severity_to_int(A) when is_atom(A) ->
    element(1, lists:keyfind(A, 2, severities())).

