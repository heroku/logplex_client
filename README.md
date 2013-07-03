Erlang Logplex Client
====================

Build
-----

```shell
$ make
```

Run and Use
---

```shell
$ erl -pa ebin -env ERL_LIBS deps
```

For basic logplex_client_syslog_utils use:

```erlang
1> {ok, Socket} = gen_tcp:connect("logplex.heroku.com", 601, [binary, {active, true}]).
2> Token = "............".
3> Msg = logplex_client_syslog_utils:to_msg({local0, 7, logplex_client_syslog_utils:datetime(now), "app", "web.1", "test"}, Token),
3> Data = logplex_client_syslog_utils:frame(Msg).
4> gen_tcp:send(Socket, Data).
```

For entire session usage:

```
1> logplex_client_syslog:start_link(<<"syslog://localhost:6001">>).
{ok,<0.41.0>}
2> logplex_client_syslog:send(local0, 7, logplex_client_syslog_utils:datetime(now), "app", "web.1", "testcli", "t.58ac2073-4f37-4cae-a60b-dd41d9cec201").
ok
3> logplex_client_api:start_link(<<"http://local:password@localhost:8001">>, <<"local">>, <<"password">>).
{ok,<0.48.0>}
4> {url, Url} = logplex_client_api:create_session(<<"17">>). % channel 17
{url,<<"/v2/canary-fetch/e450df84-ed41-46f1-a229-5b01c2fe1525">>}
5> logplex_client_api:fetch_logs(Url).
{ok,[<<"2013-07-03T19:01:59Z app[web.1]: testcli">>]}
```
