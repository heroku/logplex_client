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

```erlang
1> {ok, Socket} = gen_tcp:connect("logplex.heroku.com", 601, [binary, {active, true}]).
2> Token = "............".
3> Msg = logplex_client_syslog_utils:to_msg({local0, 7, logplex_client_syslog_utils:datetime(now), "app", "web.1", "test"}, Token),
3> Data = logplex_client_syslog_utils:frame(Msg).
4> gen_tcp:send(Socket, Data).
```
