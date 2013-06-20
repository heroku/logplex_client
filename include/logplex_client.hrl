-type datetime() :: 'now' | erlang:timestamp() | calendar:datetime1970().

-type facility() :: 0..127 |
                    'kernel' | 'user' | 'mail' | 'system' | 'internal' | 'lp' |
                    'news' | 'uucp' | 'clock' | 'security2' | 'ftp' | 'ntp' |
                    'audit' | 'alert' | 'clock2' | 'local0' | 'local1' |
                    'local2' | 'local3' | 'local4' | 'local5' | 'local6' |
                    'local7'.

-type severity() :: 0..7 |
                    'emergency' |
                    'alert' |
                    'critical' |
                    'error' |
                    'warning' |
                    'notice' |
                    'info' |
                    'debug'.

-type syslog_msg() :: {facility(), severity(),
                       Time::iolist(), Source::iolist(),
                       Process::iolist(), Msg::iolist()}.
