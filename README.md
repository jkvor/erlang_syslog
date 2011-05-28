### Enable UDP

Ensure that syslogd has udp sockets enabled:
[OS X](http://stackoverflow.com/questions/1185554/how-to-enable-syslogd-to-receive-udp-logs-from-routers-in-osx)

### Build

    make

### Log

    0> syslog:start_link(appname, "localhost", 514, local0).
    ok
    1> syslog:send("test").
    ok
    2> syslog:send("test", [{level, debug}]).

### Logged

    $ syslog
    ...
    Tue Mar 16 18:36:48 192.168.1.101  appname[4294967295] <Debug>: test
