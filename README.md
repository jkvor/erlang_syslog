### Enable UDP

Ensure that syslogd has udp sockets enabled:
[OS X](http://stackoverflow.com/questions/1185554/how-to-enable-syslogd-to-receive-udp-logs-from-routers-in-osx)

### Build

    make
    
### Log

    0> application:start(syslog).
    ok
    1> syslog:send(wombat, info, "happy").
    ok
    
### Logged

    $ syslog
    ...
    Tue Mar 16 18:36:48 192.168.1.101  wombat[4294967295] <Info>: happy
