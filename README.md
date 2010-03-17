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

### Get your head scratched and be happy...

![wombat](http://neatorama.cachefly.net/images/uploads/2007/05/450_happywombat.jpg)