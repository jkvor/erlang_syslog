## Overview

This library writes messages formatted as per [RFC5424][rfc5424], transmitted
via UDP as described in [RFC5426][rfc5426].

### Enable UDP

You can configure syslog-ng to receive these messages with the following directive:

    syslog(transport("udp") port(514) keep_timestamp(yes));

### Build

    make

### Log

    0> syslog:start_link(name, app_name, "localhost", 514, local0).
    ok
    2> syslog:send(name, "test").
    ok
    3> syslog:notice(name, "other test").
    ok
    4> syslog:send(name, "test 3", [{timestamp, os:timestamp()}]).
    ok

### Logged

    $ syslog
    ...
    <134>1 2013-04-03T21:30:44.403394Z myhost.local app_name pid - - my log message

[rfc5424]: http://tools.ietf.org/html/rfc5424 "RFC5424 - The Syslog Protocol"
[rfc5426]: http://tools.ietf.org/html/rfc5426#section-3.1 "RFC5426 - Transmission of Syslog Messages over UDP"
