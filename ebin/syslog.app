{application, syslog, [
    {description, "Erlang syslog client"},
    {vsn, "1.0"},
    {modules, [syslog_app, syslog]},
    {mod, {syslog_app, []}},
    {applications, [kernel, stdlib]},
    {registered, [syslog]}
]}.