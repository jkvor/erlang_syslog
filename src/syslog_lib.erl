%% Copyright (c) 2010 Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% Copyright (c) 2013 Fred Hebert <mononcqc@ferd.ca>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.

%% syslog rfc: http://www.faqs.org/rfcs/rfc3164.html
-module(syslog_lib).

-export([build_packet/3]).
-export([init_table/5, terminate_table/1,
         send/2, send/3,
         emergency/2, emergency/3,
         alert/2, alert/3,
         critical/2, critical/3,
         error/2, error/3,
         warning/2, warning/3,
         notice/2, notice/3,
         info/2, info/3,
         debug/2, debug/3
        ]).


%%================================
%% api
%%================================

-spec init_table(Name::atom(), AppName::atom(), Host::list(),
                 Port::non_neg_integer(), Facility::atom()) -> ets:tid().
init_table(Name, AppName, Host, Port, Facility) when is_atom(Name), is_atom(AppName), is_list(Host),
                                                     is_integer(Port), is_atom(Facility) ->
    Name = ets:new(Name, [set, named_table, public, {read_concurrency, true}]),
    true = ets:insert_new(Name, [{app_name, AppName},
                                 {host, Host},
                                 {port, Port},
                                 {facility, Facility}]),
    {ok,Name}.

terminate_table(Name) ->
    ets:delete(Name).

build_packet(Name, Msg, Opts) ->
    AppName = get_app_name(Name, Opts),
    Pid = get_pid(Opts),
    Hostname = get_hostname(Opts),
    Timestamp = get_timestamp(Opts),

    Facility = get_facility(Name, Opts),
    Level = get_level(Facility, Opts),

    Packet = [
              "<", Level, ">1 ", % syslog version 1
              Timestamp, " ",
              Hostname, " ",
              AppName, " ",
              Pid,
              " - - ", % MSGID is -, STRUCTURED-DATA is -
              Msg, "\n"
             ],

    iolist_to_binary(Packet).

send(Name, Msg) when is_list(Msg) ->
    send(Name, Msg, []).

send(Name, Msg, Opts) when is_list(Msg), is_list(Opts) ->
    Address = get_addr(Name, Opts),
    Port = get_port(Name, Opts),
    Packet = build_packet(Name, Msg, Opts),
    %io:format("~p~n", [Packet]),
    case gen_udp:open(0) of
        {ok, Socket} -> gen_udp:send(Socket, Address, Port, Packet);
        {error, Reason} -> {error, Reason}
    end.

emergency(Name, Msg) ->
    emergency(Name, Msg, []).

emergency(Name, Msg, Opts) ->
    send(Name, Msg, [{level, emergency} | Opts]).

alert(Name, Msg) ->
    alert(Name, Msg, []).

alert(Name, Msg, Opts) ->
    send(Name, Msg, [{level, alert} | Opts]).

critical(Name, Msg) ->
    critical(Name, Msg, []).

critical(Name, Msg, Opts) ->
    send(Name, Msg, [{level, critical} | Opts]).

error(Name, Msg) ->
    error(Name, Msg, []).

error(Name, Msg, Opts) ->
    send(Name, Msg, [{level, error} | Opts]).

warning(Name, Msg) ->
    warning(Name, Msg, []).

warning(Name, Msg, Opts) ->
    send(Name, Msg, [{level, warning} | Opts]).

notice(Name, Msg) ->
    notice(Name, Msg, []).

notice(Name, Msg, Opts) ->
    send(Name, Msg, [{level, notice} | Opts]).

info(Name, Msg) ->
    info(Name, Msg, []).

info(Name, Msg, Opts) ->
    send(Name, Msg, [{level, info} | Opts]).

debug(Name, Msg) ->
    debug(Name, Msg, []).

debug(Name, Msg, Opts) ->
    send(Name, Msg, [{level, debug} | Opts]).

%%====================
%% private functions
%%===================

get_app_name({proc,Name}) ->
    case gen_server:call(Name, app_name) of
        Atom when is_atom(Atom) -> atom_to_list(Atom);
        List when is_list(List) -> List;
        Binary when is_binary(Binary) -> Binary
    end;
get_app_name(Name) ->
    case ets:lookup(Name, app_name) of
        [{_,Atom}] when is_atom(Atom) -> atom_to_list(Atom);
        [{_,List}] when is_list(List) -> List;
        [{_,Binary}] when is_binary(Binary) -> Binary
    end.

get_app_name(Name, Opts) ->
    case proplists:get_value(app_name, Opts) of
        undefined -> get_app_name(Name);
        Atom when is_atom(Atom) -> atom_to_list(Atom);
        List when is_list(List) -> List;
        Binary when is_binary(Binary) -> Binary
    end.

get_pid(Opts) ->
    case proplists:get_value(pid, Opts) of
        undefined -> os:getpid();
        Atom when is_atom(Atom) -> atom_to_list(Atom);
        List when is_list(List) -> List;
        Binary when is_binary(Binary) -> Binary
    end.

get_facility({proc, Name}) ->
    Facility = gen_server:call(Name, facility),
    facility(Facility);
get_facility(Name) ->
    [{_,Facility}] = ets:lookup(Name, facility),
    facility(Facility).

get_facility(Name, Opts) ->
    case proplists:get_value(facility, Opts) of
        undefined -> get_facility(Name);
        Facility when is_atom(Facility) -> facility(Facility);
        Facility when is_integer(Facility) -> Facility
    end.

get_hostname(Opts) ->
    case proplists:get_value(hostname, Opts) of
        undefined ->
            {ok, Host} = inet:gethostname(),
            Host;
        Atom when is_atom(Atom) -> atom_to_list(Atom);
        List when is_list(List) -> List;
        Binary when is_binary(Binary) -> Binary
    end.

get_level(Facility, Opts) ->
    Level = atom_to_level(proplists:get_value(level, Opts)),
    integer_to_list((Facility * 8) + Level).

get_timestamp(Opts) when is_list(Opts) ->
    case proplists:get_value(timestamp, Opts) of
        undefined -> format_timestamp(os:timestamp());
        Timestamp -> format_timestamp(Timestamp)
    end.

get_addr(Name, Opts) ->
    case proplists:get_value(addr, Opts) of
        undefined ->
            case ets:lookup(Name, addr) of
                [] ->
                    Host = get_host(Name, Opts),
                    {ok, Addr} = inet:getaddr(Host, inet),
                    catch ets:insert_new(Name, {addr, Addr}),
                    Addr;
                [{_, Addr}] ->
                    Addr
            end;
        Addr -> Addr
    end.

get_port(Name, Opts) ->
    case proplists:get_value(port, Opts) of
        undefined ->
            [{_,Port}] = ets:lookup(Name, port),
            Port;
        Port ->
            Port
    end.

get_host(Name, Opts) ->
    case proplists:get_value(host, Opts) of
        undefined ->
            [{_,Host}] = ets:lookup(Name, host),
            Host;
        Host ->
            Host
    end.

format_timestamp(TS={_,_,US}) ->
    {{Y, M, D}, {H, MM, S}} = calendar:now_to_universal_time(TS),
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~6.10.0BZ",
                  [Y,M,D, H,MM,S,US]).

atom_to_level(emergency) -> 0; % system is unusable
atom_to_level(alert) -> 1; % action must be taken immediately
atom_to_level(critical) -> 2; % critical conditions
atom_to_level(error) -> 3; % error conditions
atom_to_level(warning) -> 4; % warning conditions
atom_to_level(notice) -> 5; % normal but significant condition
atom_to_level(info) -> 6; % informational
atom_to_level(debug) -> 7; % debug-level messages
atom_to_level(_) -> atom_to_level(info). % default to info

% paraphrased from https://github.com/ngerakines/syslognif/blob/master/src/syslog.erl#L55
facility(kern) -> 0;      % kernel messages
facility(user) -> 1;      % random user-level messages
facility(mail) -> 2;      % mail system
facility(daemon) -> 3;    % system daemons
facility(auth) -> 4;      % security/authorization messages
facility(syslog) -> 5;    % messages generated internally by syslogd
facility(lpr) -> 6;       % line printer subsystem
facility(news) -> 7;      % network news subsystem
facility(uucp) -> 8;      % UUCP subsystem
facility(cron) -> 9;      % clock daemon
facility(authpriv) -> 10; % security/authorization messages (private)
facility(ftp) -> 11;      % ftp daemon

facility(local0) -> 16;   % reserved for local use
facility(local1) -> 17;   % reserved for local use
facility(local2) -> 18;   % reserved for local use
facility(local3) -> 19;   % reserved for local use
facility(local4) -> 20;   % reserved for local use
facility(local5) -> 21;   % reserved for local use
facility(local6) -> 22;   % reserved for local use
facility(local7) -> 23.   % reserved for local use
