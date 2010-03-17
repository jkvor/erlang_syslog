%% Copyright (c) 2010 Jacob Vorreuter <jacob.vorreuter@gmail.com>
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
-module(syslog).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

%% api callbacks
-export([start_link/0, send/3, send/4]).

-record(state, {socket, address, port}).

%%====================================================================
%% api callbacks
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send(Who, Level, Msg) when is_atom(Who), is_atom(Level), is_list(Msg) ->
    gen_server:cast(?MODULE, {send, Who, Level, Msg}).
    
send(Facility, Who, Level, Msg) when is_integer(Facility), is_atom(Who), is_atom(Level), is_list(Msg) ->
    gen_server:cast(?MODULE, {send, Facility, Who, Level, Msg}).
    
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    case gen_udp:open(0) of
        {ok, Socket} -> 
            {ok, Hostname} = inet:gethostname(),
            {ok, #state{
                    socket = Socket,
                    address = Hostname,
                    port = 514
            }};
        {error, Reason} -> {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({send, Who, Level, Msg}, State) ->
    Packet = ["<", atom_to_level(Level)+48, "> ", atom_to_list(Who), ": ", Msg, "\n"],
    do_send(State, Packet),
    {noreply, State};
    
handle_cast({send, Facility, Who, Level, Msg}, State) ->
    Packet = ["<", (Facility bor atom_to_level(Level))+48, "> ", atom_to_list(Who), ": ", Msg, "\n"],
    do_send(State, Packet),
    {noreply, State}.
    
%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
do_send(#state{socket=Socket, address=Address, port=Port}, Packet) ->
    gen_udp:send(Socket, Address, Port, Packet).
    
atom_to_level(emergency) -> 0; % system is unusable 
atom_to_level(alert) -> 1; % action must be taken immediately 
atom_to_level(critical) -> 2; % critical conditions 
atom_to_level(error) -> 3; % error conditions 
atom_to_level(warning) -> 4; % warning conditions 
atom_to_level(notice) -> 5; % normal but significant condition 
atom_to_level(info) -> 6; % informational
atom_to_level(debug) -> 7. % debug-level messages 
