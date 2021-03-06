%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created : 
%%%  
%%% -------------------------------------------------------------------
-module(monitor_server).   
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state, {}).



%% --------------------------------------------------------------------
%% Definitions 
%% --------------------------------------------------------------------


%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================



%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: 
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%
%% --------------------------------------------------------------------
init([]) ->
 
    {ok, #state{}}.
    
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (aterminate/2 is called)
%% --------------------------------------------------------------------


handle_call({ping},_From,State) ->
    Reply={pong,node(),?MODULE},
    {reply, Reply, State};

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% -------------------------------------------------------------------
handle_cast({print,Info}, State) ->
    nice(Info),
    {noreply, State};

handle_cast({print,Severity,Info}, State) ->
    io:format("~p~n",[{Severity,Info}]),  
    {noreply, State};

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{?MODULE,?LINE,Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_info(Info, State) ->
    io:format("unmatched match info ~p~n",[{?MODULE,?LINE,Info}]),
    {noreply, State}.


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
%% Time       Date         Id   Severity      App     Module      Info
%% 100721 05:33:45 12312312313 ticket kube_logger monitor badrpc nodedown  
%%
%% 10/07-21|123123434223|ticket|kube_logger    |longNAmF|badrpc nodedown   
%% 10/07-21|123123166532|log   |servic1        |module1 |badrpc nodedown 
%% 10/07-21|123123245764|alert |standrad_lib_al|log     |badrpc nodedown 
%{alert,
% {2021,7,20},
%       {15,38,23},
%       'test_mylog@joq62-X550CA',undefined,mylog_test,pass_0,100,"a1"}

nice({Severity,{Y2,M,D},{H,Min,S},
      Node,Application,Module,Fun,Line,Info})->
    Y=Y2-2000,
    {Y1,M1,D1}={integer_to_list(Y),integer_to_list(M),integer_to_list(D)},
    {H1,Min1,S1}={integer_to_list(H),integer_to_list(Min),integer_to_list(S)}, 
    DateTime=H1++":"++Min1++":"++S1++" "++D1++"/"++M1++"-"++Y1,
    Severity1=atom_to_list(Severity),
    Node1=atom_to_list(Node),
    App1=atom_to_list(Application),
    Module1=atom_to_list(Module),
    Fun1=atom_to_list(Fun),
    Line1=integer_to_list(Line),

    Msg=DateTime++" | "++Severity1++" | "++Node1++" | "++App1++"| "++Module1++"| "++Fun1++" | "++Line1++" | "++Info,
 %   io:format("~p~n",[Msg]), 
    io:format("~-16s ~-7s ~-25s ~-10s ~-15s ~-15s ~-4s ~s ~n",[DateTime,Severity1,Node1,App1,Module1,Fun1,Line1,Info]),
    ok.
 
