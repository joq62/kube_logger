%%%-------------------------------------------------------------------
%%% @author  joqerlang
%%% @copyright (C) <+$YEAR$+>,
%%% @doc
%%%
%%% @end
%%% Created : <+$FULLDATE$+> 
%%%-------------------------------------------------------------------
-module(kube_events).

-behaviour(gen_event).


%% Services
-export([log/1,ticket/1,alarm/1,
	file_log/1,file_ticket/1,file_alarm/1]).

%% API
-export([start_link/0, add_handler/0,
	add_handler/3]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
		 handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).


%%
log(LogInfo)->
    gen_event:notify(?SERVER,{log,LogInfo}).

ticket(TicketInfo)->
    gen_event:notify(?SERVER,{ticket,TicketInfo}).

alarm(AlarmInfo)->
    gen_event:notify(?SERVER,{alarm,AlarmInfo}).

file_log(LogInfo)->
    gen_event:notify(?SERVER,{log,LogInfo}).

file_ticket(TicketInfo)->
    gen_event:notify(?SERVER,{ticket,TicketInfo}).

file_alarm(AlarmInfo)->
    gen_event:notify(?SERVER,{alarm,AlarmInfo}).
%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates an event manager
%%
%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	gen_event:start_link({local, ?SERVER}).

%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%%
%% @spec add_handler() -> ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
add_handler() ->
	gen_event:add_handler(?SERVER, ?MODULE, []).

add_handler(Ref,Module,Args) ->
	gen_event:add_handler(Ref,Module,Args).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init({}) ->
   % io:format("Started ~p~n",[Args]),
    {ok, #state{}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event({log,LogInfo}, State) ->
    io:format("LogInfo ~p~n",[LogInfo]),
    {ok, State};

handle_event({ticket,TicketInfo}, State) ->
    io:format("TicketInfo ~p~n",[TicketInfo]),
    {ok, State};

handle_event({alarm,AlarmInfo}, State) ->
    io:format("AlarmInfo ~p~n",[AlarmInfo]),
    {ok, State};

handle_event({file_log,Fd,LogInfo}, State) ->
    io:format(Fd,"~p~n",[LogInfo]),
    {ok, State};

handle_event({file_ticket,Fd,TicketInfo}, State) ->
    io:format(Fd,"~p~n",[TicketInfo]),
    {ok, State};

handle_event({file_alarm,Fd,AlarmInfo}, State) ->
    io:format(Fd,"~p~n",[AlarmInfo]),
    {ok, State};

handle_event(UnMatchedSignal, State) ->
    io:format("UnMachedSignal ~p~n",[{?FUNCTION_NAME,UnMatchedSignal}]),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(UnMatchedSignal, State) ->
    io:format("UnMachedSignal ~p~n",[{?FUNCTION_NAME,UnMatchedSignal}]),
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(UnMatchedSignal, State) ->
    io:format("UnMachedSignal ~p~n",[{?FUNCTION_NAME,UnMatchedSignal}]),
	{ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
