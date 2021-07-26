%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(mylog_test).   
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("src/kube_logger.hrl").
%-include_lib("eunit/include/eunit.hrl").
%% --------------------------------------------------------------------

%% External exports
-export([start/0]). 


%% ====================================================================
%% External functions
%% ====================================================================


%% --------------------------------------------------------------------
%% Function:tes cases
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
start()->
    io:format("~p~n",[{"Start setup",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=setup(),
    io:format("~p~n",[{"Stop setup",?MODULE,?FUNCTION_NAME,?LINE}]),

 %   io:format("~p~n",[{"Start cli_0()",?MODULE,?FUNCTION_NAME,?LINE}]),
  %  ok=cli_0(),
  %  io:format("~p~n",[{"Stop cli_0()",?MODULE,?FUNCTION_NAME,?LINE}]),


    io:format("~p~n",[{"Start pass_0()",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=pass_0(),
    io:format("~p~n",[{"Stop pass_0()",?MODULE,?FUNCTION_NAME,?LINE}]),

 %   io:format("~p~n",[{"Start pass_1()",?MODULE,?FUNCTION_NAME,?LINE}]),
%    ok=pass_1(),
%    io:format("~p~n",[{"Stop pass_1()",?MODULE,?FUNCTION_NAME,?LINE}]),

%    io:format("~p~n",[{"Start pass_2()",?MODULE,?FUNCTION_NAME,?LINE}]),
%    ok=pass_2(),
%    io:format("~p~n",[{"Stop pass_2()",?MODULE,?FUNCTION_NAME,?LINE}]),

  %  io:format("~p~n",[{"Start pass_3()",?MODULE,?FUNCTION_NAME,?LINE}]),
  %  ok=pass_3(),
  %  io:format("~p~n",[{"Stop pass_3()",?MODULE,?FUNCTION_NAME,?LINE}]),

  %  io:format("~p~n",[{"Start pass_4()",?MODULE,?FUNCTION_NAME,?LINE}]),
  %  ok=pass_4(),
  %  io:format("~p~n",[{"Stop pass_4()",?MODULE,?FUNCTION_NAME,?LINE}]),

  %  io:format("~p~n",[{"Start pass_5()",?MODULE,?FUNCTION_NAME,?LINE}]),
  %  ok=pass_5(),
  %  io:format("~p~n",[{"Stop pass_5()",?MODULE,?FUNCTION_NAME,?LINE}]),
 
    
   
      %% End application tests
    io:format("~p~n",[{"Start cleanup",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=cleanup(),
    io:format("~p~n",[{"Stop cleaup",?MODULE,?FUNCTION_NAME,?LINE}]),
   
    io:format("------>"++atom_to_list(?MODULE)++" ENDED SUCCESSFUL ---------"),
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
cli_0()->
    {ok,_}=cli:start(),
    ok=cli:print(log,"no monitor"),
    N='monitor@joq62-X550CA',
    ok=cli:monitor(N),
    N=cli:where_is_monitor(),
    ok=cli:print(log,"with monitor"),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_0()->
    kube_logger:start(),
%    {ok,_}=kube_logger:start(),
    ok=kube_logger:add_monitor('monitor@joq62-X550CA'),
    ok=kube_logger:log_msg(?KubeLog(log,"l1")),
    ok=kube_logger:log_msg(?KubeLog(ticket,"t1")),
    ok=kube_logger:log_msg(?KubeLog(alert,"a1")),
    timer:sleep(3000),
    pass_0().
    %ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------


pass_1()->
    {ok,_}=kube_logger:start(),
    ok=kube_logger:add_monitor('monitor@joq62-X550CA'),
    ok=kube_logger:log(?Logger("System restarted")),
    ok=kube_logger:ticket(?Logger("check cluster status")),
    ok=kube_logger:alarm(?Logger("lost host c1")),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_2()->
    ok=kube_logger:file_log(?Logger("System restarted")),
    ok=kube_logger:file_ticket(?Logger("check cluster status")),
    ok=kube_logger:file_alarm(?Logger("lost host c1")),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 ,mylog_test,start,
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_3()->
    os:cmd("rm -rf Mn*"),
    db_logger:install(),
    application:start(mnesia),
    timer:sleep(100),
    {atomic,ok}=db_logger:create(?KubeLog(log,"log1")),
    {atomic,ok}=db_logger:create(?KubeLog(log,"log1")),
    {atomic,ok}=db_logger:create(?KubeLog(ticket,"ticket1")),
    {atomic,ok}=db_logger:create(?KubeLog(alarm,"alarm1")),

    [{_,new,log,
      {2021,7,_},
      {_,_,_},
      'test_mylog@joq62-X550CA',mylog_test,pass_3,
      _,"log1"},
     {_,new,log,
      {2021,7,_},
      {_,_,_},
      'test_mylog@joq62-X550CA',mylog_test,pass_3,
      _,"log1"}]=db_logger:severity(log),
    [{_,new,ticket,
      {2021,7,_},
      {_,_,_},
      'test_mylog@joq62-X550CA',mylog_test,pass_3,
      _,"ticket1"}]=db_logger:severity(ticket),
    [{_,new,alarm,
     {2021,7,_},
     {_,_,_},
     'test_mylog@joq62-X550CA',mylog_test,pass_3,
     _,"alarm1"}]=db_logger:severity(alarm),
    
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_4()->
    
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_5()->
  
    
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
-define(APP,kubelet). 
setup()->
    
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------    

cleanup()->
  
    application:stop(controller),
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
