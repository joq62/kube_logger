%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012 
%%% -------------------------------------------------------------------
-module(sre).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%%---------------------------------------------------------------------
%% Records for test
%%

%% --------------------------------------------------------------------
-compile(export_all).


%% ====================================================================
%% External functions
%% ====================================================================


delete(Node,Dir)->
    slave:stop(Node),
    os:cmd("rm -rf "++Dir),
    ok.
    
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
create(PodId,_Vsn,AppInfo,EnvConfig) ->
    % Unique PodId
    Unique=integer_to_list(erlang:system_time(millisecond)),
    NodeId=Unique++"_"++PodId,
    % Create unique dir
    ok=file:make_dir(NodeId),
    % clone application 
    {AppId,_AppVsn,GitPath}=AppInfo,
    PathApp=filename:join([NodeId,AppId]),
    ok=file:make_dir(PathApp),
    os:cmd("git clone "++GitPath++" "++PathApp),
    %Start Slave
    {ok,Host}=inet:gethostname(),
    Cookie=atom_to_list(erlang:get_cookie()),
    Ebin=filename:join([PathApp,"ebin"]),
    SlaveArgs="-pa "++Ebin++" "++"-setcookie "++Cookie,
    {ok,Node}=slave:start(Host,NodeId,SlaveArgs),
    %Start application
    App=list_to_atom(AppId),
    case EnvConfig of
	[]->
	    ok;
	EnvConfig ->
	    rpc:call(Node,application,set_env,[EnvConfig])
    end,
    ok=rpc:call(Node,application,start,[App]),
    % Check if started
    AppsSlave=rpc:call(Node,application,which_applications,[]),
    true=lists:keymember(App,1,AppsSlave),
    {ok,Node,NodeId}.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
node(Name)->
    {ok,HostId}=net:gethostname(),
    list_to_atom(Name++"@"++HostId).
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
vmid_hostid(Node)->
    NodeStr=atom_to_list(Node),
    [VmId,HostId]=string:lexemes(NodeStr,"@"),
    {VmId,HostId}.
