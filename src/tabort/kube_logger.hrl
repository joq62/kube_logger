%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012 
%%% -------------------------------------------------------------------
-record(kube_logger,
	{
	 id,
	 status,
	 severity,
	 date,
	 time,
	 node,
	 application,
	 module,
	 function_name,
	 line,
	 info
	}).

-define(Logger(Info),[{date,date()},{time,time()},
		      {module,?MODULE},
		      {function_name,?FUNCTION_NAME},
		      {line,?LINE},
		      {node,node()},
		      {info,Info}]).
-define(KubeLog(Severity,Info),
	{
	 Severity,
	 date(),
	 time(),
	 node(),
	 application:get_application(?MODULE),
	 ?MODULE,
	 ?FUNCTION_NAME,
	 ?LINE,
	 Info}).
