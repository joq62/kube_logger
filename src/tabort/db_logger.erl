-module(db_logger).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("src/kube_logger.hrl").

-define(TABLE,kube_logger).
-define(RECORD,kube_logger).

% Db_logger specific functions

 
severity(Key)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    L1=[{Id,Status,Severity,Date,Time,Node,Module,FunctionName,Line,Info}||{?RECORD,Id,Status,Severity,Date,Time,Node,Module,FunctionName,Line,Info}<-Z,
								      Key==Severity],
    L2=sort_by_date(L1),
%    io:format("L2 = ~p~n",[L2]),
    lists:reverse(L2).

latest(0,_)->
    [];
latest(Len,all)->
   lists:sublist(read_all(),Len).

% End Special

delete_install()->
    ok.
    
install()->
    mnesia:create_schema([node()]),
    application:start(mnesia),
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				{type,bag},
				{disc_copies,[node()]}]),
    mnesia:wait_for_tables([?TABLE], 20000),
    application:stop(mnesia),
    ok.
 
create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				{type,bag}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create({Severity,Date,Time,Application,Module,FunctionName,Line,Node,Info})->
  %  io:format("{Severity,Date,Time,Module,FunctionName,Line,Node,Info} = ~p~n",[{Severity,Date,Time,Module,FunctionName,Line,Node,Info}]),
    Id=erlang:system_time(microsecond),
    Record=#?RECORD{
		    id=Id,
		    status=new, %new,ack
		    severity=Severity,
		    date=Date,
		    time=Time,
		    node=Node,
		    application=Application,
		    module=Module,
		    function_name=FunctionName,
		    line=Line,
		    info=Info},
  %  io:format("Record = ~p~n",[Record]),
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    L1=[{Id,Status,Severity,Date,Time,Node,Application,Module,FunctionName,Line,Info}||{?RECORD,Id,Status,Severity,Date,Time,Node,Application,Module,FunctionName,Line,Info}<-Z],
 %   io:format("L1 = ~p~n",[L1]),
  %  L2=sort_by_date(L1),
  %  lists:reverse(L2).
    L1.

delete(Id) ->
    F = fun() -> 
		LogDef=[X||X<-mnesia:read({?TABLE,Id}),
			   
			   X#?RECORD.id==Id
		       ],
		case LogDef of
		    []->
			mnesia:abort(?TABLE);
		    [S1]->
			mnesia:delete_object(S1) 
		end
	end,
    mnesia:transaction(F).


do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

%%-------------------------------------------------------------------------
sort_by_date([])->
    [];
sort_by_date([{Id,Status,Severity,Date,Time,Node,Application,Module,FunctionName,Line,Info}|T]) ->
    lists:append([sort_by_date([{XId,XStatus,XSeverity,XDate,XTime,XNode,XApplication,XModule,XFunctionName,XLine,XInfo}||{XId,XStatus,XSeverity,XDate,XTime,XNode,XApplication,XModule,XFunctionName,XLine,XInfo}<-T,
													   XId=<Id]),
	[{Id,Status,Severity,Date,Time,Node,Application,Module,FunctionName,Line,Info}],
		  sort_by_date([{XId,XStatus,XSeverity,XDate,XTime,XNode,XApplication,XModule,XFunctionName,XLine,XInfo}||{XId,XStatus,XSeverity,XDate,XTime,XNode,XApplication,XModule,XFunctionName,XLine,XInfo}<-T,
													   XId>Id])]).
