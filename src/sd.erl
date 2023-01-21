%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 20 Jan 2023 by c50 <joq62@c50>

-module(sd).


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
-define(SERVER,sd_server).


%% External exports
-export([
	 get_node/1,
	 get_node_on_node/2,
	 get_node_on_host/2,
	 get_node_host/1,
	 get_node_host_on_node/2,
	 get_node_host_on_host/2,
	 call/5,
	 cast/4,
	 all/0,


	 ping/0
	]).

-export([
	 start/0,
	 stop/0
	]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start()-> gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).
stop()-> gen_server:call(?SERVER, {stop},infinity).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
call(App,M,F,A,T)->
    Result=case rpc:call(node(),sd,get_node,[App],T) of
	       {badrpc,Reason}->
		   {error,[{badrpc,Reason}]};
	       []->
		   {error,["No node available for app : ",App,?MODULE,?LINE]};
	       [Node|_]->
		   rpc:call(Node,M,F,A,T)
	   end,
    Result.

	%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
cast(App,M,F,A)->
    Result=case rpc:call(node(),sd,get_node,[App],5*1000) of
	       {badrpc,Reason}->
		   {badrpc,Reason};
	       []->
		   {error,["No node available for app : ",App,?MODULE,?LINE]};
	       [Node|_]->
		   rpc:cast(Node,M,F,A)
	   end,
    Result.			   

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
all()->
    Apps=[{Node,rpc:call(Node,net,gethostname,[],5*1000),rpc:call(Node,application,which_applications,[],5*1000)}||Node<-[node()|nodes()]],
    AvailableNodes=[{Node,HostName,AppList}||{Node,{ok,HostName},AppList}<-Apps,
				    AppList/={badrpc,nodedown}],
    AvailableNodes.
    


get_node(WantedApp)->
    Apps=[{Node,rpc:call(Node,application,which_applications,[],5*1000)}||Node<-[node()|nodes()]],
    AvailableNodes=[Node||{Node,AppList}<-Apps,
				     AppList/={badrpc,nodedown},
				     AppList/={badrpc,timeout},
				     true==lists:keymember(WantedApp,1,AppList)],
    AvailableNodes.

get_node_on_node(WantedApp,WantedNode)->

    Apps=[{Node,rpc:call(Node,application,which_applications,[],5*1000)}||Node<-[node()|nodes()]],
    AvailableNodes=[Node||{Node,AppList}<-Apps,
			  AppList/={badrpc,nodedown},
			  AppList/={badrpc,timeout},
			  true==lists:keymember(WantedApp,1,AppList),
			  Node==WantedNode],
    AvailableNodes.

get_node_on_host(WantedApp,WantedHost)->
    Apps=[{Node,rpc:call(Node,net,gethostname,[],5*1000),
	   rpc:call(Node,application,which_applications,[],5*1000)}||Node<-[node()|nodes()]],
    AvailableNodes=[Node||{Node,{ok,HostName},AppList}<-Apps,
				     AppList/={badrpc,nodedown},
				     AppList/={badrpc,timeout},
				     true=:=lists:keymember(WantedApp,1,AppList),
				     HostName=:=WantedHost],
    AvailableNodes.
	  

get_node_host(WantedApp)->
    Apps=[{Node,rpc:call(Node,net,gethostname,[],5*1000),
	   rpc:call(Node,application,which_applications,[],5*1000)}||Node<-[node()|nodes()]],
    AvailableNodes=[{Node,HostName}||{Node,{ok,HostName},AppList}<-Apps,
				     AppList/={badrpc,nodedown},
				     AppList/={badrpc,timeout},
				     true==lists:keymember(WantedApp,1,AppList)],
    AvailableNodes.

get_node_host_on_node(WantedApp,WantedNode)->

    Apps=[{Node,rpc:call(Node,net,gethostname,[],5*1000),
	   rpc:call(Node,application,which_applications,[],5*1000)}||Node<-[node()|nodes()]],
    AvailableNodes=[{Node,HostName}||{Node,{ok,HostName},AppList}<-Apps,
				     AppList/={badrpc,nodedown},
				     AppList/={badrpc,timeout},
				     true==lists:keymember(WantedApp,1,AppList),
				     Node==WantedNode],
    AvailableNodes.

get_node_host_on_host(WantedApp,WantedHost)->
    Apps=[{Node,rpc:call(Node,net,gethostname,[],5*1000),
	   rpc:call(Node,application,which_applications,[],5*1000)}||Node<-[node()|nodes()]],
    AvailableNodes=[{Node,HostName}||{Node,{ok,HostName},AppList}<-Apps,
				     AppList/={badrpc,nodedown},
				     AppList/={badrpc,timeout},
				     true=:=lists:keymember(WantedApp,1,AppList),
				     HostName=:=WantedHost],
    AvailableNodes.
	  

%% 
%% @doc:check if service is running
%% @param: non
%% @returns:{pong,node,module}|{badrpc,Reason}
%%
-spec ping()-> {atom(),node(),module()}|{atom(),term()}.
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).

