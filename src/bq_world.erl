-module(bq_world).
-behaviour(gen_server).

-include("bq.hrl").

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).
-export([
         uniq/0,
         login/2,
         clients/0,
         get_client/1,
         move/3,
         broadcast/1
        ]).

%%
%% External API
%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

uniq() ->
    ets:update_counter(?MODULE, uniq, 1).

login(Id, Name) ->
    Res = gen_server:call(?MODULE, {login, Id, Name, self()}),
    broadcast(get_client(Id)),
    Res.

clients() ->
    ets:tab2list(bq_clients).

get_client(Id) ->
    [#client{id=Id, pid=_Pid, name=Name, x=X, y=Y}] = ets:lookup(bq_clients, Id),
    [spawn, Id, warrior, X, Y, Name, left, goldenarmor, goldensword].

move(Id,X,Y) ->
    ets:update_element(bq_clients, Id, [{#client.x,X},{#client.y,Y}]),
    broadcast([move,Id,X,Y]).

broadcast(Message) ->
    [Pid ! {json, Message} || #client{pid = Pid} <- ets:tab2list(bq_clients)].


%% gen_server callbacks
%%

init(Args) ->
    process_flag(trap_exit, true),
    ets:new(bq_world, [public, named_table]),
    ets:insert(bq_world, {uniq, 0}),

    ets:new(bq_clients, [public, named_table, {keypos, #client.id}]),
    {ok, Args}.


%% Result = gen_server:call(Pid, Message)
handle_call({login, Id, Name, Pid}, _From, State) ->
    erlang:monitor(process, Pid),
    X = random:uniform(200),
    Y = random:uniform(200),
    ets:insert(bq_clients, #client{id=Id, name=Name, pid=Pid, y=Y, x=X}),
    {reply, {ok, X, Y}, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

%% ok = gen_server:cast
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _, _, Pid, _}, State)->
    List = ets:tab2list(bq_clients),
    case lists:keyfind(Pid, #client.pid, List) of
        false -> ok;
        #client{id = Id} -> ets:delete(bq_clients, Id)
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
