-module(bq_client).

-include("bq.hrl").

-export([init/3]).
-export([websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).
-export([hello/2,
         who/2,
         move/2,
         chat/2
        ]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_websocket}.


websocket_init(_TransportName, Req, _Opts) ->
    self() ! go,
    {ok, Req, #client{}}.

websocket_handle({text, Msg}, Req, State) ->
    Message = [Command | Args] = bq_msg:decode(Msg),
    lager:info("Msg: ~p", [Message]),
    case erlang:function_exported(?MODULE, Command, 2) of
        true ->
            case ?MODULE:Command(Args, State) of
                {ok, NewState} ->
                    {ok, Req, NewState};
                {ok, Reply, NewState} ->
                    {reply, {text, bq_msg:encode(Reply)}, Req, NewState}
            end;
        false ->
            {ok, Req, State}
    end.

websocket_info(go, Req, State) ->
    {reply, {text, <<"go">>}, Req, State};
websocket_info({json, Msg}, Req, State) ->
    {reply, {text, bq_msg:encode(Msg)}, Req, State};
websocket_info(Info, Req, State) ->
    lager:info("Info: ~p", [Info]),
    {ok, {text, <<"go">>}, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

hello([Name, _Armor, _Weapon], #client{}=State) ->
    Id = bq_world:uniq(),
    lager:info("New client: ~p", [Id]),
    {ok, X, Y} = bq_world:login(Id, Name),
    WelcomeMsg = [welcome, Id, Name, 24, 142, 146],
    Clients = bq_world:clients(),
    Population = [population, length(Clients), Id],
    Chat = [chat, Id, <<"Welcome to fabolous Undev Browser Quest Server">>],
    List = [list] ++ [C_id || #client{id=C_id} <- Clients , C_id /= Id ],
    State1 = State#client{id = Id, x = X, y = Y},
    {ok, [WelcomeMsg, Population, Chat, List], State1}.

who(List, State) ->
    {ok, [bq_world:get_client(Id) || Id <- List], State}.

move([X,Y], #client{id = Id} = State) ->
    bq_world:move(Id,X,Y),
    {ok, State}.

chat(Msg, #client{id=Id}=State) ->
    bq_world:broadcast([chat, Id, Msg]),
    {ok, State}.
