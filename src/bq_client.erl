-module(bq_client).

-export([init/3]).
-export([websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).
-export([hello/2]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_websocket}.


websocket_init(_TransportName, Req, _Opts) ->
    self() ! go,
    {ok, Req, []}.

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
    lager:info("Info: go"),
    {reply, {text, <<"go">>}, Req, State};
websocket_info(Info, Req, State) ->
    lager:info("Info: ~p", [Info]),
    {ok, {text, <<"go">>}, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

hello([Name, _Armor, _Weapon], State) ->
    WelcomeMsg = [welcome, 42, Name, 24, 142, 146],
    Population = [population, 13, 13],
    {ok, [WelcomeMsg, Population], State}.
