-module(bq_proxy).

-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

-export([onopen/1, onmessage/2, onclose/1]).

-record(proxy, {
    upstream,
    id
}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_websocket}.

websocket_init(_TransportName, Req, Opts) ->
    {ok, Upstream} = websocket_client:start_link(proplists:get_value(upstream, Opts), ?MODULE, [self()]),
    {ok, Req, #proxy{upstream = Upstream}}.


websocket_handle({text, Msg}, Req, #proxy{id=Id,upstream = Upstream} = Proxy) ->
    Command = bq_msg:decode(Msg),
    lager:debug("user ~p> ~p", [Id,Command]),
    websocket_client:write(Upstream, {text, Msg}),
    {ok, Req, Proxy}.


websocket_info({text, Msg}, Req, #proxy{} = Proxy) ->
    Command = bq_msg:decode(Msg),
    Id = case Command of
        [welcome,Id_|_] -> Id_;
        _ -> Proxy#proxy.id
    end,
    lager:debug("node ~p> ~p", [Id,Command]),
    {reply, {text, Msg}, Req, Proxy#proxy{id = Id}}.


websocket_terminate(_Reason, _Req, _State) ->
    ok.

onopen([Pid]) ->
    lager:debug("Upstream connected"),
    {ok, Pid}.

onmessage({close, Code}, Pid) ->
    lager:debug("Closing received from node: ~p", [Code]),
    {ok, Pid};

onmessage({text, Message}, Pid) ->
    Pid ! {text, Message},
    {ok, Pid};

onmessage({binary, Message}, Pid) ->
  lager:debug("Bin from node: ~p", [erlang:binary_to_term(Message)]),
  Pid ! {binary, Message},
  {ok, Pid}.

onclose(Pid) ->
  {ok, Pid}.
