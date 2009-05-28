-module(copyack).

-include("copyinout.hrl").

-define(DUMP_INDEX_INTERVAL, 1000).
-define(ANNOUNCE_INDEX_INTERVAL, 100).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%--------------------------------------------------------------------

-record(state, {socket, available, index, recent_replays}).

replay_all(_Socket, _Available, []) ->
    ok;
replay_all(Socket, Available, [{{Name, Low}, {Name, High}} | Rest]) ->
    replay1(Socket, Available, Name, Low, High),
    replay_all(Socket, Available, Rest).

replay1(_Socket, _Available, _Name, N, N) ->
    ok;
replay1(Socket, Available, Name, N, High) ->
    MsgId = {Name, N},
    {ok, Msg} = dict:find(MsgId, Available),
    ok = copyinout:send_all(Socket, {publish, MsgId, Msg}),
    replay1(Socket, Available, Name, N + 1, High).

handle_packet(_SenderIp, _SenderPort, {seen1, _MsgId}, State) ->
    %% We get these from ackers in response to retransmits we issue.
    State;
handle_packet(_SenderIp, _SenderPort, {seen, _Index}, State) ->
    State;
handle_packet(_SenderIp, _SenderPort, {replay, RequestedIndex},
              State = #state{socket = Socket,
                             available = Available,
                             index = Index,
                             recent_replays = RecentReplays}) ->
    ToReplay = intervals:difference(intervals:intersection(Index, RequestedIndex),
                                    RecentReplays),
    copyinout:dump_set("ToReplay", ToReplay),
    case intervals:is_empty(ToReplay) of
        true ->
            State;
        false ->
            Ranges = intervals:leaves(ToReplay),
            replay_all(Socket, Available, Ranges),
            State#state{recent_replays = intervals:union(RecentReplays, ToReplay)}
    end;
handle_packet(SenderIp, SenderPort, {publish, MsgId, Msg},
              State = #state{socket = Socket,
                             available = OldAvailable,
                             index = OldIndex}) ->
    IndexFrag = copyinout:single_message_interval(MsgId),
    ok = copyinout:send(Socket, SenderIp, SenderPort, {seen1, MsgId}),
    case dict:find(MsgId, OldAvailable) of
        error ->
            State#state{available = dict:store(MsgId, Msg, OldAvailable),
                        index = intervals:union(OldIndex, IndexFrag)};
        {ok, _} ->
            State
    end.

%%--------------------------------------------------------------------

init([]) ->
    {ok, Socket} = gen_udp:open(5672, [binary,
                                       {recbuf, 65536},
                                       {add_membership, {?COPYINOUT_MULTICAST_ADDR, {0,0,0,0}}},
                                       {multicast_loop, true},
                                       {active, true}]),
    {ok, _TimerRef} = timer:send_interval(?ANNOUNCE_INDEX_INTERVAL, announce_index_timer),
    {ok, _TimerRef2} = timer:send_interval(?DUMP_INDEX_INTERVAL, dump_index),
    {ok, #state{socket = Socket,
                available = dict:new(),
                index = intervals:empty(),
                recent_replays = intervals:empty()}}.

handle_call(Request, _From, State) ->
    {stop, {unhandled_call, Request}, State}.

handle_cast(Msg, State) ->
    {stop, {unhandled_cast, Msg}, State}.

handle_info({udp, _Sock, SenderIp, SenderPort, PacketBin}, State) ->
    Term = binary_to_term(PacketBin),
    %%io:format("PACKET ~p ~p~n~p~n", [SenderIp, SenderPort, Term]),
    {noreply, handle_packet(SenderIp, SenderPort, Term, State)};
handle_info(announce_index_timer, State = #state{socket = Socket,
                                                 index = Index}) ->
    case copyinout:send(Socket, ?COPYINOUT_MULTICAST_ADDR, 5672, {seen, Index}) of
        ok ->
            ok;
        {error, too_big} ->
            %% We'll send it later once it shrinks again.
            ok
    end,
    {noreply, State#state{recent_replays = intervals:empty()}};
handle_info(dump_index, State = #state{index = Index}) ->
    copyinout:dump_set("Seen", Index),
    {noreply, State};
handle_info(Info, State) ->
    {stop, {unhandled_info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
