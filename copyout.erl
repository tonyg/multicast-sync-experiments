-module(copyout).

-include("copyinout.hrl").

-define(DUMP_INDEX_INTERVAL, 1000).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server2:start_link(?MODULE, [], []).

%%--------------------------------------------------------------------

-record(state, {socket, index}).

request_resend(Socket, SenderIp, SenderPort, Unseen) ->
    copyinout:dump_set("ResentRequest", Unseen),
    case copyinout:send(Socket, SenderIp, SenderPort, {replay, Unseen}) of
        ok ->
            ok;
        {error, too_big} ->
            %% FIXME WARNING: Discards client ID below! Need a better partitioning by client.
            {_, High} = intervals:high(Unseen),
            {_, Low} = intervals:low(Unseen),
            Median = (High + Low) div 2,
            {FewerUnseen, _} = intervals:partition(Median, Unseen),
            io:format("Too big, high ~p and low ~p; trying fewer, median ~p~n",
                      [High, Low, Median]),
            request_resend(Socket, SenderIp, SenderPort, FewerUnseen)
    end.

handle_packet(SenderIp, SenderPort, {seen, SeenIndex},
              State = #state{socket = Socket,
                             index = Index}) ->
    Unseen = intervals:subtract(SeenIndex, Index),
    case intervals:is_empty(Unseen) of
        true ->
            ok;
        false ->
            request_resend(Socket, SenderIp, SenderPort, Unseen)
    end,
    State;
handle_packet(_SenderIp, _SenderPort, {publish, MsgId, Msg},
              State = #state{index = Index}) ->
    case intervals:is_element(MsgId, Index) of
        true ->
            State;
        false ->
            %%io:format("Received ~p ~p~n", [MsgId, Msg]),
            case random:uniform(20) of
                1 ->
                    State;
                _ ->
            State#state{index =
                          intervals:union(Index, copyinout:single_message_interval(MsgId))}
            end
    end.

%%--------------------------------------------------------------------

init([]) ->
    {ok, Socket} = gen_udp:open(5672, [binary,
                                       {recbuf, 65536}, {sndbuf, 65536},
                                       {add_membership, {?COPYINOUT_MULTICAST_ADDR, {0,0,0,0}}},
                                       {multicast_loop, true},
                                       {active, true}]),
    {ok, _TimerRef} = timer:send_interval(?DUMP_INDEX_INTERVAL, dump_index),
    {ok, #state{socket = Socket,
                index = intervals:empty()}}.

handle_call(Request, _From, State) ->
    {stop, {unhandled_call, Request}, State}.

handle_cast(Msg, State) ->
    {stop, {unhandled_cast, Msg}, State}.

handle_info({udp, _Sock, SenderIp, SenderPort, PacketBin}, State) ->
    Term = binary_to_term(PacketBin),
    %%io:format("PACKET ~p ~p~n~p~n", [SenderIp, SenderPort, Term]),
    {noreply, handle_packet(SenderIp, SenderPort, Term, State)};
handle_info(dump_index, State = #state{index = Index}) ->
    copyinout:dump_set("Seen", Index),
    {noreply, State};
handle_info(Info, State) ->
    {stop, {unhandled_info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
