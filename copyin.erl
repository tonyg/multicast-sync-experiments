-module(copyin).

-include("copyinout.hrl").

-define(RTT_UPDATE_EVERY_N_SAMPLES, 100).
-define(MIN_RTT, 5).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([publish/2]).

start_link(Name) ->
    gen_server2:start_link(?MODULE, [Name], []).

publish(Pid, Msg) ->
    gen_server:cast(Pid, {publish, Msg}).

%%--------------------------------------------------------------------

-record(state, {rtt_samples, rtt_sample_count, rtt, socket, name, counter, outstanding, timers}).

handle_packet(_SenderIp, _SenderPort, {seen1, MsgId},
              State = #state{outstanding = OldOutstanding,
                             timers = OldTimers}) ->
    %%io:format("ACK ~p~n", [MsgId]),
    case dict:find(MsgId, OldOutstanding) of
        error ->
            State;
        {ok, {SentTime, _Msg}} ->
            Now = now(),
            Sample = timer:now_diff(Now, SentTime) div 1000,
            NewOutstanding = dict:erase(MsgId, OldOutstanding),
            case dict:size(NewOutstanding) of
                0 -> io:format("No more outstanding~n");
                _ -> ok
            end,
            update_rtt(Sample, State#state{outstanding = NewOutstanding,
                                           timers = gb_trees:delete_any(SentTime, OldTimers)})
    end.

update_rtt(Sample, State = #state{rtt_samples = Samples,
                                  rtt_sample_count = Count}) ->
    NewCount = Count + 1,
    NewSamples = [Sample | Samples],
    if
        NewCount >= ?RTT_UPDATE_EVERY_N_SAMPLES ->
            NewRtt = case lists:sum(NewSamples) div NewCount of
                         N when N < ?MIN_RTT -> ?MIN_RTT;
                         N -> N
                     end,
            io:format("Rtt ~p; ~p outstanding~n", [NewRtt, dict:size(State#state.outstanding)]),
            State#state{rtt_samples = [],
                        rtt_sample_count = 0,
                        rtt = NewRtt};
        true ->
            State#state{rtt_samples = NewSamples,
                        rtt_sample_count = NewCount}
    end.

noreply(State = #state{rtt = undefined}) ->
    {noreply, State};
noreply(State) ->
    noreply1(now(), State).

noreply1(StartTime,
         State = #state{rtt = Rtt,
                        socket = Socket,
                        outstanding = Outstanding,
                        timers = Timers}) ->

    %% TODO: Remove these assertions once the code stabilises
    {dict_size, X} = {dict_size, dict:size(Outstanding)},
    {tree_size, X} = {tree_size, gb_trees:size(Timers)},

    %%io:format("NR ~p ~p~n", [dict:size(Outstanding), gb_trees:size(Timers)]),
    case gb_trees:is_empty(Timers) of
        true ->
            %%io:format("EMPTY~n"),
            {noreply, State};
        false ->
            {OldSentTime, {MsgId, Msg}, RemainingTimers} = gb_trees:take_smallest(Timers),
            Now = now(),
            MillisecSinceSending = timer:now_diff(Now, OldSentTime) div 1000,
            %%io:format("Since ~p~n", [{MsgId, MillisecSinceSending, Rtt * 2, Rtt}]),
            if
                OldSentTime >= StartTime ->
                    io:format("CYCLEROUND new rtt ~p~n", [Rtt * 2]),
                    {noreply, State#state{rtt = Rtt * 2}, Rtt};
                MillisecSinceSending >= Rtt * 2 ->
                    %%io:format("RESEND ~p~n", [MsgId]),
                    ok = copyinout:send_all(Socket, {publish, MsgId, Msg}),
                    noreply1(StartTime,
                             State#state{outstanding = dict:store(MsgId, {Now, Msg},
                                                                  Outstanding),
                                         timers = gb_trees:enter(Now, {MsgId, Msg},
                                                                 RemainingTimers)});
                true ->
                    Timeout = (Rtt * 2) - MillisecSinceSending + 1,
                    %%io:format("SLEEP ~p~n", [Timeout]),
                    {noreply, State, Timeout}
            end
    end.

%%--------------------------------------------------------------------

init([Name]) ->
    {ok, Socket} = gen_udp:open(0, [binary,
                                    {recbuf, 65536}, {sndbuf, 65536},
                                    {active, true}]),
    {ok, #state{rtt_samples = [],
                rtt_sample_count = 0,
                rtt = undefined,
                socket = Socket,
                name = Name,
                counter = 0,
                outstanding = dict:new(),
                timers = gb_trees:empty()}}.

handle_call(Request, _From, State) ->
    {stop, {unhandled_call, Request}, State}.

handle_cast({publish, Msg}, State = #state{socket = Socket,
                                           name = Name,
                                           counter = OldCounter,
                                           outstanding = OldOutstanding,
                                           timers = OldTimers}) ->
    MsgId = {Name, OldCounter},
    %%io:format("SEND ~p~n", [MsgId]),
    SentTime = now(),
    ok = copyinout:send_all(Socket, {publish, MsgId, Msg}),
    noreply(State#state{counter = OldCounter + 1,
                        outstanding = dict:store(MsgId, {SentTime, Msg}, OldOutstanding),
                        timers = gb_trees:enter(SentTime, {MsgId, Msg}, OldTimers)});
handle_cast(Msg, State) ->
    {stop, {unhandled_cast, Msg}, State}.

handle_info({udp, _Sock, SenderIp, SenderPort, PacketBin}, State) ->
    Term = binary_to_term(PacketBin),
    %%io:format("PACKET ~p ~p~n~p~n", [SenderIp, SenderPort, Term]),
    noreply(handle_packet(SenderIp, SenderPort, Term, State));
handle_info(timeout, State) ->
    noreply(State);
handle_info(Info, State) ->
    {stop, {unhandled_info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
