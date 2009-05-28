-module(copyinout).

-include("copyinout.hrl").

-export([single_message_interval/1, send/4, send_all/2, dump_set/2]).

single_message_interval({Name, Counter}) ->
    intervals:range({Name, Counter}, {Name, Counter + 1}).

send(Socket, Ip, Port, Term) ->
    case random:uniform(20) of
        1 ->
            ok;
        _ ->
            Packet = term_to_binary(Term),
            if
                size(Packet) > 65000 ->
                    {error, too_big};
                true ->
                    case gen_udp:send(Socket, Ip, Port, Packet) of
                        {error, eagain} ->
                            send(Socket, Ip, Port, Term);
                        ok ->
                            ok
                    end
            end
    end.

send_all(Socket, Term) ->
    send(Socket, ?COPYINOUT_MULTICAST_ADDR, 5672, Term).

dump_set(What, Set) ->
    {Depth, In, Out} = intervals:sizes(Set, fun({_, H}, {_, L}) -> H - L end),
    io:format("~s set has ~p in, ~p missing, depth ~p; next unseen is ~p~n",
              [What, In, Out, Depth, case Set of empty -> unknown; _ -> intervals:high(Set) end]).
