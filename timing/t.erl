-module(t).

-compile([export_all]).

time_to_run(F, CatchInfo) ->
    Start = now(),
    Result = case catch F() of
                 {'EXIT', Reason} ->
                     exit({time_to_run_failed, Reason, CatchInfo});
                 V -> V
             end,
    Stop = now(),
    {timer:now_diff(Stop, Start) / 1000.0, Result}.

tc(FH, Comment, Coord, F) ->
    io:format("~p ~p ~p", [?SET, Comment, Coord]),
    {TotalTime, TotalLeaves, Count} = accumulate_time(now(), 0, 0, 0, F),
    AvgTime = TotalTime / float(Count),
    AvgLeaves = TotalLeaves / float(Count),
    io:format(" ~p/~p = ~p ms; ~p/~p = ~p leaves~n",
              [TotalTime, Count, AvgTime, TotalLeaves, Count, AvgLeaves]),
    file:write(FH, io_lib:format("~p,\"~p\",~p,~p,\"milliseconds\",~p,\"leaves\"~n",
                                 [?SET, Comment, Coord, AvgTime, AvgLeaves])),
    ok.

accumulate_time(_StartTime, TotalTime, TotalLeaves, Count, _F)
  when TotalTime >= 1000 orelse Count >= 50 ->
    {TotalTime, TotalLeaves, Count};
accumulate_time(StartTime, TotalTime, TotalLeaves, Count, F) ->
    case timer:now_diff(now(), StartTime) / 1000.0 of
        N when N > 5000 andalso Count >= 3 ->
            {TotalTime, TotalLeaves, Count};
        _ ->
            io:format("."),
            {{TimeToRun, _Result}, NLeaves} = F(),
            accumulate_time(StartTime, TotalTime + TimeToRun, TotalLeaves + NLeaves, Count + 1, F)
    end.

gb_sets(empty) ->
    gb_sets:new().

gb_sets(nleaves, S) ->
    gb_sets:size(S);
gb_sets(from_list, L) ->
    gb_sets:from_list(expand_list([], L));
gb_sets(to_list, S) ->
    compress_list(gb_sets:to_list(S)).

expand_list(Acc, []) ->
    lists:reverse(Acc);
expand_list(Acc, [{L, H} | Rest]) ->
    expand_list(lists:reverse(lists:seq(L, H-1), Acc), Rest).

compress_list([]) ->
    [];
compress_list([First | Rest]) ->
    compress_list([], First, First, Rest).

compress_list(Acc, First, Last, []) ->
    lists:reverse([{First, Last + 1} | Acc]);
compress_list(Acc, First, Last, [Next | Rest]) ->
    case Last + 1 of
        Next -> compress_list(Acc, First, Next, Rest);
        _ -> compress_list([{First, Last + 1} | Acc], Next, Next, Rest)
    end.

intervals(empty) ->
    intervals:empty().

intervals(nleaves, S) ->
    {value, {_, NGaps}} = lists:keysearch(ngaps, 1, intervals:sizes(S, fun (B, A) -> B - A end)),
    NGaps + 1;
intervals(from_list, L) ->
    intervals:from_list(L);
intervals(to_list, S) ->
    intervals:to_list(S).

linear_intervals(empty) ->
    linear_intervals:empty().

linear_intervals(nleaves, {false, S}) ->
    length(S) / 2;
linear_intervals(from_list, L) ->
    linear_intervals:from_list(L);
linear_intervals(to_list, {false, Toggles}) ->
    lin_to_list([], Toggles).

lin_to_list(Acc, []) ->
    lists:reverse(Acc);
lin_to_list(Acc, [L,H | Rest]) ->
    lin_to_list([{L,H} | Acc], Rest).

merge_adjacent(Acc, []) ->
    lists:reverse(Acc);
merge_adjacent(Acc, [{L1, M}, {M, H2} | Rest]) ->
    merge_adjacent(Acc, [{L1, H2} | Rest]);
merge_adjacent(Acc, [R | Rest]) ->
    merge_adjacent([R | Acc], Rest).

check_set(S, L0) ->
    L1 = lists:usort(L0),
    L = merge_adjacent([], L1),
    {check_set, L0, L} = {check_set, L0, ?SET(to_list, S)},
    ok.

randset(N, Range) ->
    L = randset1([], N, Range),
    S = ?SET(from_list, L),
    check_set(S, L),
    S.

randset1(Acc, 0, _Range) ->
    Acc;
randset1(Acc, N, Range) ->
    X = random:uniform(Range),
    randset1([{X, X+1} | Acc], N - 1, Range).

with_file(FilenamePartStr, Fun) ->
    {ok, FH} =
        file:open("data/" ++ atom_to_list(?SET) ++ "_" ++ FilenamePartStr ++ ".csv", [write]),
    Fun(FH),
    file:close(FH).

cached_randset(Kind, NN, Range) ->
    Key = {cached_randset, Kind, NN, Range},
    case get(Key) of
        undefined ->
            io:format("(Caching"),
            S = randset(NN, Range),
            NLeaves = ?SET(nleaves, S),
            V = {S, NLeaves},
            put(Key, V),
            io:format(")"),
            V;
        V ->
            V
    end.

t_setop(Op) ->
    with_file(atom_to_list(Op),
              fun (FH) ->
                      Limit = 10000,
                      Range = Limit * 10,
                      expgrow(FH, 1, {Op, Range}, 1.2, Limit,
                              fun (NN) ->
                                      {S1, N1} = cached_randset(1, NN, Range),
                                      {S2, N2} = cached_randset(2, NN, Range),
                                      {time_to_run(fun () -> ?SET:Op(S1, S2) end,
                                                   [{s1,S1},{s2,S2}]),
                                       (N1 + N2) / 2}
                              end)
              end).

t_setop2(Op) ->
    t_setop2("2", Op, 10000, 10000).

t_setop2(Suffix, Op, Limit, Range) ->
    with_file(atom_to_list(Op) ++ Suffix,
              fun (FH) ->
                      expgrow(FH, 1, {Op, Range}, 1.2, Limit,
                              fun (NN) ->
                                      {S1, N1} = cached_randset(1, NN, Range),
                                      {time_to_run(fun () -> ?SET:Op(S1, S1) end,
                                                   [{s1,S1}]),
                                       N1}
                              end)
              end).

t_randset() ->
    with_file("randset",
              fun (FH) ->
                      Limit = 50000,
                      Range = Limit,
                      expgrow(FH, 1, {randset, Range}, 1.2, Limit,
                              fun (NN) ->
                                      {Time, S} = time_to_run(fun () -> randset(NN, Range) end,
                                                              []),
                                      {{Time, S},
                                       ?SET(nleaves, S)}
                              end)
              end).

t_union() -> t_setop(union).
t_union2a() -> t_setop2("2a", union, 50000, 10000).
t_union3() -> t_setop2("3", union, 50000, 500000).
t_subtract() -> t_setop(subtract).
t_subtract2a() -> t_setop2("2a", subtract, 50000, 10000).
t_subtract3() -> t_setop2("3", subtract, 50000, 500000).
t_intersection() -> t_setop(intersection).
t_intersection2() -> t_setop2("2", intersection, 50000, 50000).
t_intersection2a() -> t_setop2("2a", intersection, 50000, 10000).
t_intersection3() -> t_setop2("3", intersection, 50000, 500000).

t_all() ->
    erase(),
    t_randset(),
    t_union(),
    t_intersection(),
    t_subtract(),

    t_intersection2(),
    
    t_union2a(),
    t_intersection2a(),
    t_subtract2a(),

    t_union3(),
    t_intersection3(),
    t_subtract3().

expgrow(_FH, N, _Comment, _Factor, Limit, _Fun) when N > Limit ->
    ok;
expgrow(FH, N, Comment, Factor, Limit, Fun) ->
    NN = trunc(N),
    tc(FH, Comment, NN, fun() -> Fun(NN) end),
    expgrow(FH, N * Factor, Comment, Factor, Limit, Fun).
