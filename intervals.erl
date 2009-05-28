-module(intervals).

-export([is_empty/1, empty/0, range/2, is_element/2]).
-export([leaves/1, sizes/2, low/1, high/1, rough_split/1]).
-export([union/2, intersection/2, difference/2]).

-export([test/0, test1/0, test2/0]).

-record(node, {ht, low, left, right, high}).
-record(leaf, {low, high}).

empty() ->
    empty.

range(L, H) when L < H ->
    #leaf{low = L, high = H};
range(L, H) when L >= H ->
    empty.

is_empty(empty) ->
    true;
is_empty(#node{}) ->
    false;
is_empty(#leaf{}) ->
    false.

low(#leaf{low = L}) -> L; 
low(#node{low = L}) -> L.

high(#leaf{high = H}) -> H;
high(#node{high = H}) -> H.

rough_split(#node{left = L, right = R}) -> {L, R}.

is_element(_E, empty) ->
    false;
is_element(E, #node{left = TL, right = TR}) ->
    P = low(TR),
    if
        E < P ->
            is_element(E, TL);
        true ->
            is_element(E, TR)
    end;
is_element(E, #leaf{low = L, high = H}) ->
    E >= L andalso E < H.

leaves(T) ->
    lists:flatten(leaves1(T)).

leaves1(empty) ->
    [];
leaves1(#leaf{low = L, high = H}) ->
    [{L, H}];
leaves1(#node{left = L, right = R}) ->
    [leaves1(L) | leaves1(R)].

sizes(T, F) ->
    {Depth, Present, NGaps, Absent} = sizes1(T, F),
    [{depth, Depth},
     {present, Present},
     {ngaps, NGaps},
     {absent, Absent}].

sizes1(empty, _F) ->
    {0, 0, 0, 0};
sizes1(#leaf{low = L, high = H}, F) ->
    {1, F(H, L), 0, 0};
sizes1(#node{left = L, right = R}, F) ->
    {LD, LP, LG, LM} = sizes1(L, F),
    {RD, RP, RG, RM} = sizes1(R, F),
    {max(LD, RD) + 1, LP + RP, LG + RG + 1, LM + RM + F(low(R), high(L))}.

min(A, B) when A < B -> A;
min(_A, B) -> B.

max(A, B) when A > B -> A;
max(_A, B) -> B.

union(empty, T) ->
    T;
union(T, empty) ->
    T;
union(T1 = #leaf{low = L1, high = H1}, T2 = #leaf{low = L2, high = H2}) ->
    if
        H1 < L2 -> #node{ht = 2, low = L1, left = T1, right = T2, high = H2};
        H2 < L1 -> #node{ht = 2, low = L2, left = T2, right = T1, high = H1};
        true    -> #leaf{low = min(L1, L2), high = max(H1, H2)}
    end;
union(T1 = #leaf{}, T2 = #node{}) ->
    union(T2, T1);
union(#node{left = TL1, right = TR1}, T2) ->
    L2 = low(T2),
    H2 = high(T2),
    LTR1 = low(TR1),
    HTL1 = high(TL1),
    if
        H2 < LTR1 ->
            M = union(TL1, T2),
            balance(#node{low = low(M), left = M, right = TR1, high = high(TR1)});
        HTL1 < L2 ->
            M = union(T2, TR1),
            balance(#node{low = low(TL1), left = TL1, right = M, high = high(M)});
        true ->
            ML = union(TL1, T2),
            union(ML, TR1)
    end.

intersection(empty, _) ->
    empty;
intersection(_, empty) ->
    empty;
intersection(#leaf{low = L1, high = H1}, #leaf{low = L2, high = H2}) ->
    range(max(L1, L2), min(H1, H2));
intersection(T1 = #leaf{}, T2 = #node{}) ->
    intersection(T2, T1);
intersection(#node{left = TL1, right = TR1}, T2) ->
    L2 = low(T2),
    H2 = high(T2),
    LTR1 = low(TR1),
    HTL1 = high(TL1),
    if
        H2 < LTR1 -> intersection(TL1, T2);
        HTL1 < L2 -> intersection(T2, TR1);
        true      -> union(intersection(TL1, T2), intersection(TR1, T2))
    end.

difference(T, empty) ->
    T;
difference(empty, _T) ->
    empty;
difference(T1 = #leaf{low = L1, high = H1}, #leaf{low = L2, high = H2}) ->
    if
        H1 < L2 -> T1;
        H2 < L1 -> T1;
        H1 < H2 -> range(L1, L2);
        L2 < L1 -> range(H2, H1);
        true    -> case {range(L1, L2), range(H2, H1)} of
                       {empty, RR} -> RR;
                       {LL, empty} -> LL;
                       {LL, RR} -> balance(#node{low = L1, left = LL, right = RR, high = H1})
                   end
    end;
difference(T1, #node{left = TL2, right = TR2}) ->
    intersection(difference(T1, TL2), difference(T1, TR2));
difference(#node{left = TL1, right = TL2}, T2) ->
    union(difference(TL1, T2), difference(TL2, T2)).

ht(empty) -> 0;
ht(#leaf{}) -> 1;
ht(#node{ht = H}) -> H.

n(L, R) ->
    #node{ht = max(ht(L), ht(R)) + 1,
          low = low(L),
          left = L,
          right = R,
          high = high(R)}.

balance(N = #node{left = L, right = R}) ->
    LH = ht(L),
    RH = ht(R),
    RightBias = RH - LH,
    if
        RightBias > 1 ->
            #node{left = RL, right = RR} = R,
            Bias2 = ht(RR) - ht(RL),
            if
                Bias2 >= 0 -> n(n(L, RL), RR);
                true -> n(n(L, RL#node.left), n(RL#node.right, RR))
            end;
        RightBias < -1 ->
            #node{left = LL, right = LR} = L,
            Bias2 = ht(LL) - ht(LR),
            if
                Bias2 >= 0 -> n(LL, n(LR, R));
                true -> n(n(LL, LR#node.left), n(LR#node.right, R))
            end;
        true ->
            N#node{ht = max(LH, RH) + 1}
    end.

test() ->
    cover:compile(?MODULE),
    R = ?MODULE:test1(),
    cover:analyse_to_file(?MODULE, [html]),
    R.

test1() ->
    empty = empty(),
    true = is_empty(empty()),

    empty = range(2, 2),
    empty = range(30, 10),

    X14 = union(range(1, 3), range(2, 4)),
    X14 = union(range(2, 4), range(1, 3)),

    false = is_empty(X14),

    X1234 = union(range(1, 2), range(3, 4)),
    X3456 = union(range(3, 4), range(5, 6)),
    {node,1,{leaf,1,2},{node,3,{leaf,3,4},{leaf,5,6},6},6} = union(X1234, X3456),

    false = is_empty(X1234),

    {node,1, {leaf,1,10}, {node,15,{leaf,15,30},{leaf,40,50},50}, 50} =
        union(union(range(1, 10), range(15, 25)),
              union(range(20, 30), range(40, 50))),

    {leaf, 15, 40} = union(range(15, 30), range(30, 40)),

    X1458 = union(range(1, 4), range(5, 8)),
    {leaf, 1, 8} = union(X1458, range(3, 6)),
    {leaf, 1, 8} = union(range(3, 6), X1458),

    false = is_element(2, empty),
    false = is_element(20, empty),
    true = is_element(2, X1458),
    false = is_element(20, X1458),

    X1458 = union(empty, X1458),
    X1458 = union(X1458, empty),
    X1458 = union(range(5, 8), range(1, 4)),

    X3456 = intersection(X1458, range(3, 6)),
    X3456 = intersection(range(3, 6), X1458),
    X1234 = intersection(X1234, X1458),

    X5678 = union(range(5, 6), range(7, 8)),
    empty = intersection(X1234, X5678),
    X5678 = intersection(X1458, X5678),

    empty = intersection(empty, X1458),
    empty = intersection(X1458, empty),

    {leaf, 1, 3} = union(range(1, 2), range(2, 3)),
    empty = intersection(range(1, 2), range(2, 3)),

    X1458 = difference(X1458, empty),
    empty = difference(empty, X1458),

    X14 = difference(X14, range(5, 10)),
    X14 = difference(X14, range(-15, -10)),
    empty = difference(X14, range(1, 10)),
    empty = difference(X14, range(-15, 4)),
    empty = difference(X14, range(-15, 10)),
    X1458 = difference(range(1, 8), range(4, 5)),
    X14 = difference(range(1, 8), range(4, 10)),
    X14 = difference(range(0, 4), range(-10, 1)),

    empty = difference(X14, X14),
    empty = difference(X1458, X1458),
    {leaf, 1, 2} = difference(range(1, 3), range(2, 3)),

    {node, -15, {node,-15,{leaf,-15,1},{leaf,4,5},5},
                {leaf,8,10}, 10} = difference(range(-15, 10), X1458),

    {node,1,{leaf,1,3},{leaf,6,8},8} = difference(X1458, range(3, 6)),
    ok.

test2() ->
    F = fun (V) -> intervals:range(V, V+1) end,
    lists:foldl(fun union/2, empty(), [F(1), F(2), F(3),
                                       F(5), F(6),
                                       F(8), F(9), F(10),
                                       F(12), F(13), F(14),
                                       F(16), F(17),
                                       F(19), F(20)]).
