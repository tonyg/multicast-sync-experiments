-module(intervals).

-export([is_empty/1, empty/0, range/2, is_element/2]).
-export([to_list/1, from_list/1, sizes/2, low/1, high/1, partition/2]).
-export([union/2, intersection/2, subtract/2]).

-export([test/0, test1/0]).

%-record(node, {left, low, high, right}).
%-define(N(A,B,C,D), #node{left = A, low = B, high = C, right = D}).

-define(N(A,B,C,D), {A,B,C,D}).

is_empty(empty) ->
    true;
is_empty(_) ->
    false.

empty() ->
    empty.

range(L, H) when L < H ->
    ?N(empty, L, H, empty);
range(L, H) when L >= H ->
    empty.

is_element(_E, empty) ->
    false;
is_element(E, ?N(TL, Low, High, TR)) ->
    if
        E < Low -> is_element(E, TL);
        E < High -> true;
        true -> is_element(E, TR)
    end.

to_list(T) ->
    lists:reverse(to_list([], T)).

to_list(Acc, empty) ->
    Acc;
to_list(Acc, ?N(A,Low,High,B)) ->
    to_list([{Low,High} | to_list(Acc, A)], B).

merge_adjacent(Acc, []) ->
    lists:reverse(Acc);
merge_adjacent(Acc, [{L1, M}, {M, H2} | Rest]) ->
    merge_adjacent(Acc, [{L1, H2} | Rest]);
merge_adjacent(Acc, [R | Rest]) ->
    merge_adjacent([R | Acc], Rest).

from_list(Elts) ->
    Ranges = merge_adjacent([], lists:usort(Elts)),
    NRanges = length(Ranges),
    {T, []} = from_list(NRanges, Ranges),
    T.

from_list(0, Ranges) ->
    {empty, Ranges};
from_list(1, [{L, H} | Rest]) ->
    %% optimisation; special case of Count > 1.
    {?N(empty, L, H, empty), Rest};
from_list(Count, Ranges) ->
    Midpoint = Count div 2,
    {Left, Remainder0} = from_list(Midpoint, Ranges),
    [{L, H} | Remainder1] = Remainder0,
    {Right, Remainder2} = from_list(Count - Midpoint - 1, Remainder1),
    {?N(Left, L, H, Right), Remainder2}.

sizes(T, F) ->
    {Depth, Present, NGaps, Absent} = sizes1(T, F),
    [{depth, Depth},
     {present, Present},
     {ngaps, NGaps},
     {absent, Absent}].

sizes1(empty, _F) ->
    {0, 0, 0, 0};
sizes1(?N(L, Low, High, R), F) ->
    {LD, LP, LG, LM} = sizes1(L, F),
    {RD, RP, RG, RM} = sizes1(R, F),
    {LGaps, LAbsent} = case L of
                           empty -> {0, 0};
                           _ -> {1, F(Low, high(L))}
                       end,
    {RGaps, RAbsent} = case R of
                           empty -> {0, 0};
                           _ -> {1, F(low(R), High)}
                       end,
    {max(LD, RD) + 1,
     LP + RP + F(High, Low),
     LG + RG + LGaps + RGaps,
     LM + RM + LAbsent + RAbsent}.

low(?N(empty, Low, _, _)) -> Low;
low(?N(L, _, _, _)) -> low(L).

high(?N(_, _, High, empty)) -> High;
high(?N(_, _, _, R)) -> high(R).

union(empty, T) ->
    T;
union(T, empty) ->
    T;
union(?N(A,L,H,B), T) ->
    {TLM, TR} = partition(H, T),
    {TL, _TM} = partition(L, TLM),
    mergenode(mergenode(union(A, TL), ?N(empty, L, H, empty)), union(B, TR)).

intersection(empty, _) ->
    empty;
intersection(_, empty) ->
    empty;
intersection(?N(A,L,H,B), T) ->
    {TLM, TR} = partition(H, T),
    {TL, TM} = partition(L, TLM),
    mergenode(mergenode(intersection(A, TL), TM), intersection(B, TR)).

subtract(A, empty) ->
    A;
subtract(empty, _) ->
    empty;
subtract(?N(A,L,H,B), T) ->
    {TLM, TR} = partition(H, T),
    {TL, TM} = partition(L, TLM),
    Mid = midsub(subtract(A, TL), L, TM, H),
    mergenode(Mid, subtract(B, TR)).

midsub(Leftmost, L, empty, H) ->
    if
        L < H -> mergenode(Leftmost, ?N(empty, L, H, empty));
        true -> Leftmost
    end;
midsub(Leftmost, L, ?N(A, ML, MH, B), H) ->
    NewLeftmost = midsub(Leftmost, L, A, ML),
    midsub(NewLeftmost, MH, B, H).

mergenode(T, empty) ->
    T;
mergenode(empty, T) ->
    T;
mergenode(?N(A, AL, M, empty), ?N(empty, M, BH, B)) ->
    ?N(A, AL, BH, B);
mergenode(T1, ?N(A,L,H,B)) ->
    {T1L, empty} = partition(L, T1),
    ?N(mergenode(T1L, A), L, H, B).

%% Splits input tree into two: one with all elements LT the pivot, the
%% other with all elements GE the pivot.
partition(_Pivot, empty) ->
    {empty, empty};
partition(Pivot, T = ?N(A, L, H, B)) ->
    if
        H =< Pivot ->
            case B of
                empty ->
                    {T, empty};
                ?N(B1, BL, BH, B2) ->
                    if
                        BH =< Pivot ->
                            %% right, right
                            {Small, Big} = partition(Pivot, B2),
                            {?N(?N(A,L,H,B1),BL,BH,Small), Big};
                        BL < Pivot ->
                            %% right, mid
                            {?N(A,L,H,?N(B1,BL,Pivot,empty)), ?N(empty,Pivot,BH,B2)};
                        true ->
                            %% right, left
                            {Small, Big} = partition(Pivot, B1),
                            {?N(A,L,H,Small), ?N(Big,BL,BH,B2)}
                    end
            end;
        L < Pivot ->
            {?N(A,L,Pivot,empty), ?N(empty,Pivot,H,B)};
        true ->
            case A of
                empty ->
                    {empty, T};
                ?N(A1, AL, AH, A2) ->
                    if
                        AH =< Pivot ->
                            %% left, right
                            {Small, Big} = partition(Pivot, A2),
                            {?N(A1,AL,AH,Small), ?N(Big,L,H,B)};
                        AL < Pivot ->
                            %% left, mid
                            {?N(A1,AL,Pivot,empty), ?N(?N(empty,Pivot,AH,A2),L,H,B)};
                        true ->
                            %% left, left
                            {Small, Big} = partition(Pivot, A1),
                            {Small, ?N(Big,AL,AH,?N(A2,L,H,B))}
                    end
            end
    end.

%% min(A, B) when A < B -> A;
%% min(_A, B) -> B.

max(A, B) when A > B -> A;
max(_A, B) -> B.

test() ->
    cover:compile(?MODULE),
    R = ?MODULE:test1(),
    cover:analyse_to_file(?MODULE, [html]),
    R.

asserteq(T1, T2) ->
    L = to_list(T1),
    L = to_list(T2).

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
    ?N(?N(?N(empty,1,2,empty),3,4,empty),5,6,empty) = union(X1234, X3456),
    [{1,2},{3,4},{5,6}] = to_list(union(X1234, X3456)),
    [{1,2}] = to_list(range(1,2)),
    [] = to_list(empty),

    false = is_empty(X1234),

    ?N(?N(?N(empty,1,10,empty),15,30,empty),40,50,empty) =
        union(union(range(1, 10), range(15, 25)),
              union(range(20, 30), range(40, 50))),

    ?N(empty,15,40,empty) = union(range(15, 30), range(30, 40)),

    X1458 = union(range(1, 4), range(5, 8)),
    1 = low(X1458),
    {empty, X1458R} = partition(0, X1458),
    8 = high(X1458R), %% tricky splay to cover the second clause of high/1.
    ?N(empty,1,8,empty) = union(X1458, range(3, 6)),
    ?N(empty,1,8,empty) = union(range(3, 6), X1458),

    {?N(?N(empty,1,2,empty),3,4,?N(empty,5,6,empty)), empty}
        = partition(6.5, ?N(empty,1,2,?N(empty,3,4,?N(empty,5,6,empty)))),
    {?N(empty,1,2,?N(empty,3,3.5,empty)), ?N(empty,3.5,4,?N(empty,5,6,empty))}
        = partition(3.5, ?N(empty,1,2,?N(empty,3,4,?N(empty,5,6,empty)))),
    {?N(empty,1,2,empty), ?N(empty,3,4,?N(empty,5,6,empty))}
        = partition(2.5, ?N(empty,1,2,?N(empty,3,4,?N(empty,5,6,empty)))),

    false = is_element(2, empty),
    false = is_element(20, empty),
    true = is_element(2, X1458),
    false = is_element(20, X1458),

    X1458 = union(empty, X1458),
    X1458 = union(X1458, empty),
    X1458 = union(range(5, 8), range(1, 4)),

    [{1,4},{5,8}] = to_list(X1458),
    [] = to_list(empty),

    X1458 = from_list(to_list(X1458)),
    asserteq(X1458, from_list([{1,4},{5,6},{6,7},{7,8},{1,4}])),
    asserteq(X1458, from_list([{1,4},{1,4},{5,8}])),
    asserteq(X1458, from_list([{5,8},{1,4},{1,4}])),

    X3456 = intersection(X1458, range(3, 6)),
    X3456 = intersection(range(3, 6), X1458),
    X1234 = intersection(X1234, X1458),

    X5678 = union(range(5, 6), range(7, 8)),
    empty = intersection(X1234, X5678),
    asserteq(X5678, intersection(X1458, X5678)),

    empty = intersection(empty, X1458),
    empty = intersection(X1458, empty),

    ?N(empty,1,3,empty) = union(range(1, 2), range(2, 3)),
    empty = intersection(range(1, 2), range(2, 3)),

    X1458 = subtract(X1458, empty),
    empty = subtract(empty, X1458),

    asserteq(X14, subtract(X14, range(5, 10))),
    asserteq(X14, subtract(X14, range(-15, -10))),
    empty = subtract(X14, range(1, 10)),
    empty = subtract(X14, range(-15, 4)),
    empty = subtract(X14, range(-15, 10)),
    X1458 = subtract(range(1, 8), range(4, 5)),
    asserteq(X14, subtract(range(1, 8), range(4, 10))),
    asserteq(X14, subtract(range(0, 4), range(-10, 1))),

    empty = subtract(X14, X14),
    empty = subtract(X1458, X1458),
    ?N(empty,1,2,empty) = subtract(range(1, 3), range(2, 3)),

    Deepish
        = ?N(?N(?N(empty,-15,1,empty),4,5,empty),8,10,empty)
        = subtract(range(-15, 10), X1458),
    Deepish1
        = ?N(empty, -15, 1, ?N(empty, 4, 5, ?N(empty, 8, 10, empty))),
    [{depth, 3}, {present, 19}, {ngaps, 2}, {absent, 6}]
        = sizes(Deepish, fun (B, A) -> B - A end),
    [{depth, 3}, {present, 19}, {ngaps, 2}, {absent, 6}]
        = sizes(Deepish1, fun (B, A) -> B - A end),
    [{depth, 0}, {present, 0}, {ngaps, 0}, {absent, 0}]
        = sizes(empty, fun (B, A) -> B - A end),

    ?N(?N(empty,1,3,empty),6,8,empty) = subtract(X1458, range(3, 6)),
    ok.
