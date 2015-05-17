%% Copyright (c) 2015, Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Binomial heaps
-module(binomial_heaps).

-behaviour(heaps).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/0, is_empty/1, in/2, out/1, peek/1, merge/2, fold/3]).

-export_type([heap/0, heap/1, item/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-opaque heap(Item) :: [tree(Item)].
-type heap() :: heap(item()).
-type item() :: term().
-type tree(Item) :: {rank(), Item, [tree(Item)]}.
-type rank() :: non_neg_integer().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc Returns an empty heap
-spec new() -> heap().
new() -> [].

%% @doc Tests if `Heap' is empty and returns `true' if so and `false' otherwise
-spec is_empty(Heap :: heap()) -> boolean().
is_empty([]) -> true;
is_empty(_)  -> false.

%% @doc Inserts `Item' into the heap `Heap'
%%
%% Returns the resulting heap
-spec in(Item, Heap :: heap(Item)) -> heap(Item).
in(X, Heap) -> ins_tree({0, X, []}, Heap).

%% @doc Removes the smallest item from the heap `Heap'
%%
%% Returns the tuple `{Item, Heap2}', where `Item' is the item removed and `Heap2' is the resulting heap.
%% If `Heap' is empty, the tuple `empty' is returned.
-spec out(Heap :: heap(Item)) -> {Item, Heap2 :: heap(Item)} | empty.
out([])    -> empty;
out(Heap0) ->
    {Tree = {_, _, Heap1}, Heap2} = remove_min_tree(Heap0),
    {root(Tree), merge(lists:reverse(Heap1), Heap2)}.

%% @doc Returns the tuple `{Item, Heap2}' where `Item' is the front item of `Heap', or `empty' if `Heap' is empty
%%
%% `Heap2' is always equivalent to `Heap'
-spec peek(Heap :: heap(Item)) -> {Item, Heap2 :: heap(Item)} | empty.
peek([])   -> empty;
peek(Heap) ->
    {Tree, _} = remove_min_tree(Heap),
    {root(Tree), Heap}.

%% @doc Returns the merged heap of `Heap1' and `Heap2'
-spec merge(Heap1 :: heap(Item1), Heap2 :: heap(Item2)) -> heap(Item1|Item2).
merge(Heap, []) -> Heap;
merge([], Heap) -> Heap;
merge(Ts1 = [T1 | Ts1_], Ts2 = [T2 | Ts2_]) ->
    R1 = rank(T1),
    R2 = rank(T2),
    if
        R1 < R2 -> [T1 | merge(Ts1_, Ts2)];
        R2 < R1 -> [T2 | merge(Ts1, Ts2_)];
        true    -> ins_tree(link(T1, T2), merge(Ts1_, Ts2_))
    end.

%% @doc Folds `Function' over every item in `Heap' returing the final value of the accumulator
-spec fold(heaps:fold_fun(Item), term(), heap(Item)) -> term().
fold(Fun, Initial, Heap) ->
    lists:foldl(fun ({_, X, Ts}, Acc0) -> fold(Fun, Fun(X, Acc0), Ts) end, Initial, Heap).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec rank(tree(_Item)) -> rank().
rank({R, _, _}) -> R.

-spec root(tree(Item)) -> Item.
root({_, X, _}) -> X.

-spec link(tree(Item), tree(Item)) -> tree(Item).
link(T1 = {R, X1, C1}, T2 = {R, X2, C2}) ->
    case X1 < X2 of
        true  -> {R + 1, X1, [T2 | C1]};
        false -> {R + 1, X2, [T1 | C2]}
    end.

-spec ins_tree(tree(Item), heap(Item)) -> heap(Item).
ins_tree(T, []) -> [T];
ins_tree(T, Ts = [T_ | Ts_]) ->
    case rank(T) < rank(T_) of
        true  -> [T | Ts];
        false -> ins_tree(link(T, T_), Ts_)
    end.

-spec remove_min_tree(heap(Item)) -> {tree(Item), heap(Item)}.
remove_min_tree([T])      -> {T, []};
remove_min_tree([T | Ts]) ->
    {T_, Ts_} = remove_min_tree(Ts),
    case root(T) < root(T_) of
        true  -> {T, Ts};
        false -> {T_, [T | Ts_]}
    end.
