%% Copyright (c) 2015, Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Leftist heaps
-module(leftist_heaps).

-behaviour(heaps).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/0, is_empty/1, in/2, out/1, merge/2, fold/3]).

-export_type([heap/0, heap/1, item/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-opaque heap(Item) :: empty | {rank(), Item, heap(Item), heap(Item)}.
-type heap() :: heap(item()).
-type item() :: term().

-type rank() :: non_neg_integer().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc Returns an empty heap
-spec new() -> heap().
new() -> empty.

%% @doc Tests if `Heap' is empty and returns `true' if so and `false' otherwise
-spec is_empty(Heap :: heap()) -> boolean().
is_empty(empty) -> true;
is_empty(_)     -> false.

%% @doc Inserts `Item' into the heap `Heap'
%%
%% Returns the resulting heap
-spec in(Item, heap(Item)) -> heap().
in(Item, Heap) -> merge({1, Item, empty, empty}, Heap).

%% @doc Removes the smallest item from the heap `Heap'
%%
%% Returns the tuple `{{value, Item}, Heap2}', where `Item' is the item removed and `Heap2' is the resulting heap.
%% If `Heap' is empty, the tuple `{empty, Heap}' is returned.
-spec out(Heap :: heap(Item)) -> {{value, Item}, Heap2 :: heap(Item)} | {empty, Heap :: heap(Item)}.
out(empty)                   -> {empty, empty};
out({_, Item, Heap1, Heap2}) -> {{value, Item}, merge(Heap1, Heap2)}.

%% @doc Returns the merged heap of `Heap1' and `Heap2'
-spec merge(Heap1 :: heap(Item1), Heap2 :: heap(Item2)) -> heap(Item1|Item2).
merge(empty, Heap2) -> Heap2;
merge(Heap1, empty) -> Heap1;
merge(Heap1, Heap2) ->
    {_, X, A1, B1} = Heap1,
    {_, Y, A2, B2} = Heap2,
    case X < Y of
        true  -> make_node(X, A1, merge(B1, Heap2));
        false -> make_node(Y, A2, merge(Heap1, B2))
    end.

%% @doc Folds `Function' over every item in `Heap' returing the final value of the accumulator
%%
%% NOTE: The iteration order is undefined
-spec fold(Function :: heaps:fold_fun(Item), AccInitial :: term(), heap(Item)) -> AccResult :: term().
fold(_Fun, Acc, empty)         -> Acc;
fold(Fun, Acc, {_, X, H1, H2}) -> fold(Fun, fold(Fun, Fun(X, Acc), H1), H2).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec make_node(Item, heap(Item), heap(Item)) -> heap(Item).
make_node(X, A, B) ->
    case rank(A) >= rank(B) of
        true  -> {rank(B) + 1, X, A, B};
        false -> {rank(A) + 1, X, B, A}
    end.

-spec rank(heap()) -> rank().
rank(empty)        -> 0;
rank({R, _, _, _}) -> R.
