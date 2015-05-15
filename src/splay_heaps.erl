%% Copyright (c) 2015, Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Splay heaps
-module(splay_heaps).

-behaviour(heaps).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/0, is_empty/1, in/2, out/1, merge/2, fold/3]).

-export_type([heap/0, heap/1, item/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-opaque heap(Item) :: empty | {heap(Item), Item, heap(Item)}.
-type heap() :: heap(item()).
-type item() :: term().

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
-spec in(Item, Heap :: heap(Item)) -> heap(Item).
in(Item, Heap) ->
    {Left, Right} = partition(Item, Heap),
    {Left, Item, Right}.

%% @doc Removes the smallest item from the heap `Heap'
%%
%% Returns the tuple `{Item, Heap2}', where `Item' is the item removed and `Heap2' is the resulting heap.
%% If `Heap' is empty, the tuple `empty' is returned.
-spec out(Heap :: heap(Item)) -> {Item, Heap2 :: heap(Item)} | empty.
out(empty) -> empty;
out(Heap0) -> out_impl(Heap0).

%% @doc Returns the merged heap of `Heap1' and `Heap2'
-spec merge(Heap1 :: heap(Item1), Heap2 :: heap(Item2)) -> heap(Item1|Item2).
merge(empty,     Heap)  -> Heap;
merge({L1, X, R1}, Heap) ->
    {L2, R2} = partition(X, Heap),
    {merge(L2, L1), X, merge(R2, R1)}.

%% @doc Folds `Function' over every item in `Heap' returing the final value of the accumulator
-spec fold(heaps:fold_fun(Item), term(), heap(Item)) -> term().
fold(_Fun, Acc, empty)    -> Acc;
fold(Fun, Acc, {A, X, B}) -> fold(Fun, fold(Fun, Fun(X, Acc), A), B).

%%--------------------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------------------
-spec out_impl(heap(Item)) -> {Item, heap(Item)}.
out_impl({empty,         Y, C}) -> {Y, C};
out_impl({{empty, X, B}, Y, C}) -> {X, {B, Y, C}};
out_impl({{    A, X, B}, Y, C}) ->
    {Z, AA} = out_impl(A),
    {Z, {AA, X, {B, Y, C}}}.

-spec partition(Item, heap(Item)) -> {heap(Item), heap(Item)}.
partition(_Pivot, empty)                                 -> {empty, empty};
partition(Pivot, {A, X, B}) when X < Pivot,  B =:= empty -> {{A, X, B}, empty};
partition(Pivot, {A, X, B}) when Pivot =< X, A =:= empty -> {empty, {A, X, B}};
partition(Pivot, {A, X, {B1, Y, B2}}) when Y < Pivot ->
    {Small, Big} = partition(Pivot, B2),
    {{{A, X, B1}, Y, Small}, Big};
partition(Pivot, {A, X, {B1, Y, B2}}) when X < Pivot, Pivot =< Y->
    {Small, Big} = partition(Pivot, B1),
    {{A, X, Small}, {Big, Y, B2}};
partition(Pivot, {{A1, Y, A2}, X, B}) when Y < Pivot, Pivot =< X ->
    {Small, Big} = partition(Pivot, A2),
    {{A1, Y, Small}, {Big, X, B}};
partition(Pivot, {{A1, Y, A2}, X, B}) when Pivot =< Y ->
    {Small, Big} = partition(Pivot, A1),
    {Small, {Big, Y, {A2, X, B}}}.
