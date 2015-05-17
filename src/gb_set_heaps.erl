%% Copyright (c) 2015, Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Heaps implmented by gb_sets module
-module(gb_set_heaps).

-behaviour(heaps).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/0, is_empty/1, in/2, out/1, peek/1, merge/2, fold/3]).

-export_type([heap/0, heap/1, item/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-opaque heap(Item) :: {gb_sets:set(Item), non_neg_integer(), reference()}.
-type heap() :: heap(item()).
-type item() :: term().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc Returns an empty heap
-spec new() -> heap().
new() -> {gb_sets:empty(), 0, make_ref()}.

%% @doc Tests if `Heap' is empty and returns `true' if so and `false' otherwise
-spec is_empty(Heap :: heap()) -> boolean().
is_empty({Set, _, _}) -> gb_sets:is_empty(Set).

%% @doc Inserts `Item' into the heap `Heap'
%%
%% Returns the resulting heap
-spec in(Item, Heap :: heap(Item)) -> heap().
in(Item, {Set, N, Ref}) -> {gb_sets:add({Item, N, Ref}, Set), N + 1, Ref}.

%% @doc Removes the smallest item from the heap `Heap'
%%
%% Returns the tuple `{Item, Heap2}', where `Item' is the item removed and `Heap2' is the resulting heap.
%% If `Heap' is empty, the tuple `empty' is returned.
-spec out(Heap :: heap(Item)) -> {Item, Heap2 :: heap(Item)} | empty.
out({Set, N, Ref}) ->
    case gb_sets:is_empty(Set) of
        true  -> empty;
        false ->
            {{Item, _, _}, Set2} = gb_sets:take_smallest(Set),
            {Item, {Set2, N, Ref}}
    end.

%% @doc Returns the tuple `{Item, Heap2}' where `Item' is the front item of `Heap', or `empty' if `Heap' is empty
%%
%% `Heap2' is always equivalent to `Heap'
-spec peek(Heap :: heap(Item)) -> {Item, Heap2 :: heap(Item)} | empty.
peek({Set, _, _} = Heap) ->
    case gb_sets:is_empty(Set) of
        true  -> empty;
        false -> {element(1, gb_sets:smallest(Set)), Heap}
    end.

%% @doc Returns the merged heap of `Heap1' and `Heap2'
-spec merge(Heap1 :: heap(Item1), Heap2 :: heap(Item2)) -> heap(Item1|Item2).
merge({Set1, _, _}, {Set2, _, _}) -> {gb_sets:union(Set1, Set2), 0, make_ref()}.

%% @doc Folds `Function' over every item in `Heap' returing the final value of the accumulator
%%
%% NOTE: The iteration order is undefined
-spec fold(Function :: heaps:fold_fun(Item), AccInitial :: term(), heap(Item)) -> AccResult :: term().
fold(Fun, Initial, {Set, _, _}) -> gb_sets:fold(fun ({Item, _, _}, Acc) -> Fun(Item, Acc) end, Initial, Set).
