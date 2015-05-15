%% Copyright (c) 2015, Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Heaps implmented by lists module
-module(list_heaps).

-behaviour(heaps).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/0, is_empty/1, in/2, out/1, merge/2, fold/3]).

-export_type([heap/0, heap/1, item/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-opaque heap(Item) :: ordsets:ordset(Item).
-type heap() :: heap(item()).
-type item() :: term().

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
-spec in(Item, heap(Item)) -> heap().
in(Item, [])                            -> [Item];
in(Item, Heap = [X | _]) when Item =< X -> [Item | Heap];
in(Item, [X | Heap])                    -> [X | in(Item, Heap)].

%% @doc Removes the smallest item from the heap `Heap'
%%
%% Returns the tuple `{Item, Heap2}', where `Item' is the item removed and `Heap2' is the resulting heap.
%% If `Heap' is empty, the tuple `empty' is returned.
-spec out(Heap :: heap(Item)) -> {Item, Heap2 :: heap(Item)} | empty.
out([])            -> empty;
out([Item | Heap]) -> {Item, Heap}.

%% @doc Returns the merged heap of `Heap1' and `Heap2'
-spec merge(Heap1 :: heap(Item1), Heap2 :: heap(Item2)) -> heap(Item1|Item2).
merge(Heap1, Heap2) -> lists:merge(Heap1, Heap2).

%% @doc Folds `Function' over every item in `Heap' returing the final value of the accumulator
-spec fold(Function :: heaps:fold_fun(Item), AccInitial :: term(), heap(Item)) -> AccResult :: term().
fold(Fun, Acc, Heap) -> lists:foldl(Fun, Acc, Heap).
