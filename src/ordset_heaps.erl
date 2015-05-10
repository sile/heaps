%% Copyright (c) 2015, Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Heaps implmented by ordsets module
-module(ordset_heaps).

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
new() -> ordsets:new().

%% @doc Tests if `Heap' is empty and returns `true' if so and `false' otherwise
-spec is_empty(Heap :: heap()) -> boolean().
is_empty([]) -> true;
is_empty(_)  -> false.

%% @doc Inserts `Item' into the heap `Heap'
%%
%% Returns the resulting heap
-spec in(Item, heap(Item)) -> heap().
in(Item, Heap) -> ordsets:add_element(Item, Heap).

%% @doc Removes the smallest item from the heap `Heap'
%%
%% Returns the tuple `{{value, Item}, Heap2}', where `Item' is the item removed and `Heap2' is the resulting heap.
%% If `Heap' is empty, the tuple `{empty, Heap}' is returned.
-spec out(Heap :: heap(Item)) -> {{value, Item}, Heap2 :: heap(Item)} | {empty, Heap :: heap(Item)}.
out([])            -> {empty, []};
out([Item | Heap]) -> {{value, Item}, Heap}.

%% @doc Returns the merged heap of `Heap1' and `Heap2'
-spec merge(Heap1 :: heap(Item1), Heap2 :: heap(Item2)) -> heap(Item1|Item2).
merge(Heap1, Heap2) -> ordsets:union(Heap1, Heap2).

%% @doc Folds `Function' over every item in `Heap' returing the final value of the accumulator
%%
%% NOTE: The iteration order is undefined
-spec fold(Function :: heaps:fold_fun(Item), AccInitial :: term(), heap(Item)) -> AccResult :: term().
fold(Fun, Acc, Heap) -> ordsets:fold(Fun, Acc, Heap).
