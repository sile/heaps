%% Copyright (c) 2015, Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Interface for heaps
-module(heaps).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([size/2, to_list/2, to_ordlist/2, from_list/2, fold_in_order/4]).
-export_type([heap/0, item/0, fold_fun/0, fold_fun/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type heap() :: term().
-type item() :: term().
-type fold_fun() :: fold_fun(item()).
-type fold_fun(Item) :: fun ((Item, AccIn::term()) -> AccOut::term()).

%%----------------------------------------------------------------------------------------------------------------------
%% Callback API
%%----------------------------------------------------------------------------------------------------------------------
-callback new() -> heap().
-callback is_empty(heap()) -> boolean().
-callback in(item(), heap()) -> heap().
-callback out(heap()) -> {item(), heap()} | empty.
-callback peek(heap()) -> {item(), heap()} | empty.
-callback merge(heap(), heap()) -> heap().
-callback fold(fold_fun(), Initial::term(), heap()) -> Result::term().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc Calculates and returns the length of heap `Heap'
%%
%% Computational Complexity: `O(N)'
-spec size(module(), heap()) -> non_neg_integer().
size(Module, Heap) ->
    Module:fold(fun (_, N) -> N + 1 end, 0, Heap).

%% @doc Returns a list of the items in the heap
%%
%% The order of resulting list is unspecified
-spec to_list(module(), heap()) -> [item()].
to_list(Module, Heap) ->
    Module:fold(fun (X, Acc) -> [X | Acc] end, [], Heap).

%% @doc Returns a list of the items in the heap in the sorted order; the smallest item of the heap will become the head of the list
-spec to_ordlist(module(), heap()) -> [item()].
to_ordlist(Module, Heap) ->
    lists:reverse(fold_in_order(Module, fun (X, Acc) -> [X | Acc] end, [], Heap)).

%% @doc Returns a heap containing the items in `List'
-spec from_list(module(), [item()]) -> heap().
from_list(Module, List) ->
    lists:foldl(fun Module:in/2, Module:new(), List).

-spec fold_in_order(module(), fold_fun(), Initial::term(), heap()) -> Result::term().
fold_in_order(Module, Fun, Acc, Heap0) ->
    case Module:out(Heap0) of
        empty      -> Acc;
        {X, Heap1} -> fold_in_order(Module, Fun, Fun(X, Acc), Heap1)
    end.
