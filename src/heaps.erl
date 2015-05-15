%% Copyright (c) 2015, Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Interface for heaps
-module(heaps).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
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
