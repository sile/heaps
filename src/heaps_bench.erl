%% Copyright (c) 2015, Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Benchmark module
-module(heaps_bench).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
%%-export([do_bench/0, print_bench/0]).
-export([
         bench_in/1,
         bench_out/1
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type microseconds() :: number().
-type average_elapsed_time() :: microseconds().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec bench_in([heaps:item()]) -> [{module(), average_elapsed_time()}].
bench_in(Items) ->
    [{Module, bench_in(Module, Items)} || Module <- modules()].

-spec bench_out([heaps:item()]) -> [{module(), average_elapsed_time()}].
bench_out(Items) ->
    [{Module, bench_out(Module, Items)} || Module <- modules()].

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec modules() -> [module()].
modules() -> [leftist_heaps, pairing_heaps, splay_heaps, gb_set_heaps, list_heaps].

-spec bench_in(module(), [heaps:item()]) -> average_elapsed_time().
bench_in(Module, Items) ->
    erlang:garbage_collect(),
    Elapsed = times(3, fun () -> from_list(Module, Items) end),
    Elapsed / 3 / length(Items).

-spec bench_out(module(), [heaps:item()]) -> average_elapsed_time().
bench_out(Module, Items) ->
    Heap = from_list(Module, Items),
    true = (length(Items) =:= size(Module, Heap)),

    erlang:garbage_collect(),
    Elapsed = times(3, fun () -> out_all(Module, Heap) end),
    Elapsed / 3 / length(Items).

-spec from_list(module(), [heaps:item()]) -> heaps:heap().
from_list(Module, Items) ->
    lists:foldl(fun Module:in/2, Module:new(), Items).

-spec out_all(module(), heaps:heap()) ->  ok.
out_all(Module, Heap) ->
    case Module:out(Heap) of
        {empty, _} -> ok;
        {_, Heap2} -> out_all(Module, Heap2)
    end.

-spec size(module(), heaps:heap()) -> non_neg_integer().
size(Module, Heap) ->
    Module:fold(fun (_, N) -> N + 1 end, 0, Heap).

-spec times(non_neg_integer(), fun()) -> microseconds().
times(N, Fun) ->
    Seq = lists:seq(1, N),
    Start = now(),
    ok = lists:foreach(fun (_) -> Fun() end, Seq),
    timer:now_diff(now(), Start).
