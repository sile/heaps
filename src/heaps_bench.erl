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
         bench_out/1,
         bench_in_out/1,
         bench_merge/1
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types & Macros
%%----------------------------------------------------------------------------------------------------------------------
-type microseconds() :: number().
-type average_elapsed_time() :: microseconds().

-define(LOOP_COUNT, 3).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec bench_in([heaps:item()]) -> [{module(), average_elapsed_time()}].
bench_in(Items) ->
    [{Module, bench_in(Module, Items)} || Module <- modules()].

-spec bench_out([heaps:item()]) -> [{module(), average_elapsed_time()}].
bench_out(Items) ->
    [{Module, bench_out(Module, Items)} || Module <- modules()].

-spec bench_in_out([Spec]) -> [{module(), average_elapsed_time()}] when
      Spec :: {in, heaps:item()} | out.
bench_in_out(Specs) ->
    [{Module, bench_in_out(Module, Specs)} || Module <- modules()].

-spec bench_merge([Pair]) -> [{module(), average_elapsed_time()}] when
      Pair :: {[heaps:item()], [heaps:item()]}.
bench_merge(Pairs) ->
    [{Module, bench_merge(Module, Pairs)} || Module <- modules()].

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec modules() -> [module()].
modules() -> [leftist_heaps, pairing_heaps, splay_heaps, gb_set_heaps, list_heaps, binomial_heaps].

-spec bench_in(module(), [heaps:item()]) -> average_elapsed_time().
bench_in(Module, Items) ->
    erlang:garbage_collect(),
    Elapsed = times(?LOOP_COUNT, fun () -> from_list(Module, Items) end),
    Elapsed / ?LOOP_COUNT / length(Items).

-spec bench_merge(module(), [Pair]) -> average_elapsed_time() when
      Pair :: {[heaps:item()], [heaps:item()]}.
bench_merge(Module, Pairs) ->
    HeapPairs = [{from_list(Module, A), from_list(Module, B)} || {A, B} <- Pairs],
    erlang:garbage_collect(),
    Elapsed = times(?LOOP_COUNT, fun () -> lists:foreach(fun ({A, B}) -> Module:merge(A, B) end, HeapPairs) end),
    Elapsed / ?LOOP_COUNT / length(Pairs).

-spec bench_out(module(), [heaps:item()]) -> average_elapsed_time().
bench_out(Module, Items) ->
    Heap = from_list(Module, Items),
    true = (length(Items) =:= size(Module, Heap)),

    erlang:garbage_collect(),
    Elapsed = times(?LOOP_COUNT, fun () -> out_all(Module, Heap) end),
    Elapsed / ?LOOP_COUNT / length(Items).

-spec bench_in_out(module(), [Spec]) -> average_elapsed_time() when
      Spec  :: {in, heaps:item()} | out.
bench_in_out(Module, Specs) ->
    Heap0 = Module:new(),
    erlang:garbage_collect(),
    Elapsed = times(?LOOP_COUNT, fun () -> in_out_all(Module, Heap0, Specs) end),
    Elapsed / ?LOOP_COUNT / length(Specs).

-spec in_out_all(module(), heaps:heap(), [Spec]) -> ok when
      Spec  :: {in, heaps:item()} | out.
in_out_all(_, _, [])                     -> ok;
in_out_all(Module, Heap, [Spec | Specs]) ->
    case Spec of
        out        -> in_out_all(Module, case Module:out(Heap) of {_, Heap1} -> Heap1; empty -> Heap end, Specs);
        {in, Item} -> in_out_all(Module, Module:in(Item, Heap), Specs)
    end.

-spec from_list(module(), [heaps:item()]) -> heaps:heap().
from_list(Module, Items) ->
    lists:foldl(fun Module:in/2, Module:new(), Items).

-spec out_all(module(), heaps:heap()) ->  ok.
out_all(Module, Heap) ->
    case Module:out(Heap) of
        empty      -> ok;
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
