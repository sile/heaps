%% Copyright (c) 2015, Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Heaps tests
-module(heaps_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(HEAP_MODULES, [leftist_heaps, pairing_heaps, list_heaps, gb_set_heaps, splay_heaps, binomial_heaps]).
-define(TEST_FOREACH_MODULE(Title, Fun),
        [begin
             {"["++atom_to_list(__Module__)++"] "++Title, fun () -> Fun(__Module__) end}
         end || __Module__ <- ?HEAP_MODULES]).

-define(assignMatch(Pattern, Exp),
        begin
            %% The meaningless equality checking guard suppresses "variable VAR_NAME is unused" compiler warnings
            ?assertMatch(Pattern when Pattern == Pattern, Exp),
            Pattern = Exp
        end).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    ?TEST_FOREACH_MODULE(
       "Creates an empty heap",
       fun (Module) ->
               Heap = Module:new(),
               ?assert(Module:is_empty(Heap))
       end).

int_out_test_() ->
    ?TEST_FOREACH_MODULE(
       "Inserts an item",
       fun (Module) ->
               H0 = Module:in(aaa, Module:new()),
               ?assert(not Module:is_empty(H0))
       end) ++
    ?TEST_FOREACH_MODULE(
       "Try removing item from a empty heap",
       fun (Module) ->
               H0 = Module:new(),
               ?assertEqual(empty, Module:out(H0))
       end) ++
    ?TEST_FOREACH_MODULE(
       "Inserts an item then removes it",
       fun (Module) ->
               H0 = Module:in(aaa, Module:new()),
               ?assignMatch({aaa, H1}, Module:out(H0)),
               ?assert(Module:is_empty(H1))
       end) ++
    ?TEST_FOREACH_MODULE(
       "Inserts items then removes them",
       fun (Module) ->
               Items = [3, 100, 0, 1],
               H0 = lists:foldl(fun Module:in/2, Module:new(), Items),

               %% The smaller the item, the earlier it is removed from the heap
               ?assignMatch({  0, H1}, Module:out(H0)),
               ?assignMatch({  1, H2}, Module:out(H1)),
               ?assignMatch({  3, H3}, Module:out(H2)),
               ?assignMatch({100, H4}, Module:out(H3)),
               ?assertEqual(empty, Module:out(H4))
       end) ++
    ?TEST_FOREACH_MODULE(
       "Inserts duplicated items",
       fun (Module) ->
               Items = [aaa, aaa],
               H0 = lists:foldl(fun Module:in/2, Module:new(), Items),

               ?assignMatch({aaa, H1}, Module:out(H0)),
               ?assignMatch({aaa, H2}, Module:out(H1)),
               ?assert(Module:is_empty(H2))
       end) ++
    ?TEST_FOREACH_MODULE(
       "Inserts items then removes them (large input)",
       fun (Module) ->
               Items = [random:uniform() || _ <- lists:seq(1, 1000)],
               ResultHeap =
                   lists:foldl(
                     fun (ExpectedItem, H0) ->
                             ?assignMatch({ExpectedItem, H1}, Module:out(H0)),
                             H1
                     end,
                     lists:foldl(fun Module:in/2, Module:new(), Items),
                     lists:sort(Items)),
               ?assert(Module:is_empty(ResultHeap))
       end).

peek_test_() ->
    ?TEST_FOREACH_MODULE(
       "Try peeking item from a empty heap",
       fun (Module) ->
               H0 = Module:new(),
               ?assertEqual(empty, Module:peek(H0))
       end) ++
    ?TEST_FOREACH_MODULE(
       "Inserts an item then peeks it",
       fun (Module) ->
               H0 = Module:in(aaa, Module:new()),
               ?assignMatch({aaa, H1}, Module:peek(H0)),
               ?assert(not Module:is_empty(H1)) % No item is consumed
       end).

merge_test_() ->
    ?TEST_FOREACH_MODULE(
       "Merges two heaps",
       fun (Module) ->
               H_A = lists:foldl(fun Module:in/2, Module:new(), [3, 0, 9]),
               H_B = lists:foldl(fun Module:in/2, Module:new(), [0, 7]),
               H0 = Module:merge(H_A, H_B),
               ?assignMatch({0, H1}, Module:out(H0)),
               ?assignMatch({0, H2}, Module:out(H1)),
               ?assignMatch({3, H3}, Module:out(H2)),
               ?assignMatch({7, H4}, Module:out(H3)),
               ?assignMatch({9, H5}, Module:out(H4)),
               ?assert(Module:is_empty(H5))
       end).

fold_test_() ->
    ?TEST_FOREACH_MODULE(
       "Counts heap items",
       fun (Module) ->
               Items = [3, 100, 0, 1],
               H0 = lists:foldl(fun Module:in/2, Module:new(), Items),
               ?assertEqual(length(Items), Module:fold(fun (_, C) -> C + 1 end, 0, H0))
       end).

size_test_() ->
    ?TEST_FOREACH_MODULE(
       "Calculates heap size",
       fun (Module) ->
               H0 = Module:new(),
               ?assertEqual(0, heaps:size(Module, H0)),

               H1 = Module:in(1, H0),
               ?assertEqual(1, heaps:size(Module, H1)),

               H2 = Module:in(2, H1),
               ?assertEqual(2, heaps:size(Module, H2))
       end).

to_list_test_() ->
    ?TEST_FOREACH_MODULE(
       "Converts to a list",
       fun (Module) ->
               H0 = Module:in(1, Module:new()),
               ?assertEqual([1], heaps:to_list(Module, H0))
       end).

to_ordlist_test_() ->
    ?TEST_FOREACH_MODULE(
       "Converts to an ordered list",
       fun (Module) ->
               H0 = Module:in(3, Module:in(10, Module:in(1, Module:new()))),
               ?assertEqual([1, 3, 10], heaps:to_ordlist(Module, H0))
       end).

from_list_test_() ->
    ?TEST_FOREACH_MODULE(
       "Makes from a list",
       fun (Module) ->
               Items = [10, 1, 3],
               H0 = heaps:from_list(Module, Items),
               ?assertEqual(lists:sort(Items), heaps:to_ordlist(Module, H0))
       end).
