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

               ?assertMatch({aaa, _}, Module:out(H0)),
               {_, H1} = Module:out(H0),

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
