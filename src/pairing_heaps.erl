-module(pairing_heaps).

-export([
         new/0,
         is_empty/1,
         in/2,
         out/1
        ]).

-export_type([
              heap/0,
              elem/0
             ]).

-type elem() :: term().
-opaque heap() :: empty | {elem(), [heap()]}.

-spec new() -> heap().
new() -> empty.

-spec is_empty(heap()) -> boolean().
is_empty(empty) -> true;
is_empty(_)     -> false.

-spec in(elem(), heap()) -> heap().
in(Elem, Heap) ->
    merge({Elem, []}, Heap).

-spec out(heap()) -> {{value, elem()}, heap()} | {empty, heap()}.
out(empty) -> {empty, empty};
out({Elem, Heap}) -> {{value, Elem}, merge_pairs(Heap)}.

-spec merge(heap(), heap()) -> heap().
merge(H, empty) -> H;
merge(empty, H) -> H;
merge(H1 = {X, Hs1}, H2 = {Y, Hs2}) ->
    case X < Y of
        true  -> {X, [H2 | Hs1]};
        false -> {Y, [H1 | Hs2]}
    end.

-spec merge_pairs([heap()]) -> heap().
merge_pairs([]) -> empty;
merge_pairs([H]) -> H;
merge_pairs([H1, H2 | Hs]) ->
    merge(merge(H1, H2), merge_pairs(Hs)).
