-module(leftist_heaps).

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

-opaque heap() :: empty | {rank(), elem(), heap(), heap()}.
-type elem() :: term().

-type rank() :: non_neg_integer().

-spec new() -> heap().
new() -> empty.

-spec is_empty(heap()) -> boolean().
is_empty(empty) -> true;
is_empty(_)     -> false.

-spec in(elem(), heap()) -> heap().
in(Elem, Heap) ->
    merge({1, Elem, empty, empty}, Heap).

-spec out(heap()) -> {{value, elem()}, heap()} | {empty, heap()}.
out(empty)                   -> {empty, empty};
out({_, Elem, Heap1, Heap2}) -> {{value, Elem}, merge(Heap1, Heap2)}.

-spec merge(heap(), heap()) -> heap().
merge(empty, Heap2) -> Heap2;
merge(Heap1, empty) -> Heap1;
merge(Heap1, Heap2) ->
    {_, X, A1, B1} = Heap1,
    {_, Y, A2, B2} = Heap2,
    case X < Y of
        true  -> make_node(X, A1, merge(B1, Heap2));
        false -> make_node(Y, A2, merge(Heap1, B2))
    end.

-spec make_node(elem(), heap(), heap()) -> heap().
make_node(X, A, B) ->
    case rank(A) >= rank(B) of
        true  -> {rank(B) + 1, X, A, B};
        false -> {rank(A) + 1, X, B, A}
    end.

-spec rank(heap()) -> rank().
rank(empty)        -> 0;
rank({R, _, _, _}) -> R.
