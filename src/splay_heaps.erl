%% Copyright (c) 2015, Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Splay heaps
-module(splay_heaps).

-compile(inline).

-behaviour(heaps).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/0, is_empty/1, in/2, out/1, merge/2, fold/3]).

-export_type([heap/0, heap/1, item/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-opaque heap(Item) :: maybe_tree_node(Item).
-type heap() :: heap(item()).
-type item() :: term().

-type maybe_tree_node(Item) :: tree_node(Item) | nil.
-type tree_node(Item)       :: {Item, maybe_tree_node(Item), maybe_tree_node(Item)}.
-type direction()           :: lft | rgt.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc Returns an empty heap
-spec new() -> heap().
new() -> nil.

%% @doc Tests if `Heap' is empty and returns `true' if so and `false' otherwise
-spec is_empty(Heap :: heap()) -> boolean().
is_empty(nil) -> true;
is_empty(_)   -> false.

%% @doc Inserts `Item' into the heap `Heap'
%%
%% Returns the resulting heap
-spec in(Item, Heap :: heap(Item)) -> heap(Item).
in(Item, Heap) ->
    splay(leaf(Item), path_to_node(Item, Heap, [])).

%% @doc Removes the smallest item from the heap `Heap'
%%
%% Returns the tuple `{{value, Item}, Heap2}', where `Item' is the item removed and `Heap2' is the resulting heap.
%% If `Heap' is empty, the tuple `{empty, Heap}' is returned.
-spec out(Heap :: heap(Item)) -> {{value, Item}, Heap2 :: heap(Item)} | {empty, Heap :: heap(Item)}.
out(nil)              -> {empty, nil};
out({Item, nil, Rgt}) -> {{value, Item}, Rgt};
out(Heap)             -> out(lft(Heap), [Heap]).

%% @doc Returns the merged heap of `Heap1' and `Heap2'
-spec merge(Heap1 :: heap(Item1), Heap2 :: heap(Item2)) -> heap(Item1|Item2).
merge(Heap1, Heap2) -> fold(fun in/2, Heap1, Heap2).

%% @doc Folds `Function' over every item in `Heap' returing the final value of the accumulator
-spec fold(heaps:fold_fun(Item), term(), heap(Item)) -> term().
fold(Fun, Acc0, Tree) -> fold_node(Fun, Tree, Acc0).

%%--------------------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------------------
-spec item(tree_node(Item)) -> Item.
item({Item, _, _}) -> Item.

-spec lft(tree_node(Item)) -> maybe_tree_node(Item).
lft({_, Lft, _}) -> Lft.

-spec lft(tree_node(Item), maybe_tree_node(Item)) -> tree_node(Item).
lft(Node, Lft) -> setelement(2, Node, Lft).

-spec rgt(tree_node(Item)) -> maybe_tree_node(Item).
rgt({_, _, Rgt}) -> Rgt.

-spec rgt(tree_node(Item), maybe_tree_node(Item)) -> tree_node(Item).
rgt(Node, Rgt) -> setelement(3, Node, Rgt).

-spec lft_rgt(tree_node(Item), maybe_tree_node(Item), maybe_tree_node(Item)) -> tree_node(Item).
lft_rgt(Node, Lft, Rgt) -> {item(Node), Lft, Rgt}.

-spec rgt_lft(tree_node(Item), maybe_tree_node(Item), maybe_tree_node(Item)) -> tree_node(Item).
rgt_lft(Node, Rgt, Lft) -> {item(Node), Lft, Rgt}.

-spec leaf(Item) -> tree_node(Item).
leaf(Item) -> {Item, nil, nil}.

-spec path_to_node(Item, maybe_tree_node(Item), [{direction(),tree_node(Item)}]) -> [{direction(),tree_node(Item)}].
path_to_node(_Item, nil, Path) -> Path;
path_to_node(Item, Node, Path) ->
    case item(Node) of
        I when Item =< I -> path_to_node(Item, lft(Node), [{lft,Node}|Path]);
        I when Item  > I -> path_to_node(Item, rgt(Node), [{rgt,Node}|Path])
    end.

-spec out(tree_node(Item), [tree_node(Item)]) -> {{value, Item}, tree_node(Item)}.
out({Item, nil, Rgt}, [Node | Path]) -> {{value, Item}, splay_lft(lft(Node,Rgt), Path)};
out(Node,             Path)          -> out(lft(Node), [Node|Path]).

-spec splay_lft(tree_node(Item), [tree_node(Item)]) -> tree_node(Item).
splay_lft(X, [])            -> X;
splay_lft(X, [P])           -> rgt(X, lft(P, rgt(X))); % zig
splay_lft(X, [P, G | Path]) -> splay_lft(rgt(X, rgt_lft(P, lft(G, rgt(P)), rgt(X))), Path). % zig-zig

-spec splay(tree_node(Item), [{direction(),tree_node(Item)}]) -> tree_node(Item).
splay(X, []) -> X;
splay(X, [{Dir, P}]) ->                % zig
    case Dir of
        lft -> rgt(X, lft(P, rgt(X)));
        rgt -> lft(X, rgt(P, lft(X)))
    end;
splay(X, [{Dir,P}, {Dir,G} | Path]) -> % zig-zig
    splay(case Dir of
              lft -> rgt(X, rgt_lft(P, lft(G, rgt(P)), rgt(X)));
              rgt -> lft(X, lft_rgt(P, rgt(G, lft(P)), lft(X)))
          end,
          Path);
splay(X, [{Dir,P}, {_,G} | Path]) ->   % zig-zag
    splay(case Dir of
              lft -> rgt_lft(X, lft(P, rgt(X)), rgt(G, lft(X)));
              rgt -> lft_rgt(X, rgt(P, lft(X)), lft(G, rgt(X)))
          end,
          Path).

-spec fold_node(heaps:fold_fun(Item), maybe_tree_node(Item), term()) -> term().
fold_node(_Fun, nil, Acc)             -> Acc;
fold_node(Fun, {Item, Lft, Rgt}, Acc) -> fold_node(Fun, Rgt, Fun(Item, fold_node(Fun, Lft, Acc))).
