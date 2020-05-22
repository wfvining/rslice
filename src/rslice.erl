-module(rslice).
-export([new/0, join/2, owner/2]).
-export_type([table/0]).

-type node_id() :: any().
-type key() :: iodata().
-type capacity() :: pos_integer().

-record(slice, {table = interval_table:new() :: interval_table:table(),
                nodes = #{} :: #{node_id() => capacity()}}).

-opaque table() :: #slice{}.

%% 2^128 - 1
-define(MAX_HASH, 340282366920938463463374607431768211455).

%% @doc Create a new (empty) random slicing table.
-spec new() -> table().
new() ->
    #slice{}.

%% @doc Join keys in `Keys' to the slice table.
-spec join(#{node_id() => capacity()}, table()) -> table().
join(NewKeys, Table=#slice{nodes=Nodes, table=IntervalTable}) ->
    CurrentTotalCapacity = sum_values(Nodes),
    NewTotalCapacity = CurrentTotalCapacity + sum_values(NewKeys),
    CapacityChanges =
        maps:map(
          fun(_, Capacity) ->
                  rational:subtract(
                    rational:new(Capacity, CurrentTotalCapacity),
                    rational:new(Capacity, NewTotalCapacity))
          end, Nodes),
    NewRelativeCapacity =
        maps:map(fun(_, Capacity) ->
                         rational:new(Capacity, NewTotalCapacity)
                 end, NewKeys),
    Table#slice{
      nodes = maps:merge(Nodes, NewKeys),
      table =
          interval_table:assign(
            NewRelativeCapacity,
            interval_table:shrink(CapacityChanges, IntervalTable))}.

sum_values(Map) ->
    maps:fold(fun(_, X, Acc) ->
                      X + Acc
              end, 0, Map).

-spec owner(key(), table()) -> node_id().
owner(Key, #slice{table = IntervalTable}) ->
    KeyHash = hash(Key),
    {ok, Node} = interval_table:lookup(KeyHash, IntervalTable),
    Node.

-spec hash(key()) -> rational:rational().
hash(Key) ->
    HashState = crypto:hash_update(crypto:hash_init(blake2b), Key),
    <<Hash:128, _/binary>> = crypto:hash_final(HashState),
    rational:new(Hash, ?MAX_HASH).
