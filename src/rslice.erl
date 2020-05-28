-module(rslice).
-export([new/0, join/2, owner/2, owner/3]).
-export_type([table/0]).

-type node_id() :: any().
-type key() :: iodata().
-type capacity() :: pos_integer().

-record(slice, {table = interval_table:new() :: interval_table:table(),
                nodes = #{} :: #{node_id() => capacity()}}).

-opaque table() :: #slice{}.

%% 2^128 - 1
-define(MAX_HASH, 340282366920938463463374607431768211455).

%%%% API Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%% @doc Get the node that "owns"  a given key.
-spec owner(key(), table()) -> node_id().
owner(Key, SliceTable) ->
    hd(owner(Key, 1, SliceTable)).

%% @doc Get the nodes that "own" a given key.
-spec owner(Key::key(), NumOwners::pos_integer(), Table::table()) -> list(node_id()).
owner(Key, NumOwners, #slice{table = IntervalTable, nodes = Nodes})
  when NumOwners =< map_size(Nodes) ->
    owner(Key, NumOwners, IntervalTable, hash_init(), []).

%%%% Internal functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sum_values(Map) ->
    lists:sum(maps:values(Map)).

owner(_, 0, _, _, Owners) ->
    lists:reverse(Owners);
owner(Key, Count, IntervalTable, HashState, Owners) ->
    {Hash, NewState} = hash(Key, HashState),
    {ok, Owner} = interval_table:lookup(Hash, IntervalTable),
    case lists:member(Owner, Owners) of
        true ->
            owner(Key, Count, IntervalTable, NewState, Owners);
        false ->
            owner(Key, Count - 1, IntervalTable, NewState, [Owner|Owners])
    end.

hash_init() ->
    crypto:hash_init(sha256).

hash(Key, State) ->
    HashState = crypto:hash_update(State, Key),
    <<Hash:128, _/binary>> = crypto:hash_final(HashState),
    {rational:new(Hash, ?MAX_HASH), HashState}.
