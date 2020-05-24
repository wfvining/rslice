-module(rslice_test).
-include_lib("eunit/include/eunit.hrl").

-define(NUM_NODES, 20).

join_nodes_test_() ->
    {"Can join nodes to the slice table",
     {setup,
      fun rslice:new/0,
      fun(SliceTable) ->
              [?_test(rslice:join(#{foo => 100}, SliceTable)),
               ?_test(rslice:join(#{foo => 100, bar => 100}, SliceTable)),
               ?_test(rslice:join(#{foo => 10, bar => 100}, SliceTable)),
               ?_test(rslice:join(#{}, SliceTable)),
               ?_test(
                  rslice:join(
                    #{baz => 200},
                    rslice:join(#{foo => 100, bar => 100}, SliceTable)))]
      end}}.

lookup_key_test_() ->
    {"Can look up the responsible node for a key",
     {setup,
      fun() -> rslice:join(#{foo => 100}, rslice:new()) end,
      fun(SliceTable) ->
              [?_assertEqual(foo, rslice:owner("test", SliceTable)),
               {with, SliceTable, [fun lookup_two_keys/1]}]
      end}}.

lookup_two_keys(SliceTable) ->
    Table = rslice:join(#{bar => 200}, SliceTable),
    Owners = [rslice:owner(
                io_lib:format("key-~3..0B", [Key]), Table) ||
                 Key <- lists:seq(1, 100)],
    [?_assert(lists:member(foo, Owners)),
     ?_assert(lists:member(bar, Owners)),
     ?_assert(math:abs(50 - length(Owners -- [foo])) < 5),
     ?_assert(math:abs(50 - length(Owners -- [bar])) < 5)].

multiple_owners_test_() ->
    {setup,
     fun many_nodes/0,
     fun(SliceTable) ->
             [{"correct number of owners are returned",
               fun() ->
                       [?_assertEqual(
                           1,
                           length(rslice:owner("foo", 1, SliceTable))),
                        ?_assertEqual(
                           12,
                           length(rslice:owner("foo", 12, SliceTable)))]
               end},
              {"crash when more owners are requested than there "
               "are nodes in the slice table",
               fun() ->
                       ?assertException(
                          _, _,
                          rslice:owner("foo", ?NUM_NODES + 1, SliceTable))
               end},
              {"all returned owners are unique",
               fun() ->
                       Owners = rslice:owner("foo", 12, SliceTable),
                       ?assertEqual(length(lists:usort(Owners)), length(Owners))
               end},
              {"all nodes are returned when the number of owners "
               "requested is same as number of nodes",
               fun() ->
                       Owners = rslice:owner("foo", ?NUM_NODES, SliceTable),
                       [?_assertEqual(?NUM_NODES, length(Owners)),
                        ?_assertEqual(
                           length(lists:usort(Owners)),
                           length(Owners))]
               end},
              {"an empty list is returned when zero owners are requested",
               fun() ->
                       ?assertEqual([], rslice:owner("foo", 0, SliceTable))
               end},
              {"decreasing the number of requested owners returns "
               "a prefix of the longer list",
               fun() ->
                       Owners1 = rslice:owner("foo", 1, SliceTable),
                       Owners5 = rslice:owner("foo", 5, SliceTable),
                       Owners10 = rslice:owner("foo", 10, SliceTable),
                       [?_assert(lists:prefix(Owners1, Owners5)),
                        ?_assert(lists:prefix(Owners5, Owners10))]
               end},
              {"different keys give different owners",
               fun() ->
                       OwnersFoo = rslice:owner("foo", 6, SliceTable),
                       OwnersBar = rslice:owner("bar", 6, SliceTable),
                       ?assertNotEqual(OwnersFoo, OwnersBar)
               end},
              {"Same owners for same key",
               fun() ->
                       OwnersFoo = rslice:owner("foo", 12, SliceTable),
                       ?assertEqual(OwnersFoo, rslice:owner("foo", 12, SliceTable))
               end}
             ]
     end}.

many_nodes() ->
    rslice:join(
      maps:from_list(
        [{integer_to_list(X), 100} || X <- lists:seq(0, (?NUM_NODES div 2) - 1)] ++
        [{integer_to_list(X), 200} || X <- lists:seq(?NUM_NODES div 2, ?NUM_NODES - 1)]),
      rslice:new()).
