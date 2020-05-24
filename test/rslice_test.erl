-module(rslice_test).
-include_lib("eunit/include/eunit.hrl").

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
     fun twenty_nodes/0,
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
               fun() -> ?assert(false) end},
              {"all returned owners are unique",
               fun() -> ?assert(false) end},
              {"all nodes are returned when the number of owners "
               "requested is same as number of nodes",
               fun() -> ?assert(false) end},
              {"an empty list is returned when zero owners are requested",
               fun() -> ?assert(false) end},
              {"decreasing the number of requested owners returns "
               "a prefix of the longer list",
               fun() -> ?assert(false) end}
             ]
     end}.

twenty_nodes() ->
    rslice:join(
      maps:from_list(
        [{integer_to_list(X), 100} || X <- lists:seq(0, 10)] ++
        [{integer_to_list(X), 200} || X <- lists:seq(10, 20)]),
      rslice:new()).
