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
