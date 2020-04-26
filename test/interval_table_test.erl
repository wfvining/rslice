-module(interval_table_test).
-include_lib("eunit/include/eunit.hrl").

empty_test_() ->
    {"An empty interval table can be created.",
     fun() -> interval_table:empty() end}.

empty_size_test_() ->
    {"An empty interval table has size 0",
     {setup,
      fun interval_table:empty/0,
      fun(Empty) ->
              [?_assertEqual(0, interval_table:size(Empty))]
      end
     }}.
