-module(interval_table_test).
-include_lib("eunit/include/eunit.hrl").

new_table_test_() ->
    {"Can create a new interval table.",
     fun() ->
         interval_table:new()
     end}.

new_table_gaps_test_() ->
    {"A new table has gaps covering the full range [0, 1]",
     fun() ->
         Table = interval_table:new(),
         ?assertEqual(rational:new(1), interval_table:gap_size(Table))
      end}.

assign_gaps_test_() ->
    {"Can assign a gaps to a key with capacity <= gap size for the table.",
     {setup,
      fun interval_table:new/0,
      fun assign_gaps/1
     }}.

over_assign_gaps_test_() ->
    {"Assign a key with capacity > gap size raises an exception.",
     {setup,
      fun interval_table:new/0,
      fun over_assign_gaps/1
     }}.

lookup_gap_test_() ->
    {"Looking up a point that lies in a gap returns \'unassigned\'",
     {setup,
      fun interval_table:new/0,
      fun lookup_gap/1
     }}.

lookup_assigned_test_() ->
    {"Looking up a point that lies in an assigned interval returns the assigned key",
     {setup,
      fun() ->
              interval_table:assign(
                foo,
                rational:new(1),
                interval_table:new())
      end,
      fun(Table) ->
              ?_assertEqual(
                 [{ok, foo}, {ok, foo}, {ok, foo}, {ok, foo}, {ok, foo}],
                 [interval_table:lookup(rational:new(N, D), Table) ||
                     {N, D} <- [{0, 1}, {1, 4}, {1, 2}, {3, 4}, {1, 1}]])
      end}}.

lookup_two_assigned_test_() ->
    {"looking values in two assigned intervals returns the two keys",
     {setup,
      fun() ->
              interval_table:assign(
                bar,
                rational:new(1, 2),
                interval_table:assign(
                  foo,
                  rational:new(1, 2),
                  interval_table:new()
                 ))
      end,
      fun(Table) ->
              [?_assertEqual(
                  {ok, foo},
                  interval_table:lookup(rational:new(0), Table)),
               ?_assertEqual(
                  {ok, foo},
                  interval_table:lookup(rational:new(1, 2), Table)),
               ?_assertEqual(
                  {ok, bar},
                  interval_table:lookup(rational:new(3, 4), Table)),
               ?_assertEqual(
                  {ok, bar},
                  interval_table:lookup(rational:new(1), Table))]
      end
     }}.

assign_gaps(Table) ->
    [?_assertEqual(rational:new(0),
                   interval_table:gap_size(
                     interval_table:assign("a", rational:new(1), Table))),
     ?_assertEqual(rational:new(3, 4),
                   interval_table:gap_size(
                     interval_table:assign("a", rational:new(1, 4), Table))),
     ?_assertEqual(rational:new(1, 4),
                   interval_table:gap_size(
                     interval_table:assign("a", rational:new(3, 4), Table))),
     ?_assertEqual(interval_table:gap_size(Table),
                   interval_table:gap_size(
                     interval_table:assign("a", rational:new(0), Table))),
     ?_assertEqual(rational:new(0),
                   interval_table:gap_size(
                     interval_table:assign(
                       "b", rational:new(1, 4),
                       interval_table:assign(
                         "a", rational:new(3, 4), Table))))].

over_assign_gaps(Table) ->
    [?_assertThrow({badarg, _},
                   interval_table:assign("foo", rational:new(3, 2), Table)),
     ?_assertThrow({badarg, _},
                   interval_table:assign(
                     "foo", rational:new(1, 2),
                     interval_table:assign(
                       "bar", rational:new(3, 4), Table)))].

lookup_gap(Table) ->
    [?_assertEqual(unassigned,
                   interval_table:lookup(rational:new(1, 2), Table)),
     ?_assertEqual(unassigned,
                   interval_table:lookup(rational:new(0), Table)),
     ?_assertEqual(unassigned,
                   interval_table:lookup(rational:new(1), Table)),
     ?_assertEqual(unassigned,
                   interval_table:lookup(
                     rational:new(3, 4),
                     interval_table:assign(foo, rational:new(1, 2), Table)))].
