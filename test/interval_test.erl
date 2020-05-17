-module(interval_test).
-include_lib("eunit/include/eunit.hrl").

invalid_interval_test_() ->
    {"Creating an interval with left larger than right throws an exception.",
     fun() ->
             OneQuarter = rational:new(1, 4),
             OneHalf = rational:new(2, 4),
             ?assertThrow(
                {badarg, _},
                interval:new(OneHalf, OneQuarter)
               )
     end}.

interval_length_test_() ->
    {"Interval length is equal to 'right' minus 'left'",
     {setup,
      fun() -> {rational:new(0, 0), rational:new(1, 4)} end,
      fun({Zero, OneQuarter}) ->
              [?_assertEqual(
                  Zero,
                  interval:length(interval:new(OneQuarter, OneQuarter))),
               ?_assertEqual(
                  Zero,
                  interval:length(interval:new(Zero, Zero))
                 ),
               ?_assertEqual(
                  OneQuarter,
                  interval:length(interval:new(Zero, OneQuarter))
                 ),
               ?_assertEqual(
                 rational:new(1, 1),
                 interval:length(interval:new(
                                  Zero,
                                  rational:multiply(
                                    rational:new(4, 1),
                                    OneQuarter)))),
              ?_assertEqual(
                rational:new(1, 2),
                interval:length(interval:new(
                                 rational:new(-1, 4),
                                 rational:new(1, 4))))]
      end
     }}.

interval_contains_default_test_() ->
    {"contains/2 includes both endpoints",
     {setup,
      fun() ->
              interval:new(rational:new(0), rational:new(1))
      end,
      fun(Interval) ->
              [?_assert(interval:contains(rational:new(1, 2), Interval)),
               ?_assert(interval:contains(rational:new(1), Interval)),
               ?_assert(interval:contains(rational:new(0), Interval)),
               ?_assertNot(interval:contains(rational:new(3, 2), Interval)),
               ?_assertNot(interval:contains(rational:new(-1, 2), Interval)),
               ?_assert(interval:contains(
                          rational:new(0),
                          interval:new(rational:new(0), rational:new(0))))]
      end}}.

interval_split_too_long_test_() ->
    [?_assertEqual(
        {interval:new(rational:new(0), rational:new(0)), empty},
        interval:split(
          rational:new(1, 2),
          interval:new(rational:new(0), rational:new(0)))),
     ?_assertEqual(
        {interval:new(rational:new(1, 3), rational:new(2, 3)), empty},
        interval:split(
          rational:new(1, 2),
          interval:new(rational:new(1, 3), rational:new(2, 3))))].

interval_split_test_() ->
    I = interval:new(rational:new(0), rational:new(1)),
    [?_assertEqual(
        {empty, I},
        interval:split(rational:new(0), I)),
     ?_assertEqual(
        {I, empty}, interval:split(rational:new(1), I)),
     ?_assertEqual(
        {interval:new(rational:new(0), rational:new(1, 2)),
         interval:new(rational:new(1, 2), rational:new(1))},
        interval:split(rational:new(1, 2), I))].

interval_split_right_test_() ->
    {"can split intervals from the right",
     {setup,
     fun() ->
             interval:new(rational:new(0), rational:new(1))
     end,
     fun(Interval) ->
             [?_assertEqual(
                 {empty, Interval},
                 interval:split(rational:new(1), Interval, right)),
              ?_assertEqual(
                 {empty, Interval},
                 interval:split(rational:new(2), Interval, right)),
              ?_assertEqual(
                 interval:split(rational:new(1, 4), Interval, left),
                 interval:split(rational:new(3, 4), Interval, right)),
              ?_assertEqual(
                 interval:split(rational:new(3, 4), Interval, left),
                 interval:split(rational:new(1, 4), Interval, right)),
              ?_assertEqual(
                 {Interval, empty},
                 interval:split(rational:new(0), Interval, right))]
     end}}.

preceeds_test_() ->
    [?_assert(interval:preceeds(
                interval:new(rational:new(0), rational:new(1, 2)),
                interval:new(rational:new(1, 2), rational:new(1)))),
     ?_assert(interval:preceeds(
                interval:new(rational:new(0), rational:new(1, 4)),
                interval:new(rational:new(1, 2), rational:new(1)))),
     ?_assertNot(interval:preceeds(
                   interval:new(rational:new(1, 2), rational:new(1)),
                   interval:new(rational:new(1, 2), rational:new(3, 2)))),
     ?_assertNot(interval:preceeds(
                   interval:new(rational:new(0), rational:new(1)),
                   interval:new(rational:new(0), rational:new(1)))),
     ?_assertNot(interval:preceeds(
                   interval:new(rational:new(1, 2), rational:new(1)),
                   interval:new(rational:new(0), rational:new(1, 2)))),
     ?_assertNot(interval:preceeds(
                   interval:new(rational:new(1, 2), rational:new(3, 4)),
                   interval:new(rational:new(0), rational:new(1)))),
     ?_assertNot(interval:preceeds(
                   interval:new(rational:new(0), rational:new(1)),
                   interval:new(rational:new(1, 2), rational:new(3, 4))))].

adjacent_test_() ->
    [?_assert(
        interval:adjacent(
          interval:new(rational:new(0), rational:new(1, 2)),
          interval:new(rational:new(1, 2), rational:new(1)))),
     ?_assert(
        interval:adjacent(
          interval:new(rational:new(1, 2), rational:new(1)),
          interval:new(rational:new(0), rational:new(1, 2)))),
     ?_assertNot(
        interval:adjacent(
          interval:new(rational:new(1, 3), rational:new(2, 3)),
          interval:new(rational:new(3, 4), rational:new(1))))].

merge_test_() ->
    [?_assertEqual(
        interval:new(rational:new(0), rational:new(1)),
        interval:merge(
          interval:new(rational:new(0), rational:new(1, 3)),
          interval:new(rational:new(1, 3), rational:new(1)))),
     ?_assertEqual(
        interval:new(rational:new(0), rational:new(1)),
        interval:merge(
          interval:new(rational:new(1, 3), rational:new(1)),
          interval:new(rational:new(0), rational:new(1, 3)))),
     ?_assertEqual(
        interval:new(rational:new(1, 3), rational:new(1, 2)),
        interval:merge(
          interval:new(rational:new(1, 2), rational:new(1, 2)),
          interval:new(rational:new(1, 3), rational:new(1, 2)))),
     ?_assertThrow(
        {badarg, "intervals must be adjacent"},
        interval:merge(
          interval:new(rational:new(1, 2), rational:new(2, 3)),
          interval:new(rational:new(3, 4), rational:new(1)))),
     ?_assertThrow(
        {badarg, "intervals must be adjacent"},
        interval:merge(
          interval:new(rational:new(3, 4), rational:new(1)),
          interval:new(rational:new(1, 2), rational:new(2, 3))))].
