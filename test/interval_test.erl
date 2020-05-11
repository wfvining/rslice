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

interval_span_too_long_test_() ->
    [?_assertThrow(
        {badarg, _},
        interval:span(
          rational:new(1, 2),
          interval:new(rational:new(0), rational:new(0)))),
     ?_assertThrow(
        {badarg, _},
        interval:span(
          rational:new(1, 2),
          interval:new(rational:new(1, 3), rational:new(2, 3)))),
     ?_assertThrow(
        {badarg, _},
        interval:span(
          rational:new(-1, 2),
          interval:new(rational:new(0), rational:new(1))))].

interval_span_test_() ->
    I = interval:new(rational:new(0), rational:new(1)),
    [?_assertEqual(
        {interval:new(rational:new(0), rational:new(0)), I},
        interval:span(rational:new(0), I)),
     ?_assertEqual(
        I, interval:span(rational:new(1), I)),
     ?_assertEqual(
        {interval:new(rational:new(0), rational:new(1, 2)),
         interval:new(rational:new(1, 2), rational:new(1))},
        interval:span(rational:new(1, 2), I))].

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
