-module(rational_test).
-include_lib("eunit/include/eunit.hrl").

equal_test_() ->
    [?_assertEqual(rational:new(1, 2), rational:new(1, 2)),
     ?_assertEqual(rational:new(1, 2), rational:new(2, 4)),
     ?_assertEqual(rational:new(2, 4), rational:new(1, 2)),
     ?_assertEqual(rational:new(0, 0), rational:new(0, 5)),
     ?_assertNotEqual(rational:new(2, 3), rational:new(3, 2)),
     ?_assertNotEqual(rational:new(1, 2), rational:new(0, 2))].

add_test_() ->
    [?_assertEqual(rational:new(1, 2),
                   rational:add(rational:new(1, 4), rational:new(1, 4))),
     ?_assertEqual(rational:new(1, 2),
                   rational:add(rational:new(-1, 4), rational:new(3, 4))),
     ?_assertEqual(rational:new(2, 1),
                   rational:add(rational:new(1, 1), rational:new(2, 2))),
     ?_assertEqual(rational:new(9, 10),
                   rational:add(rational:new(9, 10), rational:new(0, 1))),
     ?_assertEqual(rational:add(rational:new(5, 16), rational:new(10, 12)),
                   rational:add(rational:new(10, 12), rational:new(5, 16)))].

multiply_test_() ->
    [?_assertEqual(rational:new(0, 1),
                   rational:multiply(rational:new(0, 1), rational:new(2, 10))),
     ?_assertEqual(rational:multiply(rational:new(5, 3), rational:new(16, 33)),
                   rational:multiply(rational:new(16, 33), rational:new(5, 3))),
     ?_assertEqual(rational:new(-1, 4),
                   rational:multiply(rational:new(-1, 1), rational:new(1, 4)))].

subtract_test_() ->
    [?_assertEqual(rational:new(1, 2),
                   rational:subtract(rational:new(1, 1), rational:new(1, 2))),
     ?_assertEqual(rational:new(-1, 2),
                   rational:subtract(rational:new(1, 2), rational:new(1, 1))),
     ?_assertEqual(rational:new(0, 0),
                   rational:subtract(rational:new(3, 5), rational:new(3, 5))),
     ?_assertEqual(rational:new(0, 0),
                   rational:subtract(rational:new(0, 0), rational:new(0, 0))),
     ?_assertEqual(rational:new(-5,6),
                   rational:subtract(rational:new(-5, 6), rational:new(0, 0))),
     ?_assertEqual(rational:new(5, 6),
                   rational:subtract(rational:new(0, 0), rational:new(-5, 6)))].

reciprocal_test_() ->
    [?_assertEqual(rational:new(3, 3), rational:reciprocal(rational:new(3, 3))),
     ?_assertEqual(rational:new(0, 0), rational:reciprocal(rational:new(0, 5))),
     ?_assertEqual(rational:new(2, 1), rational:reciprocal(rational:new(1, 2)))].

compare_test_() ->
    {setup,
     fun() -> {rational:new(1, 4), rational:new(1, 5)} end,
     fun({OneQuarter, OneFifth}) ->
             [?_assertEqual(eq, rational:compare(OneQuarter, OneQuarter)),
              ?_assertEqual(lt, rational:compare(OneFifth, OneQuarter)),
              ?_assertEqual(gt, rational:compare(OneQuarter, OneFifth)),
              ?_assertEqual(lt, rational:compare(
                                  rational:multiply(rational:new(-1, 1), OneQuarter),
                                  OneFifth)),
              ?_assertEqual(lt, rational:compare(
                                  rational:multiply(rational:new(1, -1), OneQuarter),
                                  OneFifth)),
              ?_assertEqual(gt, rational:compare(
                                  rational:new(3, 2), rational:new(0))),
              ?_assertEqual(gt, rational:compare(
                                  rational:new(3, 2), rational:new(1))),
              ?_assertEqual(gt, rational:compare(
                                  rational:new(1, 2), rational:new(0))),
              ?_assertEqual(lt, rational:compare(
                                  rational:new(0), rational:new(3, 2))),
              ?_assertEqual(lt, rational:compare(
                                  rational:new(0), rational:new(1)))]
     end}.

new_integral_test_() ->
    [?_assertEqual(rational:new(1, 1), rational:new(1)),
     ?_assertEqual(rational:new(0, 1), rational:new(0)),
     ?_assertEqual(rational:new(10, 1), rational:new(10)),
     ?_assertEqual(rational:new(-100, 1), rational:new(-100))].
