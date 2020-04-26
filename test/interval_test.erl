-module(interval_test).
-include_lib("eunit/include/eunit.hrl").

interval_equal_test_() ->
    {"Creating an interval with left larger than right throws an exception.",
     fun() ->
             OneQuarter = rational:new(1, 4),
             OneHalf = rational:new(2, 4),
             ?assertThrow(
                badarg,
                interval:new(OneHalf, OneQuarter)
               )
     end}.
