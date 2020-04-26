-module(interval).
-export([new/2]).
-export_type([interval/0]).

-record(interval, {left  :: rational:rational(),
                   right :: rational:rational()}).

-type interval() :: #interval{}.

-spec new(rational:rational(), rational:rational()) -> interval().
new(Left, Right) ->
    case rational:compare(Left, Right) of
        gt -> throw(badarg);
        _  -> #interval{left = Left, right = Right}
    end.
