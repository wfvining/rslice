-module(interval).
-export([new/2, length/1, contains/2, contains/3, span/2, preceeds/2]).
-export_type([interval/0]).

-record(interval, {left  :: rational:rational(),
                   right :: rational:rational()}).

-type interval() :: #interval{}.

%% @doc Create a new interval with the given endpoints.
%%
%% The function fails with a `badarg' exception if `Left' is greater
%% than `Right'.
%%
%% @throws badarg
-spec new(rational:rational(), rational:rational()) -> interval().
new(Left, Right) ->
    case rational:compare(Left, Right) of
        gt -> throw({badarg, "Left must be less than Right"});
        _  -> #interval{left = Left, right = Right}
    end.

%% @doc Get the length of an interval.
-spec length(interval()) -> rational:rational().
length(#interval{left=Left, right=Right}) ->
    rational:subtract(Right, Left).

%% @doc Test whether `Value' is contained in `Interval'.
-spec contains(Value::rational:rational(), Interval::interval()) -> boolean().
contains(Value, IntervalTable) ->
    contains(Value, IntervalTable, both).

-spec contains(Value::rational:rational(), Interval::interval(),
               Closed::left|right|both|neither) ->
                      boolean().
contains(Value, #interval{left=Left, right=Right}, Closed) ->
    L = rational:compare(Value, Left),
    R = rational:compare(Value, Right),
    case {L, R} of
        {lt, lt} -> false;
        {gt, gt} -> false;
        {gt, lt} -> true;
        {eq, eq} ->
            (Closed =:= left) or (Closed =:= right) or (Closed =:= both);
        {eq, lt} ->
            (Closed =:= left) or (Closed =:= both);
        {gt, eq} ->
            (Closed =:= right) or (Closed =:= both)
    end.

%% @doc split an interval into two parts where the first part is at
%% least `Span' long.
-spec span(rational:rational(), interval()) -> {interval() | empty, interval() | empty}.
span(Span, I=#interval{left=Left, right=Right}) ->
    Split = rational:add(Left, Span),
    Length = interval:length(I),
    case rational:compare(Span, Length) of
        eq ->
            {I, empty};
        gt ->
            {I, empty};
        lt ->
            {empty_or(interval:new(Left, Split)),
             empty_or(interval:new(Split, Right))}
    end.

-spec empty_or(interval()) -> interval() | empty.
empty_or(#interval{left=Left, right=Right}) when Left =:= Right ->
    empty;
empty_or(Interval) ->
    Interval.

-spec preceeds(interval(), interval()) -> boolean().
preceeds(#interval{left=LeftA, right=RightA},
         #interval{left=LeftB}) ->
    (rational:compare(LeftA, LeftB) == lt)
        andalso ((rational:compare(RightA, LeftB) == lt)
                 orelse (rational:compare(RightA, LeftB) == eq)).
