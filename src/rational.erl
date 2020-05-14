-module(rational).
-export([new/1, new/2, add/2, multiply/2, subtract/2, reciprocal/1,
         compare/2, minimum/2, maximum/2]).
-export_type([rational/0]).

-record(rational, {numerator   :: integer(),
                   denominator :: integer()}).

-opaque rational() :: #rational{}.

%% @doc Create a new rational number with denominator 1.
-spec new(integer()) -> rational().
new(N) ->
    new(N, 1).

%% @doc Create a new rational number.
%%
%% The denominator must be non-zero
-spec new(integer(), integer()) -> rational().
new(Numerator, Denominator) when Denominator < 0 ->
    new(-1 * Numerator, abs(Denominator));
new(Numerator, Denominator) ->
    reduce(#rational{numerator=Numerator, denominator=Denominator}).

%% @doc Add two rational numbers.
-spec add(rational(), rational()) -> rational().
add(#rational{numerator=0}, B) ->
    B;
add(A, #rational{numerator=0}) ->
    A;
add(#rational{numerator=NumA, denominator=DenomA},
    #rational{numerator=NumB, denominator=DenomB}) when DenomA =:= DenomB ->
    new(NumA + NumB, DenomA);
add(#rational{numerator=NumA, denominator=DenomA},
    #rational{numerator=NumB, denominator=DenomB}) ->
    new(NumA * DenomB + NumB * DenomA, DenomA * DenomB).

%% @doc Multiply two rational numbers.
-spec multiply(rational(), rational()) -> rational().
multiply(#rational{numerator=NumA, denominator=DenomA},
         #rational{numerator=NumB, denominator=DenomB}) ->
    new(NumA * NumB, DenomA * DenomB).

%% @doc Subtract rational number `B' from rational number `A'.
-spec subtract(rational(), rational()) -> rational().
subtract(A, B) ->
    add(A, multiply(B, new(-1, 1))).

%% @doc Reduce a rational number.
-spec reduce(rational()) -> rational().
reduce(#rational{numerator = 0}) ->
    #rational{numerator = 0, denominator = 1};
reduce(#rational{numerator=Numerator, denominator=Denominator}) ->
    G = gcd(abs(Numerator), abs(Denominator)),
    #rational{numerator=Numerator div G, denominator=Denominator div G}.

%% @doc Return the reciprocal of `X'.
-spec reciprocal(rational()) -> rational().
reciprocal(#rational{numerator=0}) ->
    #rational{numerator=0, denominator=1};
reciprocal(X) ->
    new(X#rational.denominator, X#rational.numerator).

%% @doc Compare two rational numbers.
%%
%% @returns ``lt'', ``eq'', or ``gt'' if `A' is less than, equal to,
%% or grater than `B' respectively.
-spec compare(A :: rational(), B :: rational()) -> eq | lt | gt.
compare(#rational{numerator=NumA, denominator=DenomA},
        #rational{numerator=NumB, denominator=DenomB}) ->
    NumeratorA = NumA * DenomB,
    NumeratorB = NumB * DenomA,
    if NumeratorA < NumeratorB ->
            lt;
       NumeratorA =:= NumeratorB ->
            eq;
       NumeratorA > NumeratorB ->
            gt
    end.

%% @doc return the maximum of `A' and `B'
-spec maximum(rational(), rational()) -> rational().
maximum(A, B) ->
    case rational:compare(A, B) of
        eq -> A;
        lt -> B;
        gt -> A
    end.

%% @doc return the minimum of `A' and `B'
-spec minimum(rational(), rational()) -> rational().
minimum(A, B) ->
    case rational:compare(A, B) of
        eq -> A;
        lt -> A;
        gt -> B
    end.

-spec gcd(integer(), integer()) -> integer().
gcd(X, 0) ->
    X;
gcd(X, Y) ->
    gcd(Y, X rem Y).
