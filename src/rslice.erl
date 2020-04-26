-module(rslice).

-export([empty/0]).

%% @doc Create an empty random slicing table.
-spec empty() -> inteval_table:table().
empty() ->
    inteval_table:empty().
