-module(interval_table).
-export([empty/0, size/1]).

-export_type([table/0]).

-record(table, {keys = []      :: list(any()),
                intervals = [] :: list(interval:interval())}).

-type table() :: #table{}.

%% @doc Create a table with no entries
-spec empty() -> table().
empty() ->
    #table{}.

%% @doc Get the number of keys in the interval table.
-spec size(table()) -> integer().
size(#table{keys=Keys}) ->
    length(Keys).
