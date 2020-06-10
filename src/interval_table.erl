-module(interval_table).
-export([new/0, gap_size/1, span/2, assign/2, shrink/2, lookup/2]).
-export_type([table/0]).

-type key() :: any().

-record(table, {gaps = [] :: list(interval:interval()),
                intervals = [] :: list({interval:interval(), key()})}).

-opaque table() :: #table{}.

-spec new() -> table().
new() ->
    #table{gaps = [interval:new(rational:new(0, 1), rational:new(1, 1))]}.

%% @doc Get the span covered by all gaps in the table.
-spec gap_size(table()) -> rational:rational().
gap_size(#table{gaps=Gaps}) ->
    lists:foldl(
      fun(Interval, Size) ->
              rational:add(Size, interval:length(Interval))
      end,
      rational:new(0, 1), Gaps).

%% @doc Assign all keys in `NewKeys' to gaps in `Table'. Uses the
%% greedy strategy (largest span/largest gap first strategy).
-spec assign(NewKeys::#{key() => rational:rational()}, Table::table()) -> table().
assign(NewKeys, Table) when map_size(NewKeys) =:= 0 ->
    Table;
assign(NewKeys, Table=#table{gaps=[]}) ->
    NewNewKeys = maps:filter(fun(_, Span) -> Span =/= rational:new(0) end, NewKeys),
    case maps:size(NewNewKeys) of
        0 -> Table;
        _ -> throw({badarg, "Span too large for available gaps."})
    end;
assign(NewKeys, Table=#table{gaps=Gaps, intervals=Intervals}) ->
    {LargestKey, LargestSpan} =
        maps:fold(
          fun(Key, Span, Acc={_, LargestSpan}) ->
                  case rational:compare(Span, LargestSpan) of
                      eq -> {Key, Span};
                      lt -> Acc;
                      gt -> {Key, Span}
                  end
          end, {'$no_key$', rational:new(0)}, NewKeys),
    LargestGap =
        lists:foldl(
          fun(Gap, LargestGap) ->
                  case rational:compare(interval:length(Gap),
                                        interval:length(LargestGap)) of
                      eq -> Gap;
                      lt -> LargestGap;
                      gt -> Gap
                  end
          end, interval:new(rational:new(0), rational:new(0)), Gaps),
    RemainingGaps = lists:delete(LargestGap, Gaps),
    case interval:split(LargestSpan, LargestGap, left) of
        {empty, _} ->
            % if the key with the largest span has an empty span the
            % assignment is done
            Table;
        {Interval, empty} ->
            assign(
              maps:update_with(
                LargestKey,
                fun(Span) ->
                        rational:subtract(Span, interval:length(Interval))
                end,
                NewKeys),
              Table#table{
                gaps = RemainingGaps,
                intervals = insert_inorder({Interval, LargestKey}, Intervals)});
        {Interval, Gap} ->
            assign(
              maps:remove(LargestKey, NewKeys),
              Table#table{
               gaps = [Gap | RemainingGaps],
               intervals = insert_inorder({Interval, LargestKey}, Intervals)})
    end.

insert_inorder({Gap, Key}, []) ->
    [{Gap, Key}];
insert_inorder({Gap, Key}, [Assigned={I, _}|Intervals]) ->
    % no case for eq because we should never insert an interval that
    % is equal to an interval that is already assigned.
    case interval:preceeds(Gap, I) of
        true ->
            [{Gap, Key},Assigned|Intervals];
        false ->
            [Assigned|insert_inorder({Gap, Key}, Intervals)]
    end.

%% @doc Get the total span assigned to `Key'.
-spec span(key(), table()) -> rational:rational().
span(Key, IntervalTable) ->
    lists:foldl(
      fun({Interval, AssignedTo}, Span) ->
              if AssignedTo =:= Key ->
                      rational:add(interval:length(Interval), Span);
                 AssignedTo =/= Key ->
                      Span
              end
      end, rational:new(0), IntervalTable#table.intervals).

%% @doc Create gaps by shrinking the span assigned to each key.
%% @param Changes a map ``#{Key => ShrinkAmount}'' giving the amount
%% by which to shrink the span assigned to `Key'.
%% @reutrns the table with gaps.
-spec shrink(#{key() => rational:rational()}, table()) -> table().
shrink(Changes, Table=#table{gaps=Gaps, intervals=Intervals}) ->
    {NewGaps, NewIntervals} = cut_shift(Changes, Intervals),
    Table#table{gaps = merge_gaps(NewGaps, Gaps),
                intervals = NewIntervals}.

%% merge two lists of gaps combining adjacent gaps into single larger
%% gaps.
merge_gaps(Gaps1, Gaps2) ->
    SortedGaps =
        lists:merge(fun interval:preceeds/2,
                    lists:sort(fun interval:preceeds/2, Gaps1),
                    lists:sort(fun interval:preceeds/2, Gaps2)),
    lists:foldr(fun(Gap, []) ->
                        [Gap];
                   (Gap, [PreceedingGap|Gaps]) ->
                        case interval:adjacent(Gap, PreceedingGap) of
                            true ->
                                [interval:merge(Gap, PreceedingGap) | Gaps];
                            false ->
                                [Gap, PreceedingGap | Gaps]
                        end
                end,
                [], SortedGaps).


cut_shift(AssignmentChanges, Intervals) ->
    SubtractLength =
        fun(I) ->
                fun(X) ->
                        rational:subtract(X, interval:length(I))
                end
        end,
    {_, Gaps, Remaining, _} =
        lists:foldl(
          fun({I, Key}, {Changes, Gaps, Remaining, left}) ->
                  Span = maps:get(Key, Changes, rational:new(0)),
                  case interval:split(Span, I, left) of
                      {empty, _} ->
                          {Changes, Gaps, [{I, Key}|Remaining], right};
                      {Gap, empty} ->
                          {maps:update_with(
                             Key, SubtractLength(Gap), rational:new(0), Changes),
                           [Gap|Gaps],
                           Remaining,
                           left};
                      {Gap, Rem} ->
                          {maps:update_with(
                             Key, SubtractLength(Gap), rational:new(0), Changes),
                           [Gap|Gaps],
                           [{Rem, Key}|Remaining],
                           right}
                  end;
             ({I, Key}, {Changes, Gaps, Remaining, right}) ->
                  Span = maps:get(Key, Changes, rational:new(0)),
                  case interval:split(Span, I, right) of
                      {_, empty} ->
                          {Changes, Gaps, [{I, Key}|Remaining], right};
                      {empty, Gap} ->
                          {maps:update_with(
                             Key, SubtractLength(Gap), rational:new(0), Changes),
                           [Gap|Gaps],
                           Remaining,
                           left};
                      {Rem, Gap} ->
                          {maps:update_with(
                             Key, SubtractLength(Gap), rational:new(0), Changes),
                           [Gap|Gaps],
                           [{Rem, Key}|Remaining],
                           left}
                  end
          end,
          {AssignmentChanges, [], [], right}, Intervals),
    {Gaps, Remaining}.

%% @doc Look up the key assigned `X'.
%%
%% @returns `{ok, Key}' if `X' falls in the interval assigned to `Key'
%% or `unassigned' if `X' falls in a gap.
-spec lookup(rational:rational(), table()) -> {ok, key()} | unassigned.
lookup(X, #table{intervals=Intervals}) ->
    case lists:filter(fun({I, _}) -> interval:contains(X, I) end, Intervals) of
        [{_, Key}|_] ->
            {ok, Key};
        [] ->
            unassigned
    end.
