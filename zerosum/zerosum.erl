%% zerosum.erl
%% Find lists that have sub-sets that sum to zero
%% 
%% Build a function that given a list of Integers in the range [-65000,65000], the function
%% 
%% returns true if any subset of the list summed is equal to zero. False otherwise.
%% 
%% Example:
%% 
%%  [0, 1, 2, -3] returns true. As 1+2+(-3)==0.
%%  [1, 2, 3, -8] returns false. As no subset summed is equal 0.
%%  [1, 4, 5, 2, -3] returns true.

-module(zerosum).
-export([subsets/1, zerosum/1]).

%% For a given list, find all sub-lists (i.e. combinations of elements)
subsets([]) ->
    [];
subsets(List) ->
    [Head | Tail] = List,
    TailSubSets = subsets(Tail),
    [[Head]] ++ TailSubSets ++ [[Head | Ts] || Ts <- TailSubSets].

zerosum(List) ->
    lists:any(
        fun(Subset) -> 
            lists:sum(Subset) == 0 end,
        subsets(List)
    ).

