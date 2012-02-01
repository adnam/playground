-module(levenshtein).
-export([dist/2]).

%% Simple levenshtein distance
dist(S, S) -> 0;                % Identicle strings
dist([_C1], [_C2]) -> 1;        % Different single characters
dist([], S2) -> length(S2);     % First string empty
dist(S1, []) -> length(S1);     % Second string empty
dist([H1 | T1], [H2 | T2]) ->   % General solution
    lists:min([
        dist([H1 | T1], T2)+1,
        dist([H2 | T2], T1)+1,
        dist(T1, T2)+dist([H1], [H2])
    ]).

