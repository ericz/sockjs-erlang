-module(mqb_core).
-export([get_random_string/1]).

get_random_string(Length) ->
	AllowedChars = ["a", "b", "c", "1", "2", "3"],
    CharList = lists:foldl(fun(_, Acc) ->
                        [lists:nth(random:uniform(length(AllowedChars)),
                                   AllowedChars)]
                            ++ Acc
                end, [], lists:seq(1, Length)),
    unicode:characters_to_binary(CharList).
