-module(decode_ffi).

-export([index/2]).

index(Tuple, Index) when is_tuple(Tuple) andalso is_integer(Index) ->
    try
        {ok, element(Index + 1, Tuple)}
    catch _:_ -> 
        {ok, nil}
    end;
index(Map, Key) when is_map(Map) ->
    try
        {ok, maps:get(Key, Map)}
    catch _:_ -> 
        {ok, nil}
    end;
index(_, Index) when is_integer(Index) ->
    {error, <<"Indexable">>};
index(_, _) ->
    {error, <<"Dict">>}.
