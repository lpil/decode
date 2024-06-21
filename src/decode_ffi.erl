-module(decode_ffi).

-export([index/2]).

index(Tuple, Index)
        when is_tuple(Tuple) 
        andalso is_integer(Index)
        andalso Index >= 0
        andalso Index < tuple_size(Tuple) ->
    {ok, element(Index + 1, Tuple)};
index(_, _) ->
  {error, nil}.
