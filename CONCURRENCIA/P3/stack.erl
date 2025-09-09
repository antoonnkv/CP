-module(stack).

-export([empty/0, push/2, pop/1, peek/1]).

empty() -> [].

push(Stack, Elem) -> [Elem|Stack].

pop(Stack) ->
    case Stack of
    [] -> [];
    [_|T] -> T 
    end.

peek(Stack) ->
    case Stack of
    [] -> empty;
    [H|_] -> {ok, H} 
    end.
