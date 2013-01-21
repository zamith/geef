-module(reference, [Handle]).
-export([resolve/0]).

resolve() ->
    case geef:reference_resolve(Handle) of
	{ok, Ref} ->
	    reference:new(Ref);
	other -> other
    end.
