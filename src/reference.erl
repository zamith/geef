-module(reference, [Handle]).
-export([resolve/0, id/0]).

resolve() ->
    case geef:reference_resolve(Handle) of
	{ok, Ref} ->
	    reference:new(Ref);
	Other ->
	    Other
    end.

-spec id() -> binary().
id() ->
    geef:reference_id(Handle).
