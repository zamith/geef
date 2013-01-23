-module(geef_ref).

-export([resolve/1, id/1]).

-include("geef_records.hlr").

resolve(#ref{handle=Handle}) ->
    case geef:reference_resolve(Handle) of
	{ok, Ref} ->
	    {ok, #ref{handle=Handle}};
	Other ->
	    Other
    end.

-spec id(term()) -> binary().
id(#ref{handle=Handle}) ->
    geef:reference_id(Handle).
