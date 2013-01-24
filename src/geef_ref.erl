-module(geef_ref).

-export([resolve/1, target/1]).

-include("geef_records.hlr").

resolve(#ref{handle=Handle}) ->
    case geef:reference_resolve(Handle) of
	{ok, Ref} ->
	    {ok, #ref{handle=Handle}};
	Other ->
	    Other
    end.

-spec target(term()) -> binary().
target(#ref{handle=Handle}) ->
    geef:reference_target(Handle).
