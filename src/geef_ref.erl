-module(geef_ref).

-export([lookup/2, resolve/1, target/1]).

-include("geef_records.hlr").

new(Handle) ->
    Type = geef:reference_type(Handle),
    #ref{handle=Handle, type=Type}.

-spec lookup(term(), iolist()) -> term().
lookup(#repo{handle=Handle}, Refname) ->
    case geef:reference_lookup(Handle, Refname) of
	{ok, Ref} ->
	    {ok, new(Ref)};
	Other ->
	    Other
    end.

resolve(#ref{handle=Handle}) ->
    case geef:reference_resolve(Handle) of
	{ok, Ref} ->
	    {ok, new(Ref)};
	Other ->
	    Other
    end.

-spec target(term()) -> binary().
target(#ref{handle=Handle, type=oid}) ->
    geef:reference_target(Handle).
