-module(geef_ref).

-export([lookup/2, resolve/1, target/1]).

-include("geef_records.hrl").

-spec new(term()) -> ref().
new(Handle) ->
    Type = geef:reference_type(Handle),
    #ref{handle=Handle, type=Type}.

-spec lookup(repo(), iolist()) -> {ok, ref()} | {error, term()}.
lookup(#repo{handle=Handle}, Refname) ->
    case geef:reference_lookup(Handle, Refname) of
	{ok, Ref} ->
	    {ok, new(Ref)};
	Other ->
	    Other
    end.

-spec resolve(ref()) -> {ok, ref()} | {error, term()}.
resolve(#ref{handle=Handle}) ->
    case geef:reference_resolve(Handle) of
	{ok, Ref} ->
	    {ok, new(Ref)};
	Other ->
	    Other
    end.

-spec target(ref()) -> binary() | oid().
target(#ref{handle=Handle,type=symbolic}) ->
    geef:reference_target(Handle);
target(#ref{handle=Handle,type=oid}) ->
    Oid = geef:reference_target(Handle),
    #oid{oid=Oid}.
