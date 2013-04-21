-module(geef_ref).

-export([lookup/2, resolve/1, target/1, name_to_id/2]).

-include("geef_records.hrl").

-spec new(term()) -> ref().
new(Handle) ->
    Type = geef:reference_type(Handle),
    #ref{handle=Handle, type=Type}.

-spec lookup(pid(), iolist()) -> {ok, ref()} | {error, term()}.
lookup(Repo, Refname) ->
    case geef_repo:lookup_reference(Repo, Refname) of
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

-spec name_to_id(repo(), iolist()) -> {ok, oid()} | {error, binary()}.
name_to_id(#repo{handle=Handle}, Name) ->
    case geef:reference_to_id(Handle, Name) of
	{ok, Oid} ->
	    {ok, #oid{oid=Oid}};
	Other ->
	    Other
    end.
