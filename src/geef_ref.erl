-module(geef_ref).

-export([lookup/2, resolve/1, target/1, name_to_id/2]).

-include("geef_records.hrl").

-spec new(term()) -> ref().
new(Handle) ->
    Type = geef_nif:reference_type(Handle),
    Bin = geef_nif:reference_target(Handle),
    Target = case Type of
		 symbolic ->
		     Bin;
		 oid ->
		     #oid{oid=Bin}
	     end,
    #ref{handle=Handle, type=Type, target=Target}.

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
target(#ref{target=Target}) ->
    Target.

-spec name_to_id(repo(), iolist()) -> {ok, oid()} | {error, binary()}.
name_to_id(#repo{handle=Handle}, Name) ->
    case geef_nif:reference_to_id(Handle, Name) of
	{ok, Oid} ->
	    {ok, #oid{oid=Oid}};
	Other ->
	    Other
    end.
