-module(geef_ref).

-export([lookup/2, resolve/1, create/4, target/1]).

-include("geef_records.hrl").

-spec new(term()) -> geef_ref().
new(Handle) ->
    Type = geef_nif:reference_type(Handle),
    Bin = geef_nif:reference_target(Handle),
    Target = case Type of
		 symbolic ->
		     Bin;
		 oid ->
		     #geef_oid{oid=Bin}
	     end,
    #geef_ref{handle=Handle, type=Type, target=Target}.

-spec create(pid(), iolist(), geef_oid() | binary(), boolean()) -> {ok, geef_ref()} | {error, term()}
.
create(Repo, Refname, Target, Force) ->
    {ok, Ref} = geef_repo:create_reference(Repo, Refname, Target, Force),
    {ok, new(Ref)}.

-spec lookup(pid(), iolist()) -> {ok, geef_ref()} | {error, term()}.
lookup(Repo, Refname) ->
    case geef_repo:lookup_reference(Repo, Refname) of
	{ok, Ref} ->
	    {ok, new(Ref)};
	Other ->
	    Other
    end.

-spec resolve(geef_ref()) -> {ok, geef_ref()} | {error, term()}.
resolve(#geef_ref{handle=Handle}) ->
    case geef_nif:reference_resolve(Handle) of
	{ok, Ref} ->
	    {ok, new(Ref)};
	Other ->
	    Other
    end.

-spec target(geef_ref()) -> {geef_oid(), binary()}.
target(#geef_ref{target=Target}) ->
    Target.
