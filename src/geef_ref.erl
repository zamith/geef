-module(geef_ref).

-export([lookup/2, resolve/1, create/4, name/1, type/1, target/1]).

-include("geef_records.hrl").

-spec new(binary(), term()) -> geef_ref().
new(Name, Handle) ->
    Type = geef_nif:reference_type(Handle),
    Bin = geef_nif:reference_target(Handle),
    Target = case Type of
		 symbolic ->
		     Bin;
		 oid ->
		     #geef_oid{oid=Bin}
	     end,
    #geef_ref{handle=Handle, name=Name, type=Type, target=Target}.

-spec create(pid(), iolist(), geef_oid() | binary(), boolean()) -> {ok, geef_ref()} | {error, term()}
.
create(Repo, Refname, Target, Force) ->
    {ok, Ref} = geef_repo:create_reference(Repo, Refname, Target, Force),
    {ok, new(Refname, Ref)}.

-spec lookup(pid(), iolist()) -> {ok, geef_ref()} | {error, term()}.
lookup(Repo, Refname) ->
    Name = iolist_to_binary(Refname),
    case geef_repo:lookup_reference(Repo, Name) of
	{ok, Ref} ->
	    {ok, new(Name, Ref)};
	Other ->
	    Other
    end.

-spec resolve(geef_ref()) -> {ok, geef_ref()} | {error, term()}.
resolve(#geef_ref{handle=Handle}) ->
    case geef_nif:reference_resolve(Handle) of
	{ok, Ref} ->
	    {ok, Name} = geef_nif:reference_name(Ref),
	    {ok, new(Name, Ref)};
	Other ->
	    Other
    end.

-spec target(geef_ref()) -> {geef_oid(), binary()}.
target(#geef_ref{target=Target}) ->
    Target.

-spec type(geef_ref()) -> symbolic | oid.
type(#geef_ref{type=Type}) ->
    Type.

-spec name(geef_ref()) -> binary().
name(#geef_ref{name=Name}) ->
    Name.
