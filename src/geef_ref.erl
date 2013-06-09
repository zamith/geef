-module(geef_ref).

-export([lookup/2, resolve/1, create/4, dwim/2, shorthand/1]).

-include("geef_records.hrl").

-spec new(binary(), term()) -> geef_reference().
new(Name, Handle) ->
    Type = geef_nif:reference_type(Handle),
    Bin = geef_nif:reference_target(Handle),
    Target = case Type of
		 symbolic ->
		     Bin;
		 oid ->
		     #geef_oid{oid=Bin}
	     end,
    #geef_reference{handle=Handle, name=Name, type=Type, target=Target}.

-spec create(pid(), iolist(), geef_oid() | binary(), boolean()) -> {ok, geef_reference()} | {error, term()}.
create(Repo, Refname, Target, Force) ->
    {ok, Ref} = geef_repo:create_reference(Repo, Refname, Target, Force),
    {ok, new(Refname, Ref)}.

-spec lookup(pid(), iolist()) -> {ok, geef_reference()} | {error, term()}.
lookup(Repo, Refname) ->
    Name = iolist_to_binary(Refname),
    case geef_repo:lookup_reference(Repo, Name) of
	{ok, Ref} ->
	    {ok, new(Name, Ref)};
	Other ->
	    Other
    end.

-spec resolve(geef_reference()) -> {ok, geef_reference()} | {error, term()}.
resolve(Ref = #geef_reference{type=oid}) ->
    {ok, Ref}; % resolving an oid ref is a no-op, skip going into the NIF
resolve(#geef_reference{handle=Handle}) ->
    case geef_nif:reference_resolve(Handle) of
	{ok, Ref} ->
	    {ok, Name} = geef_nif:reference_name(Ref),
	    {ok, new(Name, Ref)};
	Other ->
	    Other
    end.

-spec dwim(pid(), iolist()) -> {ok, geef_reference()} | {error, term()}.
dwim(Repo, Name) ->
    case geef_repo:reference_dwim(Repo, Name) of
	{ok, Handle} ->
	    {ok, FullName} = geef_nif:reference_name(Handle),
	    {ok, new(FullName, Handle)};
	Err ->
	    Err
    end.

%% @doc Get the shorthand name for a particular reference
-spec shorthand(geef_reference() | binary()) -> binary().
shorthand(<<"refs/heads/", Rest/binary>>) ->
    Rest;
shorthand(<<"refs/tags/", Rest/binary>>) ->
    Rest;
shorthand(<<"refs/remotess/", Rest/binary>>) ->
    Rest;
shorthand(<<"refs/", Rest/binary>>) ->
    Rest;
shorthand(#geef_reference{name=Name}) ->
    shorthand(iolist_to_binary(Name)).
