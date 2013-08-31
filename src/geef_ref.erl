-module(geef_ref).

-export([lookup/2, iterator/1, iterator/2, next/1, resolve/1, create/4, dwim/2, shorthand/1]).

-include("geef_records.hrl").

-type iterator() :: #geef_iterator{type :: ref}.
-type ref() :: #geef_reference{name :: binary()}.
-export_type([reference/0, iterator/0]).

-spec new(term()) -> ref().
new(Handle) ->
    {ok, Name} = geef_nif:reference_name(Handle),
    new(Name, Handle).

-spec new(binary(), term()) -> ref().
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

-spec create(pid(), iolist(), geef_oid:oid() | binary(), boolean()) -> {ok, ref()} | {error, term()}.
create(Repo, Refname, Target, Force) ->
    case geef_repo:create_reference(Repo, Refname, Target, Force) of
        {ok, Ref} ->
            {ok, new(Refname, Ref)};
        Err = {error, _} ->
            Err
    end.

-spec lookup(pid(), iolist()) -> {ok, ref()} | {error, term()}.
lookup(Repo, Refname) ->
    Name = iolist_to_binary(Refname),
    case geef_repo:lookup_reference(Repo, Name) of
	{ok, Ref} ->
	    {ok, new(Name, Ref)};
	Other ->
	    Other
    end.

-spec iterator(pid(), iolist() | undefined) -> {ok, iterator()} | {error, term()}.
iterator(Repo, Regexp) ->
    case geef_repo:iterator(Repo, Regexp) of
	{ok, Handle} ->
	    {ok, #geef_iterator{type=ref, repo=Repo, regexp=Regexp, handle=Handle}};
	Other ->
	    Other
    end.

-spec iterator(pid()) -> {ok, iterator()} | {error, term()}.
iterator(Repo) ->
    iterator(Repo, undefined).

-spec next(iterator()) -> {ok, ref()} | {error, term()}.
next(#geef_iterator{type=ref, handle=Handle}) ->
    case geef_nif:reference_next(Handle) of
	{ok, RefHandle} ->
	    {ok, new(RefHandle)};
	Other ->
	    Other
    end.

-spec resolve(ref()) -> {ok, ref()} | {error, term()}.
resolve(Ref = #geef_reference{type=oid}) ->
    {ok, Ref}; % resolving an oid ref is a no-op, skip going into the NIF
resolve(#geef_reference{type=symbolic, handle=Handle}) ->
    case geef_nif:reference_resolve(Handle) of
	{ok, Ref} ->
	    {ok, new(Ref)};
	Other = {error, _} ->
	    Other
    end.

-spec dwim(pid(), iolist()) -> {ok, ref()} | {error, term()}.
dwim(Repo, Name) ->
    case geef_repo:reference_dwim(Repo, Name) of
	{ok, Handle} ->
	    {ok, new(Handle)};
	Err ->
	    Err
    end.

%% @doc Get the shorthand name for a particular reference
-spec shorthand(ref() | binary()) -> binary().
shorthand(<<"refs/heads/", Rest/binary>>) ->
    Rest;
shorthand(<<"refs/tags/", Rest/binary>>) ->
    Rest;
shorthand(<<"refs/remotes/", Rest/binary>>) ->
    Rest;
shorthand(<<"refs/", Rest/binary>>) ->
    Rest;
shorthand(#geef_reference{name=Name}) ->
    shorthand(Name).
