-module(geef_ref).

-export([lookup/2, iterator/1, iterator/2, next/1, resolve/1, create/4, create_symbolic/4, dwim/2, shorthand/1]).

-include("geef_records.hrl").

-type iterator() :: #geef_iterator{type :: ref}.
-type type() :: oid | symbolic.
-type target() :: binary() | geef_oid:oid().
-type ref() :: #geef_reference{name :: binary()}.
-export_type([reference/0, type/0, target/0, iterator/0]).

-spec create(pid(), iolist(), geef_oid:oid(), boolean()) -> {ok, ref()} | {error, term()}.
create(Repo, Refname, Target, Force) ->
    RepoHandle = geef_repo:handle(Repo),
    case geef_nif:reference_create(RepoHandle, Refname, oid, Target, Force) of
        ok ->
	    {ok, make(Repo, iolist_to_binary(Refname), oid, Target)};
        Err = {error, _} ->
            Err
    end.

-spec create_symbolic(pid(), iolist(), iolist(), boolean()) -> {ok, ref()} | {error, term()}.
create_symbolic(Repo, Refname, Target, Force) ->
    RepoHandle = geef_repo:handle(Repo),
    case geef_nif:reference_create(RepoHandle, Refname, symbolic, Target, Force) of
        ok ->
	    {ok, make(Repo, iolist_to_binary(Refname), symbolic, iolist_to_binary(Target))};
        Err = {error, _} ->
            Err
    end.

-spec make(pid(), binary(), type(), target()) -> ref().
make(Repo, Name, oid, Target) ->
    #geef_reference{repo=Repo, name=Name, type=oid, target=Target};
make(Repo, Name, symbolic, Target) ->
    #geef_reference{repo=Repo, name=Name, type=symbolic, target=Target}.

-spec lookup(pid(), iolist()) -> {ok, ref()} | {error, term()}.
lookup(Repo, Refname) ->
    Name = iolist_to_binary(Refname),
    RepoHandle = geef_repo:handle(Repo),
    case geef_nif:reference_lookup(RepoHandle, Name) of
	{ok, Type, Target} ->
	    {ok, make(Repo, Name, Type, Target)};
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
next(#geef_iterator{type=ref, repo=Repo, handle=Handle}) ->
    case geef_nif:reference_next(Handle) of
	{ok, Name, Type, Target} ->
	    {ok, make(Repo, Name, Type, Target)};
	Other ->
	    Other
    end.

-spec resolve(ref()) -> {ok, ref()} | {error, term()}.
resolve(Ref = #geef_reference{type=oid}) ->
    {ok, Ref}; % resolving an oid ref is a no-op, skip going into the NIF
resolve(#geef_reference{repo=Repo, type=symbolic, target=Name}) ->
    RepoHandle = geef_repo:handle(Repo),
    case geef_nif:reference_resolve(RepoHandle, Name) of
	{ok, ResolvedName, Target} ->
	    {ok, make(Repo, ResolvedName, oid, Target)};
	Other = {error, _} ->
	    Other
    end.

-spec dwim(pid(), iolist()) -> {ok, ref()} | {error, term()}.
dwim(Repo, Name) ->
    RepoHandle = geef_repo:handle(Repo),
    case geef_nif:reference_dwim(RepoHandle, Name) of
	{ok, RealName, Type, Target} ->
	    {ok, make(Repo, RealName, Type, Target)};
	Err = {error, _}->
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
