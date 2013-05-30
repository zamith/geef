-module(geef_object).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("geef_records.hrl").

-export([lookup/2, lookup/3, id/1, type/1]).

-spec lookup(pid(), geef_oid() | iolist()) -> {ok, geef_object()} | {error, term()}.
lookup(Repo, #geef_oid{oid=Oid}) ->
    case geef_repo:lookup_object(Repo, Oid) of
	{ok, Type, Handle} ->
	    {ok, #geef_object{type=Type, handle=Handle}};
	{error, Err} ->
	    {error, Err}
    end;
lookup(Repo, Id) ->
    lookup(Repo, geef_oid:parse(Id)).

%% As lookup/2, but it asserts that the type is correct
-spec lookup(pid(), geef_oid() | iolist(), atom()) -> {ok, geef_object()} | {error, term()}.
lookup(Repo, Id, Type) ->
    case lookup(Repo, Id) of
	{ok, Obj = #geef_object{type=Type}} ->
	    {ok, Obj};
	{error, Err} ->
	    {error, Err}
    end.

-spec id(geef_object()) -> {ok, geef_oid()} | {error, term()}.
id(#geef_object{handle=Handle}) ->
    case geef_nif:object_id(Handle) of
	{ok, Oid} ->
	    #geef_oid{oid=Oid};
	Other ->
	    Other
    end.

-spec type(geef_object()) -> atom().
type(#geef_object{type=Type}) ->
    Type.

-ifdef(TEST).

lookup_test() ->
    {ok, Repo} = geef_repo:open(".."),
    {ok, #geef_object{type=commit}} = lookup(Repo, geef_oid:parse("b5b68cce8b92ca0e7bd48430617ac10c0f2c2923")),
    {ok, #geef_object{type=commit}} = lookup(Repo, "b5b68cce8b92ca0e7bd48430617ac10c0f2c2923"),
    {ok, #geef_object{type=commit}} = lookup(Repo, ["b5b68cce8b92ca0e7", "bd48430617ac10c0f2c2923"]),
    {ok, #geef_object{type=commit}} = lookup(Repo, <<"b5b68cce8b92ca0e7bd48430617ac10c0f2c2923">>).

id_test() ->
    {ok, Repo} = geef_repo:open(".."),
    Sha = <<"b5b68cce8b92ca0e7bd48430617ac10c0f2c2923">>,
    Id = geef_oid:parse(Sha),
    {ok, Commit = #geef_object{type=commit}} = lookup(Repo, Sha),
    Id = id(Commit).

-endif.
