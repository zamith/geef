-module(geef_object).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("geef_records.hrl").

-export([lookup/2, lookup/3, id/1]).

-spec lookup(pid(), oid() | iolist()) -> {ok, object()} | {error, term()}.
lookup(Repo, #oid{oid=Oid}) ->
    case geef_repo:lookup_object(Repo, Oid) of
	{ok, Type, Handle} ->
	    {ok, #object{type=Type, handle=Handle}};
	{error, Err} ->
	    {error, Err}
    end;
lookup(Repo, Id) ->
    lookup(Repo, geef_oid:parse(Id)).

%% As lookup/2, but it asserts that the type is correct
-spec lookup(pid(), oid() | iolist(), atom()) -> {ok, object()} | {error, term()}.
lookup(Repo, Id, Type) ->
    case lookup(Repo, Id) of
	{ok, Obj = #object{type=Type}} ->
	    {ok, Obj};
	{error, Err} ->
	    {error, Err}
    end.

id(#object{handle=Handle}) ->
    case geef:object_id(Handle) of
	{ok, Oid} ->
	    #oid{oid=Oid};
	Other ->
	    Other
    end.

-ifdef(TEST).

lookup_test() ->
    {ok, Repo} = geef_repo:open(".."),
    {ok, #object{type=commit}} = lookup(Repo, geef_oid:parse("b5b68cce8b92ca0e7bd48430617ac10c0f2c2923")),
    {ok, #object{type=commit}} = lookup(Repo, "b5b68cce8b92ca0e7bd48430617ac10c0f2c2923"),
    {ok, #object{type=commit}} = lookup(Repo, ["b5b68cce8b92ca0e7", "bd48430617ac10c0f2c2923"]),
    {ok, #object{type=commit}} = lookup(Repo, <<"b5b68cce8b92ca0e7bd48430617ac10c0f2c2923">>).

id_test() ->
    {ok, Repo} = geef_repo:open(".."),
    Sha = <<"b5b68cce8b92ca0e7bd48430617ac10c0f2c2923">>,
    Id = geef_oid:parse(Sha),
    {ok, Commit = #object{type=commit}} = lookup(Repo, Sha),
    Id = id(Commit).

-endif.
