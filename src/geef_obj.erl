-module(geef_obj).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("geef_records.hrl").

-type object() :: #geef_object{type :: blob | tree | commit | tag}.
-type object(Type) :: #geef_object{type :: Type}.

-export_type([object/0, object/1]).

-export([lookup/2, lookup/3]).

-spec lookup(pid(), geef_oid:oid() | iolist()) -> {ok, object()} | {error, term()}.
lookup(Repo, Id = #geef_oid{oid=Oid}) ->
    case geef_repo:lookup_object(Repo, Oid) of
	{ok, Type, Handle} ->
	    {ok, #geef_object{type=Type, id=Id, handle=Handle}};
	{error, Err} ->
	    {error, Err}
    end;
lookup(Repo, Id) ->
    lookup(Repo, geef_oid:parse(Id)).

%% As lookup/2, but it asserts that the type is correct
-spec lookup(pid(), geef_oid:oid() | iolist(), atom()) -> {ok, object()} | {error, term()}.
lookup(Repo, Id, Type) ->
    case lookup(Repo, Id) of
	{ok, Obj = #geef_object{type=Type}} ->
	    {ok, Obj};
	{error, Err} ->
	    {error, Err}
    end.

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
    Id = Commit#geef_object.id.

-endif.
