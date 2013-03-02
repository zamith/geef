-module(geef_object).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("geef_records.hrl").

-export([lookup/2]).

-spec lookup(repo(), oid() | iolist()) -> object().
lookup(#repo{handle=RepoHandle}, #oid{oid=Oid}) ->
    case geef:object_lookup(RepoHandle, Oid) of
	{ok, Type, Handle} ->
	    {ok, #object{type=Type, handle=Handle}};
	Other ->
	    Other
    end;
lookup(Repo = #repo{}, Id) ->
    lookup(Repo, geef_oid:parse(Id)).

-ifdef(TEST).

lookup_test() ->
    {ok, Repo} = geef_repo:open(".."),
    {ok, #object{type=commit}} = lookup(Repo, geef_oid:parse("b5b68cce8b92ca0e7bd48430617ac10c0f2c2923")),
    {ok, #object{type=commit}} = lookup(Repo, "b5b68cce8b92ca0e7bd48430617ac10c0f2c2923"),
    {ok, #object{type=commit}} = lookup(Repo, ["b5b68cce8b92ca0e7", "bd48430617ac10c0f2c2923"]),
    {ok, #object{type=commit}} = lookup(Repo, <<"b5b68cce8b92ca0e7bd48430617ac10c0f2c2923">>).

-endif.
