-module(geef_object).

-include("geef_records.hrl").

-export([lookup/2]).

-spec lookup(repo(), oid()) -> object().
lookup(#repo{handle=Repo}, #oid{oid=Oid}) ->
    case geef:object_lookup(Repo, Oid) of
	{ok, Type, Handle} ->
	    {ok, #object{type=Type, handle=Handle}};
	Other ->
	    Other
    end.
