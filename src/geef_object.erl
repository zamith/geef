-module(geef_object).

-include("geef_records.hrl").

-export([lookup/2]).

-spec lookup(repo(), oid()) -> object().
lookup(#repo{handle=RepoHandle}, #oid{oid=Oid}) ->
    case geef:object_lookup(RepoHandle, Oid) of
	{ok, Type, Handle} ->
	    {ok, #object{type=Type, handle=Handle}};
	Other ->
	    Other
    end.
