-module(geef_blob).


-include("geef_records.hrl").

-export([lookup/2, id/1]).

-spec lookup(repo(), oid() | iolist()) -> object().
lookup(Repo, Id) ->
    {ok, Obj = #object{type=blob}} = geef_object:lookup(Repo, Id),
    Obj.

-spec id(object()) -> oid().
id(Obj = #object{type=blob}) ->
    geef_object:id(Obj).
