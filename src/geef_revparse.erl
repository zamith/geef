-module(geef_revparse).

-include("geef_records.hrl").

-export([single/2]).

-spec single(pid(), iolist()) -> {ok, geef_obj:object()} | {error, term()}.
single(Repo, Str) ->
    RepoHandle = geef_repo:handle(Repo),
    case geef_nif:revparse_single(RepoHandle, Str) of
	{ok, ObjHandle, Type, Id} ->
	    {ok, #geef_object{type=Type, id=Id, handle=ObjHandle}};
	Error = {error, _} ->
	    Error
    end.
