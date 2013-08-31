-module(geef_commit).
-export([tree_id/1, tree/1, lookup/2]).

-include("geef_records.hrl").

-type commit() :: geef_obj:object(commit).
-export_type([commit/0]).

-spec tree_id(commit()) -> geef_oid:oid().
tree_id(#geef_object{type=commit,handle=Handle}) ->
    Oid = geef_nif:commit_tree_id(Handle),
    #geef_oid{oid=Oid}.

-spec tree(commit()) -> {ok, geef_tree:tree()} | {error, term()}.
tree(#geef_object{type=commit,handle=Handle}) ->
    case geef_nif:commit_tree(Handle) of
	{ok, Type, Handle} ->
	    {ok, #geef_object{type=Type, handle=Handle}};
	Other ->
	    Other
    end.

-spec lookup(pid(), geef_oid:oid() | iolist()) -> {ok, commit()} | {error, term()}.
lookup(Repo, Id) ->
    geef_obj:lookup(Repo, Id, commit).
