-module(geef_commit).
-export([tree_id/1, tree/1, lookup/2]).

-include("geef_records.hrl").

-spec tree_id(geef_object()) -> geef_oid().
tree_id(#geef_object{type=commit,handle=Handle}) ->
    Oid = geef_nif:commit_tree_id(Handle),
    #geef_oid{oid=Oid}.

tree(#geef_object{type=commit,handle=Handle}) ->
    case geef_nif:commit_tree(Handle) of
	{ok, Type, Handle} ->
	    {ok, #geef_object{type=Type, handle=Handle}};
	Other ->
	    Other
    end.

-spec lookup(pid(), geef_oid() | iolist()) -> geef_object().
lookup(Repo, Id) ->
    geef_obj:lookup(Repo, Id, commit).
