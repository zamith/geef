-module(geef_commit).
-export([tree_id/1, tree/1, id/1]).

-include("geef_records.hrl").

-spec tree_id(object()) -> oid().
tree_id(#object{type=commit,handle=Handle}) ->
    Oid = geef:commit_tree_id(Handle),
    #oid{oid=Oid}.

tree(#object{type=commit,handle=Handle}) ->
    case geef:commit_tree(Handle) of
	{ok, Type, Handle} ->
	    {ok, #object{type=Type, handle=Handle}};
	Other ->
	    Other
    end.

id(Obj = #object{type=commit}) ->
    geef_object:id(Obj).
