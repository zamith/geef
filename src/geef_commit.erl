-module(geef_commit).
-export([tree_id/1]).

-include("geef_records.hrl").

-spec tree_id(object()) -> oid().
tree_id(#object{type=commit,handle=Handle}) ->
    Oid = geef:commit_tree_id(Handle),
    #oid{oid=Oid}.
