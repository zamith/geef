-module(geef_tree).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([bypath/2, id/1]).

-include("geef_records.hrl").

bypath(#object{type=tree,handle=Handle}, Path) ->
    case geef:tree_bypath(Handle, Path) of
	{ok, Mode, Type, Id, Name} ->
	    {ok, #tree_entry{mode=Mode, type=Type, id=geef_oid:parse(Id), name=Name}};
	Other ->
	    Other
    end.

id(Obj = #object{type=tree}) ->
    geef_object:id(Obj).

-ifdef(TEST).

%% This is somewhat hacky, assuming that this is running under .eunit,
%% but it's good enough for now
bypath_test() ->
    {ok, Repo} = geef_repo:open(".."),
    {ok, Tree} = geef_object:lookup(Repo, geef_oid:parse("395e1c39cb203640b78da8458a42afdb92bef7aa")),
    {ok, #tree_entry{type=blob, name= <<"README.md">>}} = geef_tree:bypath(Tree, "README.md").

-endif.
