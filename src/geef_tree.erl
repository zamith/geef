-module(geef_tree).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([get/2, nth/2, count/1, lookup/2]).

-include("./src/geef_records.hrl").

-type tree() :: geef_obj:object(tree).
-type entry() :: #geef_tree_entry{}.
-export_type([tree/0, entry/0]).


from_nif({ok, Mode, Type, Id, Name}) ->
    {ok, #geef_tree_entry{mode=Mode, type=Type, id=Id, name=Name}};
from_nif(Err = {error, _}) ->
    Err.

-spec get(tree(), iolist()) -> {ok, entry()} | {error, term()}.
get(#geef_object{type=tree,handle=Handle}, Path) ->
    from_nif(geef_nif:tree_bypath(Handle, Path)).

-spec nth(tree(), non_neg_integer()) -> {ok, entry()} | {error, term()}.
nth(#geef_object{type=tree,handle=Handle}, Nth) ->
    from_nif(geef_nif:tree_nth(Handle, Nth)).

-spec count(tree()) -> non_neg_integer().
count(#geef_object{type=tree,handle=Handle}) ->
    geef_nif:tree_count(Handle).

-spec lookup(pid(), geef_oid:oid() | iolist()) -> {ok, tree()} | {error, term()}.
lookup(Repo, Id) ->
    geef_obj:lookup(Repo, Id, tree).

-ifdef(TEST).

%% This is somewhat hacky, assuming that this is running under .eunit,
%% but it's good enough for now
access_test() ->
    {ok, Repo} = geef_repo:open(".."),
    {ok, Tree} = geef_obj:lookup(Repo, geef_oid:parse("395e1c39cb203640b78da8458a42afdb92bef7aa")),
    Id = geef_oid:parse("80d5c15a040c93a4f98f4496a05ebf30cdd58650"),
    Expected = #geef_tree_entry{mode=8#100644, type=blob, id=Id, name = <<"README.md">>},
    {ok, ActualByPath}= geef_tree:get(Tree, "README.md"),
    {ok, ActualNth} = geef_tree:nth(Tree, 2),
    ?assertEqual(Expected, ActualByPath),
    ?assertEqual(Expected, ActualNth).
-endif.
