-module(repo_test).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-include("src/geef_records.hrl").

repo_test_() ->
    {foreach, fun start/0, fun stop/1, [fun bare_test/1, fun odb_write_test/1,
					fun create_ref_test/1, fun index_add_test/1]}.

start() ->
    {A, B, C} = now(),
    N = node(),
    TmpDir = io_lib:format("/tmp/geef-~p~p~p~p.git", [N, A, B, C]),
    {ok, Repo} = geef_repo:init(TmpDir, true),
    Repo.

bare_test(Repo) ->
    [?_assertEqual(geef_repo:is_bare(Repo), true)].

odb_write_test(Repo) ->
    Data = <<"This is some text that will go in a file">>,
    {ok, Odb} = geef_repo:odb(Repo),
    {ok, Actual} = geef_odb:write(Odb, Data, blob),
    Expected = geef_oid:parse("c300118399f01fe52b316061b5d32beb27e0adfd"),
    [?_assertEqual(Actual, Expected)].

index_add_test(Repo) ->
    Data = <<"This is some text that will go in a file">>,
    {ok, Odb} = geef_repo:odb(Repo),
    {ok, BlobId} = geef_odb:write(Odb, Data, blob),
    {ok, Idx} = geef_index:new(),
    Entry = #geef_index_entry{mode=8#100644, id=BlobId, path="README"},
    ok = geef_index:add(Idx, Entry),
    {ok, TreeId} = geef_index:write_tree(Idx, Repo),
    Expected = geef_oid:parse("5a20bbbf65ea75ad4d9f995d179156824ccca3a1"),
    [?_assertEqual(Expected, TreeId)].

create_ref_test(Repo) ->
    odb_write_test(Repo),
    Id = geef_oid:parse("c300118399f01fe52b316061b5d32beb27e0adfd"),
    {ok, _} = geef_ref:create(Repo, "refs/heads/branch", Id, true),
    {ok, _} = geef_ref:create(Repo, "refs/heads/other", "refs/heads/branch", true),
    {ok, Ref0} = geef_ref:lookup(Repo, "refs/heads/branch"),
    {ok, Ref1} = geef_ref:lookup(Repo, "refs/heads/other"),
    [?_assertEqual(Ref0#geef_reference.target, Id),
     ?_assertEqual(Ref1#geef_reference.target, <<"refs/heads/branch">>)].

rm_r(Path) ->
    case filelib:is_dir(Path) of
	false ->
	    file:delete(Path);
	true ->
	    {ok, Entries} = file:list_dir(Path),
	    lists:foreach(fun(X) -> ok = rm_r(filename:join([Path, X])) end, Entries),
	    file:del_dir(Path)
    end.

stop(Repo) ->
    Path = geef_repo:path(Repo),
    geef_repo:stop(Repo),
    ok = rm_r(Path).
