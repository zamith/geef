-module(repo_test).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

repo_test_() ->
    {foreach, fun start/0, fun stop/1, [fun bare_test/1, fun odb_write_test/1]}.

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

stop(_Repo) ->
    ok.
