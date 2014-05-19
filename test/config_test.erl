-module(config_test).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-include("src/geef_records.hrl").

config_test_() ->
    {foreach, fun start/0, fun stop/1, [fun bool_test/1,
					fun string_test/1]}.

start() ->
    {A, B, C} = now(),
    N = node(),
    TmpFile = io_lib:format("/tmp/geef-~p~p~p~p.gitconfig", [N, A, B, C]),
    {ok, Config} = geef_config:open(TmpFile),
    {Config, TmpFile}.

bool_test({Config, _}) ->
    Var = "core.logallrefupdates",
    ok = geef_config:set(Config, Var, true),
    [?_assertEqual(ok, geef_config:set(Config, Var, true)),
     ?_assertEqual({ok, true}, geef_config:get_bool(Config, Var))].

string_test({Config, _}) ->
    Var = "user.name",
    Val = <<"Random J. Hacker">>,
    [?_assertEqual(ok, geef_config:set(Config, Var, Val)),
     ?_assertEqual({ok, Val}, geef_config:get_string(Config, Var))].

stop({_, Path}) ->
    ok = file:delete(Path).
