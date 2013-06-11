-module(sig_test).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-include("src/geef_records.hrl").

now_test() ->
    Sig = geef_sig:new("Carlos", "a@b.c"),
    ?assertMatch(#geef_signature{name = <<"Carlos">>, email = <<"a@b.c">>, time=_}, Sig).

at_test() ->
    Time = {os:timestamp(), 120},
    Actual = geef_sig:new("Carlos", "a@b.c", Time),
    Expected = #geef_signature{name = <<"Carlos">>, email = <<"a@b.c">>, time = Time},
    ?assertEqual(Expected, Actual).
