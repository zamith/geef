-module(pkt_test).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-include("src/geef_records.hrl").

flush_test() ->
    geef_pkt:parse("0000").

have_test() ->
    Expected = {{want, geef_oid:parse("e17ca7f2d877acbf8b9a9a1cb4c243ca72e86463")}, <<>>},
    Actual = geef_pkt:parse("0032want e17ca7f2d877acbf8b9a9a1cb4c243ca72e86463\n"),
    ?assertEqual(Expected, Actual).
