-module(pkt_test).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-include("src/geef_records.hrl").

flush_test() ->
    Expected = {flush, <<>>},
    Actual = geef_pkt:parse("0000"),
    ?assertEqual(Expected, Actual).

have_test() ->
    Expected = {{want, geef_oid:parse("e17ca7f2d877acbf8b9a9a1cb4c243ca72e86463")}, <<>>},
    Actual = geef_pkt:parse("0032want e17ca7f2d877acbf8b9a9a1cb4c243ca72e86463\n"),
    ?assertEqual(Expected, Actual).

request_test() ->
    Line = <<"0039git-upload-pack /schacon/gitbook.git\0host=example.com\0">>,
    Expected = #geef_request{service=upload_pack, path= <<"/schacon/gitbook.git">>, host= <<"example.com">>},
    {ok, Actual} = geef_pkt:parse_request(Line),
    ?assertEqual(Expected, Actual).

short_test() ->
    Line = <<"0039git-upload">>,
    Expected = {error, ebufs},
    Actual = geef_pkt:parse_request(Line),
    ?assertEqual(Expected, Actual).
