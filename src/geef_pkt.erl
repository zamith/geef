-module(geef_pkt).

-export([line/1, parse/1, parse_request/1]).

-include("geef_records.hrl").

-define(SHA_LEN, 40).

-spec line(iolist()) -> iolist().
line(Text) ->
    % prefix's own size + text size + LF
    Len = 4 + iolist_size(Text) + 1,
    Prefix = io_lib:format("~4.16.0b", [Len]),
    [Prefix, Text, "\n"].

-spec parse(iolist()) -> {{want | have, geef_oid()}, binary()}.
parse(In) ->
    {Len, Rest} = unpack(In),
    case Len of
	0 ->
	    flush;
	_ ->
	    parse_pkt(Rest, Len - 4)
    end.

-spec parse_request(iolist()) -> geef_request().
parse_request(In) ->
    {_, Line} = unpack(In),
    %% Split it into request, host, rest (should be empty)
    [S, H, _] = binary:split(Line, <<0>>, [global]),
    case S of
	<<"git-upload-pack ", Path/binary>> ->
	    Service = upload_pack;
	<<"git-receive-pack ", Path/binary>> ->
	    Service = receive_pack
    end,
    <<"host=", Host/binary>> = H,
    #geef_request{service=Service, path=Path, host=Host}.

-spec unpack(iolist()) -> {non_neg_integer(), binary()}.
unpack(In0) ->
    In = iolist_to_binary(In0),
    <<BLen:4/binary, Rest/binary>> = In,
    Len = binary_to_integer(BLen, 16),
    {Len, Rest}.

parse_pkt(In, Len) when size(In) < Len ->
    {error, ebufs};
parse_pkt(In, Len0) ->
    Len1 = Len0 - 5, % "want " | "have "
    case In of
	<<"want ", Sha:?SHA_LEN/binary, Rest0/binary>> ->
	    Pkt = {want, geef_oid:parse(Sha)};
	<<"have ", Sha:?SHA_LEN/binary, Rest0/binary>> ->
	    Pkt = {have, geef_oid:parse(Sha)}
    end,
    Len2 = Len1 - ?SHA_LEN, % get rid of the LF if we have it
    <<_:Len2/binary, Rest/binary>> = Rest0,
    {Pkt, Rest}.
