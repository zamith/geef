%%% @doc Dealing with object names
-module(geef_oid).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("geef_records.hrl").

-type oid() :: #geef_oid{oid :: binary()}.
-export_type([oid/0]).

-export([parse/1, hex/1]).

%% @doc Get a hex-encoded string of the hash
-spec hex(geef_oid:oid()) -> binary().
hex(#geef_oid{oid=Oid}) ->
    geef_nif:oid_fmt(Oid).

%% @doc Parse an iolist as a hash
-spec parse(iolist() | binary()) -> geef_oid:oid().
parse(Sha) ->
    Oid = geef_nif:oid_parse(Sha),
    #geef_oid{oid=Oid}.

-ifdef(TEST).

back_and_forth_test() ->
    ?assertMatch(<<"d71c6ff702e75247ce29c51279d78a7a202f5cc9">>,
		 hex(parse(<<"d71c6ff702e75247ce29c51279d78a7a202f5cc9">>))),
    ?assertMatch(<<"d71c6ff702e75247ce29c51279d78a7a202f5cc9">>,
		 hex(parse("d71c6ff702e75247ce29c51279d78a7a202f5cc9"))).

-endif.
