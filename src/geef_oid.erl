%%% @doc Dealing with object names
-module(geef_oid).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("geef_records.hrl").

-export([parse/1, fmt/1]).

%% @doc Get the hex-encoded hash
-spec fmt(oid()) -> binary().
fmt(#oid{oid=Oid}) ->
    geef:oid_fmt(Oid).

%% @doc Parse an iolist as a hash
-spec parse(iolist()) -> oid().
parse(Sha) ->
    Oid = geef:oid_parse(Sha),
    #oid{oid=Oid}.

-ifdef(TEST).

back_and_forth_test() ->
    ?assertMatch(<<"d71c6ff702e75247ce29c51279d78a7a202f5cc9">>,
		 fmt(parse(<<"d71c6ff702e75247ce29c51279d78a7a202f5cc9">>))),
    ?assertMatch(<<"d71c6ff702e75247ce29c51279d78a7a202f5cc9">>,
		 fmt(parse("d71c6ff702e75247ce29c51279d78a7a202f5cc9"))).

-endif.
