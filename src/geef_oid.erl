-module(geef_oid).

-export([parse/1, fmt/1]).

-spec fmt(binary()) -> binary().
fmt(Oid) ->
    geef:oid_fmt(Oid).


-spec parse(binary()) -> binary().
parse(Sha) ->
    geef:oid_parse(Sha).
