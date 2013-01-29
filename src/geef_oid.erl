-module(geef_oid).

-include("geef_records.hlr").
-export([parse/1, fmt/1]).

-spec fmt(oid()) -> binary().
fmt(#oid{oid=Oid}) ->
    geef:oid_fmt(Oid).


-spec parse(binary()) -> oid().
parse(Sha) ->
    Oid = geef:oid_parse(Sha),
    #oid{oid=Oid}.
