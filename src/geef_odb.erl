-module(geef_odb).
-export([exists/2]).

-include("geef_records.hlr").

exists(#odb{handle=Handle}, Sha) ->
    geef:odb_object_exists(Handle, Sha).
