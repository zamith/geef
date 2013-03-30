-module(geef_odb).
-export([exists/2, write/3]).

-include("geef_records.hrl").

-spec exists(odb(), string()) -> boolean().
exists(#odb{handle=Handle}, Sha) ->
    geef:odb_object_exists(Handle, Sha).

-spec write(odb(), iolist(), atom()) -> {ok, oid()} | {error, term}.
write(#odb{handle=Handle}, Contents, Type) ->
    case geef:odb_write(Handle, Contents, Type) of
	{ok, Oid} ->
	    {ok, #oid{oid=Oid}};
	Other ->
	    Other
    end.
