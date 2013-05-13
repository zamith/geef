-module(geef_odb).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([exists/2, write/3]).

-include("geef_records.hrl").

-spec exists(geef_odb(), geef_oid() | iolist()) -> boolean().
exists(#geef_odb{handle=Handle}, #geef_oid{oid=Oid}) ->
    geef_nif:odb_object_exists(Handle, Oid);
exists(Odb = #geef_odb{}, Sha) ->
    exists(Odb, geef_oid:parse(Sha)).

-spec write(geef_odb(), iolist(), atom()) -> {ok, geef_oid()} | {error, term}.
write(#geef_odb{handle=Handle}, Contents, Type) ->
    case geef_nif:odb_write(Handle, Contents, Type) of
	{ok, Oid} ->
	    {ok, #geef_oid{oid=Oid}};
	Other ->
	    Other
    end.

-ifdef(TEST).

exists_test() ->
    {ok, Repo} = geef_repo:open(".."),
    {ok, Odb} = geef_repo:odb(Repo),
    ?assert(exists(Odb, "80d5c15a040c93a4f98f4496a05ebf30cdd58650")),
    ?assert(exists(Odb, geef_oid:parse("80d5c15a040c93a4f98f4496a05ebf30cdd58650"))).

-endif.
