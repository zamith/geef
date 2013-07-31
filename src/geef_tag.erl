-module(geef_tag).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([lookup/2, peel/1]).

-include("geef_records.hrl").

-spec lookup(pid(), geef_oid() | iolist()) -> {ok, geef_object()} | {error, term()}.
lookup(Repo, Id) ->
    geef_obj:lookup(Repo, Id, tag).

-spec peel(geef_object()) -> {ok, geef_object()} | {error, term()}.
peel(#geef_object{type=tag, handle=Handle}) ->
    case geef_nif:tag_peel(Handle) of
        {ok, Type, Oid, PeeledHandle} ->
	    {ok, #geef_object{type=Type, id=#geef_oid{oid=Oid}, handle=PeeledHandle}};
	Err = {error, _} ->
	    Err
    end.
