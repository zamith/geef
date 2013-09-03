-module(geef_tag).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([lookup/2, peel/1]).

-include("geef_records.hrl").

-type tag() :: geef_obj:object(tag).
-export_type([tag/0]).

-spec lookup(pid(), geef_oid:oid() | iolist()) -> {ok, tag()} | {error, term()}.
lookup(Repo, Id) ->
    geef_obj:lookup(Repo, Id, tag).

-spec peel(tag()) -> {ok, geef_obj:object()} | {error, term()}.
peel(#geef_object{type=tag, handle=Handle}) ->
    case geef_nif:tag_peel(Handle) of
        {ok, Type, Id, PeeledHandle} ->
	    {ok, #geef_object{type=Type, id=Id, handle=PeeledHandle}};
	Err = {error, _} ->
	    Err
    end.
