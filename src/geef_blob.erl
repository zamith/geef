-module(geef_blob).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("./src/geef_records.hrl").

-type blob() :: geef_obj:object(blob).
-export_type([blob/0]).

-export([lookup/2, size/1, content/1]).

-spec lookup(pid(), geef_oid:oid() | iolist()) -> {ok, blob()} | {error, term()}.
lookup(Repo, Id) ->
    geef_obj:lookup(Repo, Id, blob).

size(#geef_object{type=blob, handle=Handle}) ->
    geef_nif:blob_size(Handle).

content(#geef_object{type=blob, handle=Handle}) ->
    {ok, Content} = geef_nif:blob_content(Handle),
    Content.


-ifdef(TEST).

blob_size_test() ->
    {ok, Repo} = geef_repo:open(".."),
    {ok, Blob} = lookup(Repo, geef_oid:parse("80d5c15a040c93a4f98f4496a05ebf30cdd58650")),
    ?assertMatch(889, ?MODULE:size(Blob)),
    ?assertMatch(889, erlang:size(content(Blob))).

-endif.
