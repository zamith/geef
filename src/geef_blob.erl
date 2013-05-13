-module(geef_blob).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("geef_records.hrl").

-export([lookup/2, id/1, size/1, content/1]).

-spec lookup(pid(), geef_oid() | iolist()) -> geef_object().
lookup(Repo, Id) ->
    geef_object:lookup(Repo, Id, blob).

-spec id(geef_object()) -> geef_oid().
id(Obj = #geef_object{type=blob}) ->
    geef_object:id(Obj).

size(#geef_object{type=blob, handle=Handle}) ->
    geef_nif:blob_size(Handle).

content(#geef_object{type=blob, handle=Handle}) ->
    {ok, Content} = geef_nif:blob_content(Handle),
    Content.


-ifdef(TEST).

blob_size_test() ->
    {ok, Repo} = geef_repo:open(".."),
    {ok, Blob} = lookup(Repo, "80d5c15a040c93a4f98f4496a05ebf30cdd58650"),
    ?assertMatch(889, ?MODULE:size(Blob)),
    ?assertMatch(889, erlang:size(content(Blob))).

-endif.
