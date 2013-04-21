-module(geef_blob).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("geef_records.hrl").

-export([lookup/2, id/1, size/1, content/1]).

-spec lookup(repo(), oid() | iolist()) -> object().
lookup(Repo, Id) ->
    geef_object:lookup(Repo, Id, blob).

-spec id(object()) -> oid().
id(Obj = #object{type=blob}) ->
    geef_object:id(Obj).

size(#object{type=blob, handle=Handle}) ->
    geef:blob_size(Handle).

content(#object{type=blob, handle=Handle}) ->
    {ok, Content} = geef:blob_content(Handle),
    Content.


-ifdef(TEST).

blob_size_test() ->
    {ok, Repo} = geef_repo:open(".."),
    Blob = lookup(Repo, "80d5c15a040c93a4f98f4496a05ebf30cdd58650"),
    ?assertMatch(889, ?MODULE:size(Blob)),
    ?assertMatch(889, erlang:size(content(Blob))).

-endif.
