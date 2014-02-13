%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-

-module(geef_reflog).

-include("geef_records.hrl").

-type entry() :: #geef_reflog_entry{}.
-export_type([entry/0]).

%% API
-export([read/2, delete/2]).

to_entry({Name, Email, Timestamp, Offset, IdOld, IdNew, Message}) ->
    Sig = geef_sig:convert(Name, Email, Timestamp, Offset),
    #geef_reflog_entry{committer=Sig, id_old=IdOld, id_new=IdNew, message=Message}.

%% @doc Read in a reflog
-spec read(pid(), iolist()) -> {ok, [entry()]} | {error, term()}.
read(Repo, Name) ->
    case geef_repo:reflog_read(Repo, Name) of
        {ok, List} ->
            {ok, lists:map(fun to_entry/1, List)};
        Other ->
            Other
    end.

delete(Repo, Name) ->
    geef_repo:reflog_delete(Repo, Name).
