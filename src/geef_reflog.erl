%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-

-module(geef_reflog).

-include("geef_records.hrl").

-type entry() :: #geef_reflog_entry{}.
-export_type([entry/0]).

%% API
-export([read/2]).

%% @doc Read in a reflog
-spec read(pid(), iolist()) -> {ok, [entry()]} | {error, term()}.
read(Repo, Name) ->
    geef_repo:reflog_read(Repo, Name).
