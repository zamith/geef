-module(geef_revwalk).

-export([new/1, push/2, hide/2, next/1, sorting/2]).

-include("geef_records.hrl").


%% @doc Create a revision walker
-spec new(repo()) -> {ok, revwalk()} | {error, term}.
new(#repo{handle=Handle}) ->
    case geef:revwalk_new(Handle) of
	{ok, WalkHandle} ->
	    {ok, #revwalk{handle=WalkHandle}};
	Other ->
	    Other
    end.


%% @doc Push a commit. This commit and its parents will be included in
%% the walk as long as they haven't been hidden. At least one commit
%% must be pushed before starting a walk.
-spec push(revwalk(), oid()) -> ok | {error, binary()}.
push(#revwalk{handle=Handle}, #oid{oid=Oid}) ->
    geef:revwalk_push(Handle, Oid, false).

%% @doc Hide a commit. Hide a commit and its parents. Any Parent of
%% this commit won't be included in the walk.
-spec hide(revwalk(), oid()) -> ok | {error, binary()}.
hide(#revwalk{handle=Handle}, #oid{oid=Oid}) ->
    geef:revwalk_push(Handle, Oid, true).

%% @doc Next commit in the walk
next(#revwalk{handle=Handle}) ->
    case geef:revwalk_next(Handle) of
	{ok, Oid} ->
	    {ok, #oid{oid=Oid}};
	Other ->
	    Other
    end.

sorting(#revwalk{handle=Handle}, Opts) when is_list(Opts) ->
    geef:revwalk_sorting(Handle, Opts);
sorting(Walk = #revwalk{}, Opt) when is_atom(Opt) ->
    sorting(Walk, [Opt]).

