-module(geef_repo).

-export([open/1, init/2, path/1, workdir/1, odb/1, is_bare/1, references/1, discover/1]).

-include("geef_records.hrl").

%% @doc The repository's git-dir path
-spec path(repo()) -> binary().
path(#repo{handle=Handle}) ->
    geef:repository_get_path(Handle).

%% @doc The repository's worktree path
-spec workdir(repo()) -> binary().
workdir(#repo{handle=Handle}) ->
    geef:repository_get_workdir(Handle).

%% @doc The repository's current object database. This encompasses all
%% the configured backends.
-spec odb(repo()) -> {ok, odb} | {error, term}.
odb(#repo{handle=Handle}) ->
    case geef:repository_get_odb(Handle) of
	{ok, OdbHandle} ->
	    {ok, #odb{handle=OdbHandle}};
	Other ->
	    Other
    end.

%% @doc Whether the repository is bare
-spec is_bare(repo()) -> boolean().
is_bare(#repo{handle=Handle}) ->
    geef:repository_is_bare(Handle).

%% @doc List of references in the repository.
-spec references(repo()) -> [binary()].
references(#repo{handle=Handle}) ->
    geef:reference_list(Handle).

%% @doc Discover a repository's path from a path contained inside it
-spec discover(iolist()) -> {ok, binary()} | error | {error, term()}.
discover(Path) ->
    geef:repository_discover(Path).

%% @doc Open an existing repository. Path must point to the git-dir or
%% worktree
-spec open(iolist()) -> {ok, repo()} | {error, term()}.
open(Path) ->
    case geef:repository_open(Path) of
	{ok, Handle} ->
	    {ok, #repo{handle=Handle}};
	Other ->
	    Other
    end.

%% @doc Initialize a new repository
-spec init(iolist(), boolean()) -> {ok, repo()} | {error, term()}.
init(Path, Bare) ->
    case geef:repository_init(Path, Bare) of
	{ok, Handle} ->
	    {ok, #repo{handle=Handle}};
	Other ->
	    Other
    end.	
