-module(geef_repo).

-export([open/1, init/2, path/1, workdir/1, is_bare/1, references/1]).

-include("geef_records.hlr").

path(#repo{handle=Handle}) ->
    geef:repository_get_path(Handle).

workdir(#repo{handle=Handle}) ->
    geef:repository_get_workdir(Handle).

is_bare(#repo{handle=Handle}) ->
    geef:repository_is_bare(Handle).

references(#repo{handle=Handle}) ->
    geef:reference_list(Handle).

-spec open(iolist()) -> {ok, term()} | {error, term()}.
open(Path) ->
    case geef:repository_open(Path) of
	{ok, Handle} ->
	    {ok, #repo{handle=Handle}};
	Other ->
	    Other
    end.

-spec init(iolist(), boolean()) -> {ok, repo()} | {error, term()}.
init(Path, Bare) ->
    case geef:repository_init(Path, Bare) of
	{ok, Handle} ->
	    {ok, #repo{handle=Handle}};
	Other ->
	    Other
    end.	
