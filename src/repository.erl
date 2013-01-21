-module(repository, [Handle]).
-export([path/0, workdir/0, bare/0, references/0]).

path() ->
    geef:repository_get_path(Handle).

workdir() ->
    geef:repository_get_workdir(Handle).

bare() ->
    geef:repository_is_bare(Handle).

references() ->
    geef:reference_list(Handle).
