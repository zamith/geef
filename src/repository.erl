-module(repository, [Handle]).
-export([path/0, workdir/0]).

path() ->
    geef:repository_get_path(Handle).

workdir() ->
    geef:repository_get_workdir(Handle).
