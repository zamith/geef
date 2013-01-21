-module(repository, [Handle]).
-export([path/0, workdir/0, bare/0, references/0, odb/0, lookup/1]).

path() ->
    geef:repository_get_path(Handle).

workdir() ->
    geef:repository_get_workdir(Handle).

bare() ->
    geef:repository_is_bare(Handle).

references() ->
    geef:reference_list(Handle).

odb() ->
    case geef:repository_get_odb(Handle) of
	{ok, Odb} ->
	    odb:new(Odb);
	other ->
	    other
    end.

lookup(Refname) ->
    case geef:repositiory_lookup_reference(Handle, Refname) of
	{ok, Ref} ->
	    reference:new(Ref);
	other ->
	    other
    end.
