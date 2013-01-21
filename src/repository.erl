-module(repository, [Handle]).
-export([path/0, workdir/0, bare/0, references/0, reference_glob/1, odb/0, lookup/1]).

-spec path() -> binary().
path() ->
    geef:repository_get_path(Handle).

-spec workdir() -> binary().
workdir() ->
    geef:repository_get_workdir(Handle).

-spec bare() -> boolean().
bare() ->
    geef:repository_is_bare(Handle).

-spec references() -> [binary()] | [].
references() ->
    geef:reference_list(Handle).

-spec reference_glob(binary()) -> [binary()].
reference_glob(Glob) ->
    geef:reference_glob(Handle, Glob).

odb() ->
    case geef:repository_get_odb(Handle) of
	{ok, Odb} ->
	    odb:new(Odb);
	Other ->
	    Other
    end.

lookup(Refname) ->
    case geef:reference_lookup(Handle, Refname) of
	{ok, Ref} ->
	    reference:new(Ref);
	Other ->
	    Other
    end.
