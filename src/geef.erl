-module(geef).
-export([hex_to_raw/1, repository/1, repository_get_path/1, repository_get_odb/1,
	 repository_init/2, repository_is_bare/1, repository_get_workdir/1, reference_list/1,
	 reference_to_id/2, reference_glob/2, reference_lookup/2, reference_resolve/1,
	 reference_id/1, odb_object_exists/2, oid_fmt/1]).
-on_load(load_enif/0).

hex_to_raw(_Val) ->
    nif_error(?LINE).    

repository_open(_Val) ->
    nif_error(?LINE).

repository_is_bare(_Handle) ->
    nif_error(?LINE).

repository_get_path(_Val) ->
    nif_error(?LINE).

repository_get_workdir(_Handle) ->
    nif_error(?LINE).

repository_get_odb(_Val) ->
    nif_error(?LINE).

repository_init(_Path, _Bare) ->
    nif_error(?LINE).

reference_list(_Repo) ->
    nif_error(?LINE).

reference_to_id(_Repo, _Refname) ->
    nif_error(?LINE).

reference_glob(_Repo, _Glob) ->
    nif_error(?LINE).

reference_lookup(_Repo, _Refname) ->
    nif_error(?LINE).

reference_id(_Handle) ->
    nif_error(?LINE).

reference_resolve(_Handle) ->
    nif_error(?LINE).

repository(Path) ->
    case repository_open(Path) of
	{ok, Handle}  ->
	    repository:new(Handle);
	Other ->
	    Other
    end.

odb_object_exists(_Val, _Val) ->
    nif_error(?LINE).

oid_fmt(_Oid) ->
    nif_error(?LINE).

nif_error(Line) ->
    exit({nif_not_loaded,module,?MODULE,line,Line}).

load_enif() ->
    case code:priv_dir(geef) of
        {error, bad_name} ->
            SoName = filename:join("priv", geef);
        Dir ->
            SoName = filename:join(Dir, geef)
    end,
    io:format("Hey There: ~s~n", [SoName]),
    erlang:load_nif(SoName, 0).
