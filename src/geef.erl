-module(geef).
-export([hex_to_raw/1, reference_list/1,
	 reference_to_id/2, reference_glob/2, reference_lookup/2, reference_resolve/1,
	 reference_target/1, reference_type/1, odb_object_exists/2]).

% repository operations
-export([repository_get_path/1, repository_get_odb/1, repository_open/1, repository_init/2,
	 repository_is_bare/1, repository_get_workdir/1]).

% oid parsing
-export([oid_fmt/1, oid_parse/1]).

% objects
-export([object_lookup/2, commit_tree_id/1, commit_tree/1]).

-on_load(load_enif/0).

hex_to_raw(_Val) ->
    nif_error(?LINE).

-spec repository_open(iolist()) -> {ok, term()} | {error, term()}.
repository_open(_Val) ->
    nif_error(?LINE).

-spec repository_is_bare(term()) -> boolean().
repository_is_bare(_Handle) ->
    nif_error(?LINE).

-spec repository_get_path(term()) -> binary().
repository_get_path(_Val) ->
    nif_error(?LINE).

-spec repository_get_workdir(term()) -> binary().
repository_get_workdir(_Handle) ->
    nif_error(?LINE).

repository_get_odb(_Val) ->
    nif_error(?LINE).

-spec repository_init(iolist(), boolean()) -> term().
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

reference_target(_Handle) ->
    nif_error(?LINE).

reference_resolve(_Handle) ->
    nif_error(?LINE).

reference_type(_Handle) ->
    nif_error(?LINE).

odb_object_exists(_Val, _Val) ->
    nif_error(?LINE).

oid_fmt(_Oid) ->
    nif_error(?LINE).

oid_parse(_Sha) ->
    nif_error(?LINE).

object_lookup(_Repo, _Oid) ->
    nif_error(?LINE).

-spec commit_tree_id(term) -> binary().
commit_tree_id(_Handle) ->
    nif_error(?LINE).

-spec commit_tree(term) -> term().
commit_tree(_Handle) ->
    nif_error(?LINE).

nif_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

load_enif() ->
    case code:priv_dir(geef) of
        {error, bad_name} ->
            SoName = filename:join("priv", geef);
        Dir ->
            SoName = filename:join(Dir, geef)
    end,
    erlang:load_nif(SoName, 0).
