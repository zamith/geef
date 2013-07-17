%%% @private
%%% NIF functions, not to be used directly.

-module(geef_nif).
-compile(export_all).

-on_load(load_enif/0).

-define(NIF_FN, nif_error(?LINE)).

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

-spec repository_discover(iolist()) -> {ok, string()} | error.
repository_discover(_Path) ->
    nif_error(?LINE).

reference_list(_Repo) ->
    nif_error(?LINE).

reference_create(_Repo, _Refname, _Type, _Target, _Force) ->
    ?NIF_FN.

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

reference_name(_Handle) ->
    nif_error(?LINE).

reference_dwim(_Handle, _Name) ->
    ?NIF_FN.

odb_object_exists(_Val, _Val) ->
    nif_error(?LINE).

-spec odb_write(term(), iolist(), atom()) -> term().
odb_write(_Handle, _Contents, _Type) ->
    nif_error(?LINE).

oid_fmt(_Oid) ->
    nif_error(?LINE).

-spec oid_parse(iolist()) -> binary().
oid_parse(_Sha) ->
    nif_error(?LINE).

object_lookup(_Repo, _Oid) ->
    nif_error(?LINE).

object_id(_Handle) ->
    nif_error(?LINE).

-spec commit_tree_id(term) -> binary().
commit_tree_id(_Handle) ->
    nif_error(?LINE).

-spec commit_tree(term) -> term().
commit_tree(_Handle) ->
    nif_error(?LINE).

-spec tree_bypath(term, iolist()) -> term().
tree_bypath(_TreeHandle, _Path) ->
    nif_error(?LINE).

-spec tree_nth(term, non_neg_integer()) -> term().
tree_nth(_TreeHandle, _Nth) ->
    ?NIF_FN.

-spec blob_size(term) -> {ok, integer()} | error.
blob_size(_ObjHandle) ->
    nif_error(?LINE).

-spec blob_content(term) -> {ok, binary()} | error.
blob_content(_ObjHandle) ->
    nif_error(?LINE).

-spec tag_peel(term()) -> {ok, atom(), binary(), term()} | {error, term()}.
tag_peel(_Tag) ->
    ?NIF_FN.

-spec library_version() -> {integer, integer, integer}.
library_version() ->
    nif_error(?LINE).

revwalk_new(_Repo) ->
    ?NIF_FN.

revwalk_push(_Walk, _Id, _Hide) ->
    ?NIF_FN.

revwalk_next(_Walk) ->
    ?NIF_FN.

revwalk_sorting(_Walk, _Sort) ->
    ?NIF_FN.

revwalk_reset(_Walk) ->
    ?NIF_FN.

index_new() ->
    ?NIF_FN.

index_write(_Handle) ->
    ?NIF_FN.

index_write_tree(_Handle) ->
    ?NIF_FN.

index_write_tree(_Handle, _RepoHandle) ->
    ?NIF_FN.

index_read_tree(_Handle, _TreeHandle) ->
    ?NIF_FN.

index_add(_Handle, _Entry) ->
    ?NIF_FN.

index_clear(_Handle) ->
    ?NIF_FN.

signature_new(_Name, _Email) ->
    ?NIF_FN.

signature_new(_Name, _Email, _Time) ->
    ?NIF_FN.

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
