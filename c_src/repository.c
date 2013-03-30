#include "repository.h"
#include "object.h"
#include "oid.h"
#include "geef.h"
#include <string.h>
#include <git2.h>

void geef_repository_free(ErlNifEnv *env, void *cd)
{
	geef_repository *grepo = (geef_repository *) cd;
	git_repository_free(grepo->repo);
}

void geef_odb_free(ErlNifEnv *env, void *cd)
{
	geef_odb *odb = (geef_odb *) cd;
	git_odb_free(odb->odb);
}

ERL_NIF_TERM
geef_repository_init(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	char *path;
	int error, bare;
	git_repository *repo;
	geef_repository *res_repo;
	ErlNifBinary bin;
	ERL_NIF_TERM term_repo;

	if (!enif_inspect_iolist_as_binary(env, argv[0], &bin))
		return enif_make_badarg(env);

	path = malloc(bin.size + 1);
	if (!path)
		return atoms.error;

	memcpy(path, bin.data, bin.size);
	path[bin.size] = '\0';

	bare = !enif_compare(argv[1], atoms.true);

	error = git_repository_init(&repo, path, bare);
	free(path);

	if (error < 0)
		return geef_error(env);

	res_repo = enif_alloc_resource(geef_repository_type, sizeof(geef_repository));
	res_repo->repo = repo;
	term_repo = enif_make_resource(env, res_repo);
	enif_release_resource(res_repo);

	return enif_make_tuple2(env, atoms.ok, term_repo);
}

ERL_NIF_TERM
geef_repository_open(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	char *path;
	int error;
	git_repository *repo;
	geef_repository *res_repo;
	ErlNifBinary bin;
	ERL_NIF_TERM term_repo;

	if (!enif_inspect_iolist_as_binary(env, argv[0], &bin))
		return enif_make_badarg(env);

	path = malloc(bin.size + 1);
	if (!path)
		return atoms.error;

	memcpy(path, bin.data, bin.size);
	path[bin.size] = '\0';

	error = git_repository_open(&repo, path);
	free(path);

	if (error < 0)
		return geef_error(env);

	res_repo = enif_alloc_resource(geef_repository_type, sizeof(geef_repository));
	res_repo->repo = repo;
	term_repo = enif_make_resource(env, res_repo);
	enif_release_resource(res_repo);

	return enif_make_tuple2(env, atoms.ok, term_repo);
}

ERL_NIF_TERM
geef_repository_path(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_repository *repo;
	const char *path;
	size_t len;
	ErlNifBinary bin;

	if (!enif_get_resource(env, argv[0], geef_repository_type, (void **) &repo))
		return enif_make_badarg(env);

	path = git_repository_path(repo->repo);
	len = strlen(path);

	if (!enif_alloc_binary(len, &bin))
		return atoms.error;

	memcpy(bin.data, path, len);
	return enif_make_binary(env, &bin);
}

ERL_NIF_TERM
geef_repository_workdir(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_repository *repo;
	const char *path;
	size_t len;
	ErlNifBinary bin;

	if (!enif_get_resource(env, argv[0], geef_repository_type, (void **) &repo))
		return enif_make_badarg(env);

	if (git_repository_is_bare(repo->repo))
		return atoms.error;

	path = git_repository_workdir(repo->repo);
	len = strlen(path);

	if (!enif_alloc_binary(len, &bin))
		return atoms.error;

	memcpy(bin.data, path, len);
	return enif_make_binary(env, &bin);

}

ERL_NIF_TERM
geef_repository_is_bare(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_repository *repo;
	int bare;

	if (!enif_get_resource(env, argv[0], geef_repository_type, (void **) &repo))
		return enif_make_badarg(env);

	bare = git_repository_is_bare(repo->repo);

	if (bare)
		return atoms.true;

	return atoms.false;

}

ERL_NIF_TERM
geef_repository_odb(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_repository *repo;
	geef_odb *odb;
	ERL_NIF_TERM term_odb;

	if (!enif_get_resource(env, argv[0], geef_repository_type, (void **) &repo))
		return enif_make_badarg(env);

	odb = enif_alloc_resource(geef_odb_type, sizeof(geef_odb));
	if (git_repository_odb(&odb->odb, repo->repo) < 0)
		return geef_error(env);

	term_odb = enif_make_resource(env, odb);
	enif_release_resource(odb);

	return enif_make_tuple2(env, atoms.ok, term_odb);
}

ERL_NIF_TERM
geef_odb_exists(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	char sha[MAXBUFLEN] = {0};
	geef_odb *odb;
	git_oid oid;
	int exists;

	if (!enif_get_resource(env, argv[0], geef_odb_type, (void **) &odb))
		return enif_make_badarg(env);

	if (enif_get_string(env, argv[1], sha, sizeof(sha), ERL_NIF_LATIN1) < 1)
		return enif_make_badarg(env);

	git_oid_fromstr(&oid, sha);
	exists = git_odb_exists(odb->odb, &oid);

	if (exists)
		return atoms.true;

	return atoms.false;
}

ERL_NIF_TERM
geef_odb_write(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	git_otype type;
	git_oid oid;
	geef_odb *odb;
	ErlNifBinary contents, oid_bin;

	if (!enif_get_resource(env, argv[0], geef_odb_type, (void **) &odb))
		return enif_make_badarg(env);

	if (!enif_inspect_iolist_as_binary(env, argv[1], &contents))
		return enif_make_badarg(env);

	type = geef_object_atom2type(argv[2]);
	if (git_odb_write(&oid, odb->odb, contents.data, contents.size, type) < 0)
		return geef_error(env);

	if (geef_oid_bin(&oid_bin, &oid) < 0)
		return atoms.error;

	return enif_make_tuple2(env, atoms.ok, enif_make_binary(env, &oid_bin));
}
