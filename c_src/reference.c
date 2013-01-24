#include "geef.h"
#include "repository.h"
#include "reference.h"
#include "oid.h"
#include <string.h>
#include <git2.h>

ERL_NIF_TERM
geef_reference_list(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	unsigned int flags = GIT_REF_LISTALL;
	size_t i;
	git_strarray array;
	geef_repository *repo;
	ERL_NIF_TERM list;

	if (!enif_get_resource(env, argv[0], geef_repository_type, (void **) &repo))
		return enif_make_badarg(env);

	if (git_reference_list(&array, repo->repo, flags) < 0)
		return geef_error(env);

	list = enif_make_list(env, 0);
	for (i = 0; i < array.count; i++) {
		ErlNifBinary bin;
		size_t len = strlen(array.strings[i]);

		if (!enif_alloc_binary(len, &bin))
			goto on_error;

		memcpy(bin.data, array.strings[i], len);
		list = enif_make_list_cell(env, enif_make_binary(env, &bin), list);
	}

	git_strarray_free(&array);

	return list;

on_error:
	git_strarray_free(&array);
	return atoms.error;
}

ERL_NIF_TERM
geef_reference_lookup(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_repository *repo;
	geef_ref *res_ref;
	git_reference *ref;
	ErlNifBinary bin;
	ERL_NIF_TERM term_ref;
	char *name;
	int error;

	if (!enif_get_resource(env, argv[0], geef_repository_type, (void **) &repo))
		return enif_make_badarg(env);

	if (!enif_inspect_iolist_as_binary(env, argv[1], &bin))
		return enif_make_badarg(env);

	name = malloc(bin.size + 1);
	if (!name)
		return atoms.error;

	memcpy(name, bin.data, bin.size);
	name[bin.size] = '\0';

	error = git_reference_lookup(&ref, repo->repo, name);
	free(name);

	if (error < 0)
		return geef_error(env);

	res_ref = enif_alloc_resource(geef_ref_type, sizeof(geef_ref));
	res_ref->ref = ref;
	term_ref = enif_make_resource(env, res_ref);
	enif_release_resource(res_ref);

	return enif_make_tuple2(env, atoms.ok, term_ref);
}

ERL_NIF_TERM
geef_reference_resolve(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_ref *in_ref, *res_ref;
	ERL_NIF_TERM term_ref;
	git_reference *ref;

	if (!enif_get_resource(env, argv[0], geef_ref_type, (void **) &in_ref))
		return enif_make_badarg(env);

	if (git_reference_resolve(&ref, in_ref->ref) < 0)
		return geef_error(env);

	res_ref = enif_alloc_resource(geef_ref_type, sizeof(geef_ref));
	res_ref->ref = ref;
	term_ref = enif_make_resource(env, res_ref);
	enif_release_resource(res_ref);

	return enif_make_tuple2(env, atoms.ok, term_ref);
}

void geef_ref_free(ErlNifEnv *env, void *cd)
{
	geef_ref *ref = (geef_ref *) cd;
	git_reference_free(ref->ref);
}

struct list_data {
	ErlNifEnv *env;
	ERL_NIF_TERM list;
};

static int append_to_list(const char *name, void *payload)
{
	struct list_data *data = (struct list_data *) payload;
	ErlNifBinary bin;
	size_t len = strlen(name);

	if (!enif_alloc_binary(len, &bin))
		return -1;

	memcpy(bin.data, name, len);
	data->list = enif_make_list_cell(data->env, enif_make_binary(data->env, &bin), data->list);
	return 0;
}

ERL_NIF_TERM
geef_reference_glob(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	char *glob;
	int error;
	geef_repository *repo;
	ErlNifBinary bin;
	struct list_data data;

	if (!enif_get_resource(env, argv[0], geef_repository_type, (void **) &repo))
		return enif_make_badarg(env);

	if (!enif_inspect_binary(env, argv[1], &bin))
		return enif_make_badarg(env);

	glob = malloc(bin.size + 1);
	if (!glob)
		return atoms.error;

	memcpy(glob, bin.data, bin.size);
	glob[bin.size] = '\0';

	data.env = env;
	data.list = enif_make_list(env, 0);

	error = git_reference_foreach_glob(repo->repo, glob, GIT_REF_LISTALL, append_to_list, &data);
	free(glob);

	if (error < 0)
		return geef_error(env);

	return data.list;
}

ERL_NIF_TERM
geef_reference_target(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_ref *ref;
	ErlNifBinary bin;
	const git_oid *id;

	if (!enif_get_resource(env, argv[0], geef_ref_type, (void **) &ref))
		return enif_make_badarg(env);

	id = git_reference_target(ref->ref);

	if (geef_oid_bin(&bin, id) < 0)
		return atoms.error;

	return enif_make_binary(env, &bin);
}

ERL_NIF_TERM
geef_reference_to_id(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_repository *repo;
	ErlNifBinary bin;
	char *name;
	git_oid id;

	if (!enif_get_resource(env, argv[0], geef_repository_type, (void **) &repo))
		return enif_make_badarg(env);

	if (!enif_inspect_binary(env, argv[1], &bin))
		return enif_make_badarg(env);

	name = malloc(bin.size + 1);
	if (!name)
		return atoms.error;

	memcpy(name, bin.data, bin.size);
	name[bin.size] = '\0';

	if (git_reference_name_to_id(&id, repo->repo, name) < 0) {
		enif_release_binary(&bin);
		return geef_error(env);
	}

	if (geef_oid_bin(&bin, &id) < 0)
		return atoms.error;

	return enif_make_binary(env, &bin);
}

ERL_NIF_TERM
geef_reference_type(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_ref *ref;
	int type;
	ERL_NIF_TERM term_type;

	if (!enif_get_resource(env, argv[0], geef_ref_type, (void **) &ref))
		return enif_make_badarg(env);

	type = git_reference_type(ref->ref);

	switch (type) {
	case GIT_REF_OID:
		term_type = atoms.oid;
		break;
	case GIT_REF_SYMBOLIC:
		term_type = atoms.symbolic;
		break;
	default:
		term_type = atoms.error;
		break;
	}

	return term_type;
}
