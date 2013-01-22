#include "erl_nif.h"
#include "repository.h"
#include "reference.h"
#include "oid.h"
#include "geef.h"
#include <stdio.h>
#include <string.h>
#include <git2.h>

ErlNifResourceType *geef_repository_type;
ErlNifResourceType *geef_odb_type;
ErlNifResourceType *geef_ref_type;

geef_atoms atoms;

static int load(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info)
{
	git_threads_init();

	geef_repository_type = enif_open_resource_type(env, NULL,
						     "repository_type", geef_repository_free, ERL_NIF_RT_CREATE, NULL);

	if (geef_repository_type == NULL)
		return -1;

	geef_odb_type = enif_open_resource_type(env, NULL,
					      "odb_type", geef_odb_free, ERL_NIF_RT_CREATE, NULL);

	if (geef_odb_type == NULL)
		return -1;

	geef_ref_type = enif_open_resource_type(env, NULL,
					      "ref_type", geef_ref_free, ERL_NIF_RT_CREATE, NULL);

	if (geef_ref_type == NULL)
		return -1;

	atoms.ok = enif_make_atom(env, "ok");
	atoms.error = enif_make_atom(env, "error");
	atoms.true = enif_make_atom(env, "true");
	atoms.false = enif_make_atom(env, "false");
	atoms.repository = enif_make_atom(env, "repository");

	return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{
	git_threads_shutdown();
}

ERL_NIF_TERM
geef_error(ErlNifEnv *env)
{
	const git_error *error;

	error = giterr_last();
	if (error) {
		return enif_make_tuple2(env, atoms.error,
					enif_make_string(env, error->message, ERL_NIF_LATIN1));
	}

	return enif_make_tuple2(env, atoms.error,
				enif_make_string(env, "No message specified", ERL_NIF_LATIN1));
}

static ErlNifFunc geef_funcs[] =
{
	{"repository_init", 2, geef_repository_init},
	{"repository_open", 1, geef_repository_open},
	{"repository_is_bare", 1, geef_repository_is_bare},
	{"repository_get_path", 1, geef_repository_path},
	{"repository_get_workdir", 1, geef_repository_workdir},
	{"repository_get_odb", 1, geef_repository_odb},
	{"odb_object_exists", 2, geef_odb_exists},
	{"reference_list", 1, geef_reference_list},
	{"reference_to_id", 2, geef_reference_to_id},
	{"reference_glob", 2, geef_reference_glob},
	{"reference_lookup", 2, geef_reference_lookup},
	{"reference_resolve", 1, geef_reference_resolve},
	{"reference_id", 1, geef_reference_id},
	{"oid_fmt", 1, geef_oid_fmt},
	{"oid_parse", 1, geef_oid_parse},
};

ERL_NIF_INIT(geef, geef_funcs, load, NULL, NULL, unload)
