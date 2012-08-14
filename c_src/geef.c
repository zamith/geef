#include "erl_nif.h"
#include "repository.h"
#include "geef.h"
#include <stdio.h>
#include <string.h>
#include <git2.h>

ErlNifResourceType *geef_repository_type;
ErlNifResourceType *geef_odb_type;

geef_atoms_t geef_atoms;

static ERL_NIF_TERM
geef_hex_to_raw(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  char sha[MAXBUFLEN];
  (void)memset(&sha, '\0', sizeof(sha));

  if (enif_get_string(env, argv[0], sha, sizeof(sha), ERL_NIF_LATIN1) < 1)
      return enif_make_badarg(env);

  git_oid oid;
  git_oid_fromstr(&oid, sha);

  ErlNifBinary ibin;
  enif_alloc_binary(20, &ibin);
  memcpy(ibin.data, (&oid)->id, 20);

  return enif_make_binary(env, &ibin);
}

static int load(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info)
{
    geef_repository_type = enif_open_resource_type(env, NULL,
        "repository_type", geef_repository_free, ERL_NIF_RT_CREATE, NULL);

    if (geef_repository_type == NULL)
	return -1;

    geef_odb_type = enif_open_resource_type(env, NULL,
        "odb_type", geef_odb_free, ERL_NIF_RT_CREATE, NULL);

    if (geef_odb_type == NULL)
	return -1;

    geef_atoms.ok = enif_make_atom(env, "ok");
    geef_atoms.error = enif_make_atom(env, "error");
    geef_atoms.true = enif_make_atom(env, "true");
    geef_atoms.false = enif_make_atom(env, "false");
    geef_atoms.repository = enif_make_atom(env, "repository");

    return 0;
}

ERL_NIF_TERM
geef_error(ErlNifEnv *env)
{
    const git_error *error;

    error = giterr_last();
    if (error) {
	return enif_make_tuple2(env, geef_atoms.error,
				enif_make_string(env, error->message, ERL_NIF_LATIN1));
    }

    return enif_make_tuple2(env, geef_atoms.error,
			    enif_make_string(env, "No message specified", ERL_NIF_LATIN1));
}

static ErlNifFunc geef_funcs[] =
{
  {"hex_to_raw", 1, geef_hex_to_raw},
  {"repository_open", 1, geef_repository_open},
  {"repository_get_path", 1, geef_repository_path},
  {"repository_get_odb", 1, geef_repository_odb},
  {"odb_object_exists", 2, geef_odb_exists},
};

ERL_NIF_INIT(geef, geef_funcs, load, NULL, NULL, NULL)
