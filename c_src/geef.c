#include "erl_nif.h"
#include <stdio.h>
#include <string.h>
#include <git2.h>

#define MAXBUFLEN       1024

static ErlNifResourceType *geef_repository_type;

typedef struct {
    git_repository *repo;
} geef_repository;

static struct {
    ERL_NIF_TERM ok;
    ERL_NIF_TERM error;
    ERL_NIF_TERM repository;
} geef_atoms;

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

static ERL_NIF_TERM
geef_object_exists(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  char path[MAXBUFLEN];
  char sha[MAXBUFLEN];
  (void)memset(&path, '\0', sizeof(path));
  (void)memset(&sha, '\0', sizeof(sha));

  if (enif_get_string(env, argv[0], path, sizeof(path), ERL_NIF_LATIN1) < 1)
      return enif_make_badarg(env);
  if (enif_get_string(env, argv[1], sha, sizeof(sha), ERL_NIF_LATIN1) < 1)
      return enif_make_badarg(env);

  git_odb *odb;
  git_odb_open(&odb, path);

  git_oid oid;
  git_oid_fromstr(&oid, sha);

  int exists = git_odb_exists(odb, &oid);
  if(exists == 1) {
    return enif_make_atom(env, "true");
  }
  return enif_make_atom(env, "false");
}

static void repository_free(ErlNifEnv *env, void *cd)
{
    geef_repository *grepo = (geef_repository *) cd;
    git_repository_free(grepo->repo);
}

static int load(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info)
{
    geef_repository_type = enif_open_resource_type(env, NULL,
        "repository_type", repository_free, ERL_NIF_RT_CREATE, NULL);

    if (geef_repository_type == NULL)
	return -1;

    geef_atoms.ok = enif_make_atom(env, "ok");
    geef_atoms.error = enif_make_atom(env, "error");
    geef_atoms.repository = enif_make_atom(env, "repository");

    return 0;
}

static ERL_NIF_TERM
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

static ERL_NIF_TERM
geef_repository_open(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  char path[MAXBUFLEN];
  git_repository *repo;
  geef_repository *res_repo;
  ERL_NIF_TERM term_repo;

  if (enif_get_string(env, argv[0], path, sizeof(path), ERL_NIF_LATIN1) < 1)
      return enif_make_badarg(env);

  if (git_repository_open(&repo, path) < 0)
      return geef_error(env);

  res_repo = enif_alloc_resource(geef_repository_type, sizeof(geef_repository));
  res_repo->repo = repo;
  term_repo = enif_make_resource(env, res_repo);
  enif_release_resource(res_repo);

  return enif_make_tuple2(env, geef_atoms.ok, term_repo);
}

static ERL_NIF_TERM
geef_repository_path(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    geef_repository *repo;

    if (!enif_get_resource(env, argv[0], geef_repository_type, (void **) &repo))
        return enif_make_badarg(env);

    return enif_make_string(env, git_repository_path(repo->repo), ERL_NIF_LATIN1);
}

static ErlNifFunc geef_funcs[] =
{
  {"hex_to_raw", 1, geef_hex_to_raw},
  {"object_exists", 2, geef_object_exists},
  {"repository_open", 1, geef_repository_open},
  {"repository_get_path", 1, geef_repository_path},
};

ERL_NIF_INIT(geef, geef_funcs, load, NULL, NULL, NULL)
