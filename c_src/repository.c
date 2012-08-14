#include "repository.h"
#include "geef.h"
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

ERL_NIF_TERM
geef_repository_path(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    geef_repository *repo;

    if (!enif_get_resource(env, argv[0], geef_repository_type, (void **) &repo))
        return enif_make_badarg(env);

    return enif_make_string(env, git_repository_path(repo->repo), ERL_NIF_LATIN1);
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

    return enif_make_tuple2(env, geef_atoms.ok, term_odb);
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
	return geef_atoms.true;

    return geef_atoms.false;
}
