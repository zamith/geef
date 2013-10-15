#include "geef.h"
#include "repository.h"
#include <string.h>
#include <git2.h>

static int geef_string_to_bin(ErlNifBinary *bin, const char *str)
{
	size_t len;

	len = strlen(str);
	if (!enif_alloc_binary(len, bin))
		return -1;

	memcpy(bin->data, str, len);
	return 0;
}

ERL_NIF_TERM
geef_signature_default(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	git_signature *sig;
	geef_repository *repo;
	ErlNifBinary name, email;

	if (!enif_get_resource(env, argv[0], geef_repository_type, (void **) &repo))
		return enif_make_badarg(env);

	memset(&name, 0, sizeof(ErlNifBinary));
	memset(&email, 0, sizeof(ErlNifBinary));

	if (git_signature_default(&sig, repo->repo) < 0)
		return geef_error(env);

	if (geef_string_to_bin(&name, sig->name) < 0)
		goto oom;

	if (geef_string_to_bin(&email, sig->email) < 0)
		goto oom;

	return enif_make_tuple5(env, atoms.ok,
	         enif_make_binary(env, &name), enif_make_binary(env, &email),
		 enif_make_ulong(env, sig->when.time), enif_make_uint(env, sig->when.offset));
oom:
	git_signature_free(sig);
	enif_release_binary(&name);
	enif_release_binary(&email);

	return geef_oom(env);
}

ERL_NIF_TERM
geef_signature_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary name, email;
	git_signature *sig;
	size_t len;
	int error;
	unsigned int at;

	if (!enif_inspect_iolist_as_binary(env, argv[0], &name))
		return enif_make_badarg(env);

	if (!enif_inspect_iolist_as_binary(env, argv[1], &email))
		return enif_make_badarg(env);

	if (argc == 3 && !enif_get_uint(env, argv[2], &at))
		return enif_make_badarg(env);

	if (!geef_terminate_binary(&name))
		return atoms.error;

	if (!geef_terminate_binary(&email)) {
		enif_release_binary(&name);
		return atoms.error;
	}

	if (argc == 3)
		error = git_signature_now(&sig, (char *)name.data, (char *)email.data);
	else
		error = git_signature_new(&sig, (char *)name.data, (char *)email.data, at, 0);

	if (error < 0)
		return geef_error(env);

	len = strlen(sig->name);
	if (!enif_realloc_binary(&name, len))
		goto on_error;

	memcpy(name.data, sig->name, len);

	len = strlen(sig->email);
	if (!enif_realloc_binary(&email, len))
		goto on_error;

	memcpy(email.data, sig->email, len);
	git_signature_free(sig);

	if (argc == 3)
		return enif_make_tuple3(env, atoms.ok, enif_make_binary(env, &name), enif_make_binary(env, &email));

	return enif_make_tuple5(env, atoms.ok,
	         enif_make_binary(env, &name), enif_make_binary(env, &email),
		 enif_make_ulong(env, sig->when.time), enif_make_uint(env, sig->when.offset));

on_error:
		git_signature_free(sig);
		enif_release_binary(&name);
		enif_release_binary(&email);

		return atoms.error;
}
