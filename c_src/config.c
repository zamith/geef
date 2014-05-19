#include "config.h"
#include "geef.h"
#include <git2.h>

void geef_config_free(ErlNifEnv *env, void *cd)
{
	geef_config *cfg = (geef_config *) cd;
	git_config_free(cfg->config);
}

static ERL_NIF_TERM extract(geef_config **cfg, ErlNifBinary *bin, ErlNifEnv *env, const ERL_NIF_TERM argv[])
{
	if (!enif_get_resource(env, argv[0], geef_config_type, (void **) cfg))
		return enif_make_badarg(env);

	if (!enif_inspect_iolist_as_binary(env, argv[1], bin))
		return enif_make_badarg(env);

	if (!geef_terminate_binary(bin))
		return geef_oom(env);

	return atoms.ok;
}

ERL_NIF_TERM
geef_config_set_bool(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_config *cfg;
	ErlNifBinary bin;
	int error, val;
	ERL_NIF_TERM ret;
	
	ret = extract(&cfg, &bin, env, argv);
	if (ret != atoms.ok)
		return ret;

	val = !enif_compare(argv[2], atoms.true);

	error = git_config_set_bool(cfg->config, (char *) bin.data, val);
	enif_release_binary(&bin);

	if (error < 0)
		return geef_error(env);

	return atoms.ok;
}

ERL_NIF_TERM
geef_config_get_bool(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_config *cfg;
	ErlNifBinary bin;
	int error, val;
	ERL_NIF_TERM ret;
	
	ret = extract(&cfg, &bin, env, argv);
	if (ret != atoms.ok)
		return ret;

	error = git_config_get_bool(&val, cfg->config, (char *) bin.data);
	enif_release_binary(&bin);

	if (error < 0)
		return geef_error(env);

	ret = val ? atoms.true : atoms.false;
	return enif_make_tuple2(env, atoms.ok, ret);
}


ERL_NIF_TERM
geef_config_set_int(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_config *cfg;
	ErlNifBinary bin;
	int64_t val;
	int error;
	ERL_NIF_TERM ret;
	
	ret = extract(&cfg, &bin, env, argv);
	if (ret != atoms.ok)
		return ret;

	if (!enif_get_int64(env, argv[2], &val))
		return enif_make_badarg(env);

	error = git_config_set_int64(cfg->config, (char *) bin.data, val);
	enif_release_binary(&bin);

	if (error < 0)
		return geef_error(env);

	return atoms.ok;
}
