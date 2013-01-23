#ifndef GEEF_H
#define GEEF_H

#include "erl_nif.h"

ERL_NIF_TERM geef_error(ErlNifEnv *env);

typedef struct {
	ERL_NIF_TERM ok;
	ERL_NIF_TERM error;
	ERL_NIF_TERM true;
	ERL_NIF_TERM false;
	ERL_NIF_TERM repository;
	ERL_NIF_TERM oid;
	ERL_NIF_TERM symbolic;
} geef_atoms;

extern geef_atoms atoms;

#endif
