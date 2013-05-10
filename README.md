Geef (Git NIF)
================

Geef is a simple Erlang NIF that exposes some of the libgit2 library functions
to Erlang.

INSTALLING AND RUNNING
========================

First you need to install libgit2:

    $ git clone git://github.com/libgit2/libgit2.git
    $ cd libgit2
    $ make
    $ make install

Now, if you have Erlang OTP_R13B04 installed, you can compile this NIF to run
in it.

    $ git clone git://github.com/carlosmn/geef.git
    $ cd geef
    $ make

The API looks basically like this:

    {ok, Repo} = geef_repo:open(".").
    Workdir = geef_repo:workdir(Repo).
    {ok, Blob} = geef_blob:lookup(Repo, geef_oid:parse("abcde...")).

CONTRIBUTING
==============

Fork schacon/geef on GitHub, make it awesomer (preferably in a branch named
for the topic), send a pull request.


AUTHORS 
==============

Scott Chacon <schacon@gmail.com>
Carlos Martín Nieto <cmn@dwim.me>


LICENSE
==============

MIT.
