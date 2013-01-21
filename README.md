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

    $ git clone git://github.com/schacon/geef.git
    $ cd geef
    $ make

The API looks basically like this:

    Repo = geef:repository(<<".">>).
    Workdir = Repo:workdir().
    Odb = geef:repository_odb(Repo).
    Exists = Odb:exists(HexSha).


CONTRIBUTING
==============

Fork schacon/geef on GitHub, make it awesomer (preferably in a branch named
for the topic), send a pull request.


AUTHORS 
==============

Scott Chacon <schacon@gmail.com>


LICENSE
==============

MIT.

