defmodule Geef.Commit do
  alias Geef.Object
  alias Geef.Signature
  alias Geef.Oid
  import Object, only: :macros

  @type t :: Object[type: :commit]

  def lookup(repo, id) do
    case :geef_commit.lookup(repo, id) do
      {:ok, commit} ->
        {:ok, Object.from_erl commit}
      error ->
        error
    end
  end

  def tree_id(commit = Object[type: :commit]) do
    :geef_commit.tree_id(rebind(commit))
  end

  @type create(pid, Signature.t, Signature.t, iolist, Tree.t, Commit.t, [atom]) :: {:ok, Oid.t} | {:error, term}
  def create(repo, author = Signature[], committer = Signature[], message, tree, parents, opts // []) do
    :geef_commit.create(repo, author, committer, message, tree, parents, opts)
  end

end
