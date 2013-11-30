defmodule Geef.Commit do
  use Geef
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

  @spec tree_id(Object.t) :: Oid.t
  def tree_id(commit = Object[type: :commit]) do
    :geef_commit.tree_id(rebind(commit))
  end

  @spec tree(t) :: {:ok, Tree.t} | {:error, any}
  def tree(commit = Object[type: :commit]) do
    case :geef_commit.tree(rebind(commit)) do
      {:ok, tree} ->
        {:ok, Object.from_erl(tree)}
      error = {:error, _} ->
        error
    end
  end

  @spec tree!(t) :: Tree.t
  def tree!(commit = Object[type: :commit]), do: tree(commit) |> Geef.assert_ok

  @spec create(pid, Signature.t, Signature.t, iolist, Oid.t, [Oid.t], [:proplists.property()]) :: {:ok, Oid.t} | {:error, term}
  def create(repo, author = Signature[], committer = Signature[], message, tree, parents, opts // []) do
    :geef_commit.create(repo, Signature.to_erl(author), Signature.to_erl(committer), message, tree, parents, opts)
  end

end
