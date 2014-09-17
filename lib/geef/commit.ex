defmodule Geef.Commit do
  use Geef

  @type t :: Object[type: :commit]

  @spec lookup(pid, Oid.t) :: Commit.t
  def lookup(repo, id) do
    Object.lookup(repo, id, :commit)
  end

  @spec tree_id(Object.t) :: Oid.t
  def tree_id(%Object{type: :commit, handle: handle}) do
    :geef_nif.commit_tree_id(handle)
  end

  @spec tree(t) :: {:ok, Tree.t} | {:error, any}
  def tree(%Object{type: :commit, handle: handle}) do
    case :geef_nif.commit_tree(handle) do
      {:ok, id, handle} ->
        {:ok, %Object{type: :tree, id: id, handle: handle}}
      error = {:error, _} ->
        error
    end
  end

  @spec tree!(t) :: Tree.t
  def tree!(commit = %Object{type: :commit}), do: tree(commit) |> Geef.assert_ok

  @spec create(pid, Signature.t, Signature.t, iolist, Oid.t, [Oid.t], [:proplists.property()]) :: {:ok, Oid.t} | {:error, term}
  def create(repo, author = %Signature{}, committer = %Signature{}, message, tree, parents, opts \\ []) do
    :geef_commit.create(repo, Signature.to_record(author), Signature.to_record(committer), message, tree, parents, opts)
  end

end
