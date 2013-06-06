defmodule Geef.Commit do
  alias Geef.Object

  def lookup(repo, id) do
    case :geef_commit.lookup(repo, id) do
      {:ok, commit} ->
        {:ok, Object.new commit}
      error ->
        error
    end
  end

  def tree_id(commit = Object[type: :commit]) do
    :geef_commit.tree_id(set_elem(commit, 0, :geef_object))
  end

end
