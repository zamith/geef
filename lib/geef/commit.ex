defmodule Geef.Commit do
  alias Geef.Object
  import :macros, Object

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

end
