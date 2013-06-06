defmodule Geef.Tree do
  alias Geef.Object

  def lookup(repo, id) do
    case :geef_tree.lookup(repo, id) do
      {:ok, obj} ->
        {:ok, Object.new obj}
      error ->
        error
    end
  end

  def get(tree = Object[type: :tree], path) do
    :geef_tree.get(set_elem(tree, 0, :geef_object), path)
  end

end
