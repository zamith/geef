defrecord Geef.TreeEntry, Record.extract(:geef_tree_entry, from: "src/geef_records.hrl")

defmodule Geef.Tree do
  alias Geef.Object
  alias Geef.TreeEntry

  def lookup(repo, id) do
    case :geef_tree.lookup(repo, id) do
      {:ok, obj} ->
        {:ok, Object.new obj}
      error ->
        error
    end
  end

  def get(tree = Object[type: :tree], path) do
    case :geef_tree.get(set_elem(tree, 0, :geef_object), path) do
      {:ok, entry} ->
        {:ok, TreeEntry.new entry}
      error ->
        error
    end
  end

end
