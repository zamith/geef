defrecord Geef.TreeEntry, Record.extract(:geef_tree_entry, from: "src/geef_records.hrl")

defrecord Geef.Tree, Record.extract(:geef_object, from: "src/geef_records.hrl") do
  alias Geef.Object
  alias Geef.TreeEntry

  import :macros, Geef.Object

  def lookup(repo, id) do
    case :geef_tree.lookup(repo, id) do
      {:ok, obj} ->
        {:ok, new obj}
      error ->
        error
    end
  end

  def get(tree, path) do
    case :geef_tree.get(rebind(tree), path) do
      {:ok, entry} ->
        {:ok, TreeEntry.new entry}
      error ->
        error
    end
  end

end

defimpl Access, for: Geef.Tree do

  def access(tree, key) do
    case Geef.Tree.get(tree, key) do
      {:ok, entry} -> entry
      {:error, _} -> nil
    end
  end

end
