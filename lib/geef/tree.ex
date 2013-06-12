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

  defp maybe_entry({:ok, entry}), do: {:ok, TreeEntry.new entry}
  defp maybe_entry(error = {:error, _}), do: error

  def get(tree, path), do: :geef_tree.get(rebind(tree), path) |> maybe_entry
  def nth(tree, pos), do: :geef_tree.nth(rebind(tree), pos) |> maybe_entry

end

defimpl Access, for: Geef.Tree do
  alias Geef.Tree

  def access(tree, key) when is_number(key) do
    case Tree.nth(tree, key) do
      {:ok, entry} -> entry
      {:error, _} -> nil
    end
  end

  def access(tree, key) do
    case Tree.get(tree, key) do
      {:ok, entry} -> entry
      {:error, _} -> nil
    end
  end

end
