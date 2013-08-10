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

  def count(tree), do: :geef_tree.count(rebind(tree))

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

defexception Geef.TreeError, reason: nil do
  def message(Geef.TreeError[reason: reason]) do
    "tree error #{inspect reason}"
  end
end

defimpl Enumerable, for: Geef.Tree do
  alias Geef.Tree
  alias Geef.TreeError

  def count(tree) do
    Tree.count(tree)
  end

  def member?(tree, key) do
    case Tree.get(tree, key) do
      {:ok, _} -> true
      _ -> false
    end
  end

  def reduce(tree, acc, fun) do
    reduce(tree, 0, Tree.count(tree), acc, fun)
  end

  # We're done when the index is equal to the number of entries
  defp reduce(_, idx, idx, acc, _), do: acc

  # Call the user-passed function and recurse with the next index
  defp reduce(tree, idx, count, acc, fun) do
    case Tree.nth(tree, idx) do
      {:ok, entry} ->
        reduce(tree, idx + 1, count, fun.(entry, acc), fun)
      {:error, error} ->
        raise TreeError, reason: error
    end
  end

end
