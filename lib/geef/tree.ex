require Record

defmodule Geef.TreeEntry do
  defstruct mode: nil, type: nil, id: nil, name: nil
end

defmodule Geef.Tree do
  alias Geef.Object
  alias Geef.TreeEntry

  @type t :: Object[type: :tree]

  def lookup(repo, id) do
    Object.lookup(repo, id, :tree)
  end

  defp maybe_entry({:ok, mode, type, id, name}) do
    {:ok, %TreeEntry{mode: mode, type: type, id: id, name: name}}
  end

  defp maybe_entry(error = {:error, _}), do: error

  def get(%Object{type: :tree, handle: handle}, path) do
    :geef_nif.tree_bypath(handle, path) |> maybe_entry
  end

  def nth(%Object{type: :tree, handle: handle}, nth) do
    :geef_nif.tree_nth(handle, nth) |> maybe_entry
  end

  def count(%Object{type: :tree, handle: handle}) do
    :geef_nif.tree_count(handle)
  end
end

defmodule Geef.TreeError do
  defexception reason: nil

  def message(%Geef.TreeError{reason: reason}) do
    "tree error #{inspect(reason)}"
  end
end

defimpl Enumerable, for: Geef.Object do
  alias Geef.Object
  alias Geef.Tree
  alias Geef.TreeError

  def count(tree = %Object{type: :tree}) do
    {:ok, Tree.count(tree)}
  end

  def member?(tree = %Object{type: :tree}, key) do
    case Tree.get(tree, key) do
      {:ok, _} -> {:ok, true}
      _ -> {:ok, false}
    end
  end

  def slice(tree = %Object{type: :tree}) do
    count = Tree.count(tree)

    slicer = fn {start, length} ->
      Enum.map(start..(start + length - 1), fn idx ->
        case Tree.nth(tree, idx) do
          {:ok, entry} -> entry
          # Handle errors gracefully, although they shouldn't occur in a valid range
          {:error, _} -> nil
        end
      end)
      # Remove any potential nil values if errors were encountered
      |> Enum.reject(&is_nil/1)
    end

    {:ok, count, slicer}
  end

  def reduce(tree = %Object{type: :tree}, acc, fun) do
    reduce(tree, 0, Tree.count(tree), acc, fun)
  end

  # We're done when the index is equal to the number of entries
  defp reduce(_, idx, idx, {:cont, acc}, _), do: {:done, acc}

  # Call the user-passed function and recurse with the next index
  defp reduce(tree, idx, count, {:cont, acc}, fun) do
    case Tree.nth(tree, idx) do
      {:ok, entry} ->
        reduce(tree, idx + 1, count, fun.(entry, acc), fun)

      {:error, error} ->
        raise TreeError, reason: error
    end
  end

  # Almost default Enumerable implementation
  defp reduce(_, _, _, {:halt, acc}, _), do: {:halted, acc}

  defp reduce(tree, idx, count, {:suspend, acc}, fun) do
    {:suspended, acc, &reduce(tree, idx, count, &1, fun)}
  end
end
