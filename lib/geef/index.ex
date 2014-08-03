require Record

defmodule Geef.Index.Entry do
  record = Record.extract(:geef_index_entry, from: "src/geef_records.hrl")
  keys   = :lists.map(&elem(&1, 0), record)
  vals   = :lists.map(&{&1, [], nil}, keys)
  pairs  = :lists.zip(keys, vals)

  defaults = :lists.map(fn(_) -> :undefined end, keys)

  defstruct :lists.zip(keys, defaults)

  def from_record({:geef_index_entry, unquote_splicing(vals)}) do
    %Geef.Index.Entry{unquote_splicing(pairs)}
  end

  def to_record(e = %Geef.Index.Entry{unquote_splicing(pairs)}) do
    {:geef_index_entry, unquote_splicing(vals)}
  end

end

defmodule Geef.Index do
  use Geef
  alias Geef.Index.Entry

  defp maybe_entry({:ok, entry}), do: {:ok, Entry.from_record(entry)}
  defp maybe_entry(error = {:error, _}), do: error

  @spec new :: {:ok, pid()} | :ignore | {:error, term()}
  def new() do
    :geef_index.new()
  end

  @spec add(pid(), Entry.t()) :: :ok | {:error, term()}
  def add(pid, entry) do
    :geef_index.add(pid, Entry.to_record(entry))
  end

  @spec clear(pid()) :: :ok
  def clear(pid) do
    :geef_index.clear(pid)
  end

  @spec count(pid()) :: non_neg_integer()
  def count(pid) do
    :geef_index.count(pid)
  end

  @spec get(pid(), iolist(), non_neg_integer()) :: {:ok, Entry.t()} | {:error, term()}
  def get(pid, path, stage) do
    :geef_index.get(pid, path, stage) |> maybe_entry
  end

  @spec nth(pid(), non_neg_integer()) :: {:ok, Entry.t()} | {:error, term()}
  def nth(pid, nth) do
    :geef_index.nth(pid, nth) |> maybe_entry
  end

  @spec write(pid()) :: {:error, term()} | :ok
  def write(pid) do
    :geef_index.write(pid)
  end

  def write_tree(pid, repo) do
    :geef_index.write_tree(pid, repo)
  end

  @spec write!(pid()) :: :ok
  def write!(pid) do
    case write(pid) do
      :ok -> :ok
      {:error, error} -> raise error
    end
  end

  @spec stop(pid()) :: :ok
  def stop(pid) do
    :geef_index.stop(pid)
  end

end
