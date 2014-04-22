require Record

defmodule Geef.Index do
  use Geef
  alias Geef.Index.Entry

  defmacrop to_erl(entry) do
    quote do
      set_elem(unquote(entry), 0, :geef_index_entry)
    end
  end

  defmacrop from_erl(entry) do
    quote do
      set_elem(unquote(entry), 0, Geef.Index.Entry)
    end
  end

  defp maybe_entry({:ok, entry}), do: {:ok, from_erl(entry)}
  defp maybe_entry(error = {:error, _}), do: error

  @spec new :: {:ok, pid()} | :ignore | {:error, term()}
  def new() do
    :geef_index.new()
  end

  @spec add(pid(), Entry.t()) :: :ok | {:error, term()}
  def add(pid, entry) do
    :geef_index.add(pid, to_erl(entry))
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

defrecord Geef.Index.Entry, Record.extract(:geef_index_entry, from: "src/geef_records.hrl")
