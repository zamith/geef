require Record

defmodule Geef.Iterator do
  alias Geef.Iterator
  alias Geef.Reference

  record = Record.extract(:geef_iterator, from: "src/geef_records.hrl")
  keys   = :lists.map(&elem(&1, 0), record)
  vals   = :lists.map(&{&1, [], nil}, keys)
  pairs  = :lists.zip(keys, vals)

  defstruct keys

  def from_record({:geef_iterator, unquote_splicing(vals)}) do
    %Geef.Iterator{unquote_splicing(pairs)}
  end

  def to_record(%Geef.Iterator{unquote_splicing(pairs)}) do
    {:geef_iterator, unquote_splicing(vals)}
  end

  def stream!(%Iterator{type: :ref, repo: repo, regexp: regexp}) do
    iter =
      case :geef_ref.iterator(repo, regexp) do
        {:ok, iter} ->
          Iterator.from_erl iter
        {:error, error} ->
          raise Geef.IteratorError, message: error
      end
    &do_stream(iter, &1, &2)
  end

  defp do_stream(iter = %Iterator{type: :ref}, acc, fun) do
    case :geef_ref.next(to_record(iter)) do
      {:ok, ref} ->
        do_stream(iter, fun.(Reference.from_erl(ref), acc), fun)
      {:error, :iterover} ->
        acc
      {:error, error} ->
        raise Geef.IteratorError, message: error
    end
  end
end

defmodule Geef.IteratorError do
  defexception [message: nil]
end
