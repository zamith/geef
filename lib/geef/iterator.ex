defrecord Geef.Iterator, Record.extract(:geef_iterator, from: "src/geef_records.hrl") do
  alias Geef.Iterator
  alias Geef.Reference

  @doc false
  defmacro rebind(obj) do
    quote do
      set_elem(unquote(obj), 0, :geef_iterator)
    end
  end

  @spec from_erl(term()) :: t
  def from_erl(iterator) do
    set_elem(iterator, 0, Geef.Iterator)
  end

  def stream!(Iterator[type: :ref, repo: repo, regexp: regexp]) do
    iter =
      case :geef_ref.iterator(repo, regexp) do
        {:ok, iter} ->
          Iterator.from_erl iter
        {:error, error} ->
          raise Geef.IteratorError, message: error
      end
    &do_stream(iter, &1, &2)
  end

  defp do_stream(iter = Iterator[type: :ref], acc, fun) do
    case :geef_ref.next(rebind(iter)) do
      {:ok, ref} ->
        do_stream(iter, fun.(Reference.from_erl(ref), acc), fun)
      {:error, :iterover} ->
        acc
      {:error, error} ->
        raise Geef.IteratorError, message: error
    end
  end
end

defexception Geef.IteratorError, [message: nil]
