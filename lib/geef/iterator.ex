defrecord Geef.Iterator, Record.extract(:geef_iterator, from: "src/geef_records.hrl") do
  alias Geef.Iterator
  alias Geef.Reference

  @doc false
  defmacro rebind(obj) do
    quote do
      set_elem(unquote(obj), 0, :geef_iterator)
    end
  end


  def stream(iter = Iterator[]) do
    &stream(iter, &1, &2)
  end

  def stream(iter = Iterator[type: ref], acc, fun) do
    case :geef_ref.next(rebind(iter)) do
      {:ok, ref} ->
        stream(iter, fun.(Reference.new(ref), acc), fun)
      {:error, :iterover} ->
        acc
      {:error, error} ->
        raise Geef.IteratorError, message: error
    end
  end
end

defexception Geef.IteratorError, [message: nil]
