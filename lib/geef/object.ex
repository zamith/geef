defrecord Geef.Object, Record.extract(:geef_object, from: "src/geef_records.hrl") do

  @doc false
  defmacro rebind(obj) do
    quote do
      set_elem(unquote(obj), 0, :geef_object)
    end
  end

  def lookup(repo, id) do
    case :geef_obj.lookup(repo, id) do
      {:ok, obj} ->
        {:ok, Geef.Object.new obj}
      error ->
        error
    end
  end

  def lookup!(repo, id) do
    case lookup(repo, id) do
      {:ok, obj} ->
        obj
      {:error, err} ->
        raise err
    end
  end

end
