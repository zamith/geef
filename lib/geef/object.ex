defrecord Geef.Object, Record.extract(:geef_object, from: "src/geef_records.hrl") do

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
