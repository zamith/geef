defrecord Geef.Oid, Record.extract(:geef_oid, from: "src/geef_records.hrl") do
  alias Geef.Oid

  @doc false
  defmacro to_erl(oid) do
    quote do
      set_elem(unquote(oid), 0, :geef_oid)
    end
  end

  @doc false
  defmacro from_erl(oid) do
    quote do
      set_elem(unquote(oid), 0, Geef.Oid)
    end
  end

  defp maybe_oid({:ok, oid}), do: from_erl(oid)
  defp maybe_oid(error = {:error, _}), do: error

  @spec parse(iolist) :: t
  def parse(str) do
    :geef_oid.parse(str)
  end

  @spec hex(t) :: String.t
  def hex(oid = Oid[]) do
    :geef_oid.hex(to_erl(oid))
  end

end
