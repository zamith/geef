defrecord Geef.Signature, Record.extract(:geef_signature, from: "src/geef_records.hrl") do

  def now(name, email), do: :geef_sig.now(name, email) |> from_erl

  def default(repo), do: :geef_sig.default(repo) |> maybe_sig

  defp maybe_sig({:ok, sig}), do: from_erl(sig)
  defp maybe_sig(error = {:error, _}), do: error

  defp from_erl(sig), do: set_elem(sig, 0, Geef.Signature)

  @spec to_erl(t) :: :geef_sig.signature
  def to_erl(sig), do: set_elem(sig, 0, :geef_signature)

end
