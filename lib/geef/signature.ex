defrecord Geef.Signature, Record.extract(:geef_signature, from: "src/geef_records.hrl") do

  def new(name, email), do: :geef_sig.new(name, email) |> maybe_sig
  def new(name, email, time), do: :geef_sig.new(name, email, time) |> maybe_sig

  defp maybe_sig({ok, sig}), do: {ok, Geef.Signature.new sig}
  defp maybe_sig(error), do: error

end
