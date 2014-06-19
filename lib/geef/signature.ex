require Record

defmodule Geef.Signature do
  defstruct Record.extract(:geef_signature, from: "src/geef_records.hrl")

  def now(name, email), do: :geef_sig.now(name, email) |> from_record

  def default(repo), do: :geef_sig.default(repo) |> maybe_sig

  defp maybe_sig({:ok, sig}), do: from_record(sig)
  defp maybe_sig(error = {:error, _}), do: error

  def from_record({:geef_signature}) do
    %Geef.Signature{}
  end

end
