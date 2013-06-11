defmodule Mix.Tasks.Compile.Rebar do
  @shortdoc "Runs `rebar compile` so we build the NIF"

  def run(_) do
    Mix.shell.info System.cmd("rebar compile")
  end
end

defmodule Geef.Mixfile do
  use Mix.Project

  def project do
    [ app: :geef,
      version: "0.0.1",
      compilers: [:rebar, :elixir, :app],
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    []
  end

  # Returns the list of dependencies in the format:
  # { :foobar, "0.1", git: "https://github.com/elixir-lang/foobar.git" }
  defp deps do
    []
  end
end
