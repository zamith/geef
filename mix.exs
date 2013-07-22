defmodule Mix.Tasks.Compile.Nif do
  @shortdoc "Compile a NIF"

  def run(_) do
    config = Keyword.fetch!(Mix.project, :nif)

    file = Keyword.fetch!(config, :file)
    paths = Keyword.fetch!(config, :paths)
    flags = Keyword.fetch!(config, :flags)
    exts = Keyword.fetch!(config, :exts)

    # Create the directory (e.g. "priv/")
    file |> Path.dirname |> File.mkdir_p!

    # Figure out whether we should compile the NIF. Compare the source
    # files with the target binary. If the target is newer, recompile.
    to_compile = Mix.Utils.extract_files(paths, exts)
    stale = Mix.Utils.extract_stale(to_compile, [file])
    case stale do
      [] ->
        :noop
      _ ->
        filesarg = Enum.join(to_compile, " ")
        cmd = "gcc -shared -fpic -o #{file} #{filesarg} #{flags}"
        IO.puts System.cmd(cmd)
    end
  end
end

defmodule Geef.Mixfile do
  use Mix.Project

  def project do
    [ app: :geef,
      version: "0.0.1",
      compilers: [:nif, :elixir, :app],
      nif: nif,
      deps: deps ]
  end

  def nif do
    [ paths: ["c_src"],
      exts: [:c],
      file: "priv/geef.so",
      flags: "-lgit2" ]
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
