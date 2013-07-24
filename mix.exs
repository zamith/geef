defmodule Mix.Tasks.Compile.Nif do
  @shortdoc "Compile a NIF"

  def run(_) do
    config = Keyword.fetch!(Mix.project, :nif)

    file = Keyword.fetch!(config, :file)
    paths = Keyword.fetch!(config, :paths)
    flags = Keyword.get(config, :flags, [])
    exts = Keyword.get(config, :exts, [:c, :cpp])
    compiler =
      Keyword.get(config, :compilers, ["cc", "gcc", "clang"])
      |> find_compiler

    # Create the directory (e.g. "priv/")
    file |> Path.dirname |> File.mkdir_p!

    # Figure out whether we should compile the NIF. Compare the source
    # files with the target binary. If the target is newer, recompile.
    to_compile = Mix.Utils.extract_files(paths, exts)

    case Mix.Utils.extract_stale(to_compile, [file]) do
      [] ->
        :noop
      _ ->
        Mix.shell.info("* Compiling #{file}")
        args = ["-shared", "-fpic", "-o", file, to_compile, flags] |> List.flatten
        port = Port.open({:spawn_executable, compiler},
                         [:stream, :binary, :use_stdio, :stderr_to_stdout, :hide,
                          :exit_status, {:args, args}])
        if do_cmd(port) != 0 do
          raise Mix.Error, message: "Error compiling #{file}"
        end
    end
  end

  defp find_compiler([compiler | tail]) do
    case System.find_executable(compiler) do
      nil ->
        find_compiler(tail)
      path ->
        path
    end
  end

  defp do_cmd(port) do
    receive do
      {^port, {:data, data}} ->
        IO.write(data)
        do_cmd(port)
      {^port, {:exit_status, status}} ->
        status
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
