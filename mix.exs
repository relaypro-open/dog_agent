# File: mix.exs
# This file was generated from rebar.config
# Using rebar3_elixir (https://github.com/G-Corp/rebar3_elixir)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Dog.Mixfile do
  use Mix.Project

  def project do
    [
      app: :dog,
      version: "1.3.0",
      elixir: "~> 1.2",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps(),
      aliases: aliases(),
      releases:
      [
        dog: [
          steps: [:assemble, &Bakeware.assemble/1]
        ]
      ]
    ]
  end


  def application do
    [
      applications: [:sasl, :asn1, :public_key, :ssl, :lager, :turtle, :hackney, :amqp_client, :rabbit_common, :jsx, :base16, :quickrand, :uuid, :erldocker, :bbmustache, :jsn, :erldocker],
      env: [],
      mod: {:dog_app, []},
      erl_opts: [parse_transform: "lager_transform"]
    ]
  end

  defp deps do
    [
      {:bakeware, "~> 0.2.3"},
      {:lager, ~r/.*/, git: "https://github.com/erlang-lager/lager.git", tag: "3.9.2", override: true},
      {:jsx, ~r/.*/, git: "https://github.com/talentdeficit/jsx.git", tag: "v2.8.3", override: true},
      {:hackney, ~r/.*/, git: "https://github.com/benoitc/hackney.git", branch: "master", override: true},
      {:base16, git: "https://github.com/goj/base16.git", tag: "1.0.0"},
      {:uuid, "2.0.0", hex: :uuid_erl, override: true},
      {:erldocker, ~r/.*/, git: "https://github.com/Phonebooth/erldocker.git", branch: "master"},
      {:turtle, git: "https://github.com/relaypro-open/turtle.git", branch: "feature/runtime_creation"},
      {:jsn, "2.1.4"},
      {:bbmustache, "1.11.0"},
      {:parse_trans, "3.3.1", override: true}
    ]
  end

  defp aliases do
    [compile: &compile_with_hooks/1]
  end

  defp compile_with_hooks(args) do
    pre_compile_hooks()
    result = Mix.Task.run("compile", args)
    post_compile_hooks()
    result
  end

  defp pre_compile_hooks() do
    run_hook_cmd [
    ]
  end

  defp post_compile_hooks() do
    run_hook_cmd [
    ]
  end

  defp run_hook_cmd(commands) do
    {_, os} = :os.type
    for command <- commands, do: (fn
      ({regex, cmd}) ->
      if Regex.match?(Regex.compile!(regex), Atom.to_string(os)) do
        Mix.Shell.cmd cmd, [], fn(x) -> Mix.Shell.IO.info(trim(x)) end
      end
      (cmd) ->
      Mix.Shell.cmd cmd, [], fn(x) -> Mix.Shell.IO.info(trim(x)) end
    end).(command)
  end

  defp trim(x) do
    if Version.compare(System.version, "1.5.0") == :lt do
      Kernel.apply(String, :strip, [x])
    else
      Kernel.apply(String, :trim, [x])
    end
  end
end
