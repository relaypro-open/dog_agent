#!/usr/bin/env elixir
#
defmodule ExpandIpsets do
  def reduce_map_list(input) do
    Enum.reduce(input, %{}, fn m, acc ->
      Map.merge(acc, m, fn
        _k, v1, v2 when is_list(v1) ->
          :lists.reverse([v2 | :lists.reverse(v1)])
        _k, v1, v2 -> [v1, v2]
      end)
    end)
  end

  def networkify_ip(ip,output) do
    case String.contains?(ip,"/") do
      true ->
        case output do
          true ->
            to_string :io_lib.format("-d ~s \\1",[ip])
            #-m set --match-set src \\2 \\1
          false ->
            to_string :io_lib.format("-s ~s \\1",[ip])
        end
      false ->
        case output do
          true ->
            to_string :io_lib.format("-d ~s/32 \\1",[ip])
          false ->
            to_string :io_lib.format("-s ~s/32 \\1",[ip])
        end
    end
  end

  def networkify_ip2(ip,output) do
    case String.contains?(ip,"/") do
      true ->
        case output do
          true ->
            to_string :io_lib.format("-d ~s",[ip])
          false ->
            to_string :io_lib.format("-s ~s",[ip])
        end
      false ->
        case output do
          true ->
            to_string :io_lib.format("-d ~s/32",[ip])
          false ->
            to_string :io_lib.format("-s ~s/32",[ip])
        end
    end
  end


  #FROM: -A OUTPUT -p tcp -m set --match-set isw_proxy_pro_g_v4 src -m tcp --sport 22 -m state --state RELATED,ESTABLISHED -m comment --comment isw_proxy_pro -j ACCEP
  #BAD: -A OUTPUT -p tcp -d 10.36.68.179/32 -m tcp --sport 22 -m state --state RELATED,ESTABLISHED -m comment --comment isw_proxy_  
  #TO: -A OUTPUT -d 10.36.68.179/32 -p tcp -m tcp --sport 22 -m state --state RELATED,ESTABLISHED -m comment --comment isw_proxy_
  #
  #
  #FROM: -A OUTPUT -m set --match-set testing_qa_g_v4 src -m state --state RELATED,ESTABLISHED -m comment --comment testing_qa -j ACCEPT 
  #BAD: -A OUTPUT -m set --match-set testing_qa_g_v4 src -m state --state RELATED,ESTABLISHED -m comment --comment testing_qa -j A  
  #TO: -A OUTPUT -d 50.17.96.152/32 -m state --state RELATED,ESTABLISHED -m comment --comment testing_qa -j ACCEPT
  def expand do
    {ipsets,0} = System.cmd("sudo",["/sbin/ipset","save"])
    lines = String.split(ipsets,"\n")
    lines = Enum.filter(lines, fn line -> Regex.match?(~r/^add/,line) end)
    input = Enum.map(lines, fn(line) -> %{Enum.at(String.split(line," "),1) => Enum.at(String.split(line," "),2)} end)
    #IO.puts(Kernel.inspect(input))
    ipset_map = reduce_map_list(input)
    #IO.puts(Kernel.inspect(ipset_map))

    {:ok, ipsets} = File.read("/etc/dog/ip4tables_ipsets.txt") 
    ruleset = String.split(ipsets,"\n")

    expanded_ruleset = Enum.map(ruleset, fn(rule) -> 
      #IO.puts(Kernel.inspect(rule))
      capture = Regex.run(~r/--match-set ([^ ]+)/, rule)
      case capture do
        nil ->
          to_string :io_lib.format("~s",[rule])
        _ ->
          capture = Enum.at(capture,1)
          #IO.puts(Kernel.inspect(capture))
          replace_ips = Map.get(ipset_map,capture)
          #IO.puts(Kernel.inspect(replace_ips))
          output = String.starts_with?(rule,"-A OUTPUT")
          case replace_ips do
            nil ->
              []
            _ ->
              case is_list(replace_ips) do
                true ->
                  replace_ips = case output do
                    true ->  
                      replace_ips = Enum.reverse(Enum.sort(replace_ips))
                    false ->
                      replace_ips = Enum.sort(replace_ips)
                  end
                  Enum.map(replace_ips, fn(ip) -> 
                    if Regex.match?(~r/-p /,rule) do
                      ip = networkify_ip(ip,output)
                      Regex.replace(~r/(-p .*) -m set --match-set ([^ ]+) src/, rule, ip) 
                    else
                      ip = networkify_ip2(ip,output)
                      Regex.replace(~r/-m set --match-set ([^ ]+) src/, rule, ip) 
                    end 
                  end)
                false ->
                  if Regex.match?(~r/-p /,rule) do
                    replace_ips = networkify_ip(replace_ips,output)
                    Regex.replace(~r/(-p .*) -m set --match-set ([^ ]+) src/, rule, replace_ips) 
                  else
                    replace_ips = networkify_ip2(replace_ips,output)
                    Regex.replace(~r/-m set --match-set ([^ ]+) src/, rule, replace_ips) 
                  end
              end
          end
      end
    end)
    :io.format("~s",[Enum.join(List.flatten(expanded_ruleset),"\n")])
  end
end

ExpandIpsets.expand()
