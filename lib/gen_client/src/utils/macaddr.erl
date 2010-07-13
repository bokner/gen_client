%%% -*- Erlang -*-
%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : macaddr.erl
%%% Author  : Michael Mullis <michael@mullistechnologies.com>
%%% @copyright 2008 Mullis Technologies Corporation
%%% @doc Cross platform MAC address determination.
%%%
%%% Inspired by the macaddr gem for ruby
%%% (<a href="http://rubyforge.org/projects/codeforpeople">http://rubyforge.org/projects/codeforpeople</a>)
%%%
%%% Tested on Linux (x86_64), Windows Vista, and MacOSX(10.5.4)
%%%
%%% Works for:  <br/>
%%% * /sbin/ifconfig  <br/>
%%% * /bin/ifconfig  <br/>
%%% * ifconfig  <br/>
%%% * ipconfig /all  <br/>
%%% <br/> <pre>
%%% To return the first MAC address on the system:
%%%   macaddr:address()
%%% <br/>
%%% To return an array of all MAC addresses:
%%%    macaddr:address_list
%%% </pre>
%%% @end

%%% Created : 22 Sep 2008 by Michael Mullis <michael@mullistechnologies.com>

%%% @end
%%%-------------------------------------------------------------------
-module(macaddr).
-author("michael@mullistechnologies.com").
-author("kevin@hypotheticalabs.com").

-define(IFCONFIG_CMDS, ["/sbin/ifconfig", "/bin/ifconfig", "ifconfig", "ipconfig /all"]).

-vsn("0.1.0").

-export([address/0, address/1, address_list/0]).
-export([mac_matcher/2]).

file_exists(FileName) ->
  case filelib:is_regular(FileName) of
    true ->
      true;
    %% Even if its not a regular file, it might still exist
    %% /dev/null exhibits this behavior
    false ->
      case filelib:last_modified(FileName) of
        0 ->
          false;
        _ ->
          true
      end
  end.

identify_null_file() ->
    case file_exists("/dev/null") of
        true -> "/dev/null";
        false -> "NUL"
    end.

%% Warning: do not export this because it allows arbitrary os command execution
get_interface_info(Cmd) ->
    %% @TODO os:type() may be more useful here than the original approach
    CmdString = Cmd ++ " 2>" ++ identify_null_file(),
    case os:cmd(CmdString) of
        [] -> "";
        {ok, Data} -> Data;
        Data -> Data
    end.

%%% @doc Exported for testability.   Internal use only.
-spec mac_matcher(Line::string(), Acc::[string()]) -> [string()].
mac_matcher(Line, Acc) ->
  MACRegex = " (?<FOO>([0-9A-F][0-9A-F][:\\-]){5}[0-9A-F][0-9A-F])([^:\\-0-9A-F]|$)",
  case re:run(Line, MACRegex, [caseless, {capture,['FOO']}]) of
    {match, [{Start, Length}]} ->
      MACAddress = string:strip(lists:sublist(Line, Start, Length+1)),
      {ok, StdMACAddress, _} =  regexp:gsub(MACAddress, "-", ":"),
      [StdMACAddress|Acc];
    _ ->
      Acc
  end.

%%% @doc Retrieve list of MAC addresses for machine
-spec(address_list() -> [string()]).
address_list() ->
    Lines = extract_interface_info(),
    extract_mac_addresses(Lines).

%%% @doc Extract MAC addresses from command results
extract_mac_addresses(CmdOutput) ->
  Candidates0 = lists:foldl(fun mac_matcher/2, [], CmdOutput),

  %% Length check to avoid some false hits from the regex because re module does not seem to support more complex regex to handle it
  Candidates = lists:reverse(lists:filter(fun(Elem) ->  (Elem /= "00:00:00:00:00:00" andalso
                                                         Elem /= "00-00-00-00-00-00" andalso
                                                         length(Elem) =< 17 ) end, Candidates0)),
  case length(Candidates) of
    0 -> throw({error, {no_mac_address_candidate, "No MAC Address"}});
    _ -> ok
  end,
  lists:usort(Candidates).  % remove duplicates

%%% @doc Retrieve interface info from operating system
extract_interface_info() ->
    execute_commands(?IFCONFIG_CMDS).

extract_interface_info(Interface) ->
    Cmds = [Cmd ++ " " ++ Interface || Cmd <- ?IFCONFIG_CMDS],
    execute_commands(Cmds).

execute_commands(Cmds) ->
    {ok, Results} = regexp:split(string:join([get_interface_info(Cmd) || Cmd <- Cmds], " "), "[\r\n]"),
    [R || R <- Results,
          R /= []].

%%% @doc Retrieve the first MAC addresses for machine
-spec(address() -> string()).
address() ->
    hd(address_list()).

%%% @doc Retrieve MAC address for specific interface
-spec(address(string()) -> string()).
address(Interface) ->
    Lines = extract_interface_info(Interface),
    extract_mac_addresses(Lines).
