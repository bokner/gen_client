-module(inet_mdns).

-include_lib("kernel/src/inet_dns.hrl").

-export([open/2,start/0]).
-export([stop/1,receiver/1]).
-export([subscribe/2,unsubscribe/2,getsubs/1]).

% gets a timestamp in ms from the epoch
get_timestamp() ->
    {Mega,Sec,Micro} = erlang:now(),
    (Mega*1000000+Sec)*1000000+Micro.

open(Addr,Port) ->
   {ok,S} = gen_udp:open(Port,[{reuseaddr,true},{ip,Addr},{multicast_ttl,4},{broadcast,true}, binary]),
   inet:setopts(S,[{add_membership,{Addr,{0,0,0,0}}}]),
   S.

close(S) -> gen_udp:close(S).

start() ->
   S=open({224,0,0,251},5353),
   Pid=spawn(?MODULE,receiver,[dict:new()]),
   gen_udp:controlling_process(S,Pid),
   {S,Pid}.

stop({S,Pid}) ->
   close(S),
   Pid ! stop.

subscribe(Domain,Pid) ->
    Pid ! {sub,Domain}.

unsubscribe(Domain,Pid) ->
    Pid ! {unsub,Domain}.

getsubs(Pid) ->
    Pid ! {getsubs,self()},
    receive
        {ok,Sub} ->
            {ok,Sub}
    end.

receiver(Sub) ->
  receive
      {udp, _Socket, _IP, _InPortNo, Packet} ->
          NewSub = process_dnsrec(Sub,inet_dns:decode(Packet)),
          receiver(NewSub);
      {sub,Domain} ->
          receiver(dict:store(Domain,dict:new(),Sub));
      {unsub,Domain} ->
          receiver(dict:erase(Domain, Sub));
      {getsubs,Pid} ->
          Pid ! {ok,Sub},
          receiver(Sub);
      stop ->
           true;
       AnythingElse ->
           io:format("RECEIVED: ~p~n",[AnythingElse]),
           receiver(Sub)
   end.

% process the dns resource records list
process_dnsrec(_Sub,{error,E}) ->
    io:format("Error: ~p~n", [E]);
process_dnsrec(Sub,{ok,#dns_rec{anlist=Responses}}) ->
    process_dnsrec1(Sub,Responses).

% test to see if a dns_rr.domain is subscribed to
is_subscribed(_,[]) -> false;
is_subscribed(Dom,[S|Rest]) ->
    case lists:suffix(S,Dom) of
        true ->
            {ok,S};
        false ->
            is_subscribed(Dom,Rest)
    end.

% process the list of resource records one at a time
process_dnsrec1(Sub,[]) -> Sub;
process_dnsrec1(Sub,[Response|Rest]) ->
  Dom = Response#dns_rr.domain,
  Key = {Response#dns_rr.domain,Response#dns_rr.type,Response#dns_rr.class},
  case is_subscribed(Dom,dict:fetch_keys(Sub)) of
      {ok,SD} ->
          {ok,Value} = dict:find(SD,Sub),
          % if the ttl == Zero then we forget about the details for that server
          case Response#dns_rr.ttl == 0 of
              true ->
                  NewSub = dict:store(SD,dict:new(),Sub),
                  process_dnsrec1(NewSub,[]);
              false ->
                  % update the dns_rr to the current timestamp
                  NewRR = Response#dns_rr{tm=get_timestamp()},
                  NewValue = dict:store(Key,NewRR,Value),
                  NewSub = dict:store(SD,NewValue,Sub),
                  process_dnsrec1(NewSub,Rest)
          end;
     false ->
          process_dnsrec1(Sub,Rest)
  end.
        
            