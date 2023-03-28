-module(erlProject).
-export([computeNthPrime/4, ripNode/3, launchNode/1, start/1, connectNode/4, printTable/1, rpc/2]).

launchNode(Nickname)->
    PID=spawn(fun()->erlProject:start(Nickname) end),
    io:fwrite("~p : Created node as ~p ~n",[PID,Nickname]),
    PID.

connectNode(FirstNickname, FirstPID, SecondNickname, SecondPID) ->
    FirstPID ! {connect, SecondNickname, SecondPID},
    SecondPID ! {connect, FirstNickname, FirstPID},
    FirstPID ! {updateRT, SecondNickname, SecondPID, 1},
    SecondPID ! {updateRT, FirstNickname, FirstPID, 1},
    propagate_rt(FirstPID, SecondNickname, SecondPID, 1),
    propagate_rt(SecondPID, FirstNickname, FirstPID, 1).

propagate_rt(From, Target, TargetPID, Hops) ->
    From ! {propagateRT, Target, TargetPID, Hops},
    From ! {requestNeighbours, self()},
    receive
        Neighbours ->
            [Neighbour ! {propagateRT, Target, TargetPID, Hops + 1} || {_, Neighbour} <- Neighbours]
    end.

printTable(PID) ->
    PID ! {requestRoutingTable, self()},
    receive
        {routingTable, RTList} ->
            RTList1 = lists:map(fun({Target, _, Hops}) -> {Target, Hops} end, RTList),
            io:format("Routing table for node ~p:~n", [PID]),
            printTableHelper(RTList1)
    end.

printTableHelper([]) -> ok;
printTableHelper([{Target, Hops}|T]) ->
    io:format("  ~p - distance: ~p~n", [Target, Hops]),
    printTableHelper(T).


computeNthPrime(N, SenderNickname, DestinationNickname, Hops)->
    SenderNickname ! {computeNthPrime, N, SenderNickname, DestinationNickname, Hops}.


start(Nickname)->
    register(Nickname,self()),
    ripNode(Nickname,[],[]).

% Main loop for processing messages
ripNode(MyNickname, NList, RTList) ->
    receive
        % Print the routing table
        {printRoutingTable} ->
            io:fwrite("~p ~p : Routing Table ~p ~n", [self(), MyNickname, RTList]),
            ripNode(MyNickname, NList, RTList);
        % Compute the Nth prime number
        {computeNthPrime, N, SenderNickname, DestinationNickname, Hops} ->
            processMsgQuestion(N, SenderNickname, MyNickname, DestinationNickname, Hops, NList, RTList),
            ripNode(MyNickname, NList, RTList);
        % Receive the answer to the Nth prime computation
        {receiveAnswer, N, M, Destination, Sender, Hops} ->
            if MyNickname == Destination ->
                io:format("Answer received: Number=~p, Answer=~p, Sender=~p, Destination=~p, Hops=~p~n", [N, M, Sender, Destination, Hops]),
                ripNode(MyNickname, NList, RTList);
            true ->
                processMsgAnswer(N, M, Sender, Destination, Hops, NList, RTList),
                ripNode(MyNickname, NList, RTList)
            end;
        {requestNeighbours, Requester} ->
            Requester ! NList,
            ripNode(MyNickname, NList, RTList);
        {requestRoutingTable, Requester} ->
            Requester ! {routingTable, RTList},
            ripNode(MyNickname, NList, RTList);
        {propagateRT, NewNickname, NewPID, Hops} ->
            UpdatedRTList = update_routing_table(RTList, NewNickname, NewPID, Hops),
            lists:foreach(fun({Nickname, PID}) ->
                    PID ! {updateRT, NewNickname, NewPID, Hops + 1}
                end, NList),
            ripNode(MyNickname, NList, UpdatedRTList);
        % Update the routing table
        {updateRT, NewNickname, NewPID} ->
            UpdatedRTList = update_routing_table(RTList, NewNickname, NewPID, 1),
            ripNode(MyNickname, NList, UpdatedRTList);
        {From, {computeNthPrime, N, SenderNickname, DestinationNickname, Hops}} ->
            processMsgQuestion(N, SenderNickname, MyNickname, DestinationNickname, Hops, NList, RTList),
            From ! {self(), computeNthPrime(N)},
            ripNode(MyNickname, NList, RTList);
        % Connect to another node
        {connect, Nickname, PID} ->
            UpdatedRTList = update_routing_table(RTList, Nickname, PID, 1),
            io:fwrite("~p ~p : Connected to node ~p ~n", [self(), PID, Nickname]),
            ripNode(MyNickname, NList ++ [{Nickname, PID}], UpdatedRTList)
    end.
update_routing_table(RTList, Target, PID, Hops) ->
    case lists:keyfind(Target, 1, RTList) of
        {Target, _, OldHops} when Hops < OldHops ->
            lists:keyreplace(Target, 1, RTList, {Target, PID, Hops});
        {Target, _, _} ->
            RTList;
        false ->
            RTList ++ [{Target, PID, Hops}]
    end.

processMsgQuestion(N, SenderNickname, MyNickname, DestinationNickname, Hops, NList, RTList) when Hops >= 15 ->
    io:fwrite("~p ~p : Message to ~p is more than 15 hops ~n", [self(), SenderNickname, MyNickname]);
processMsgQuestion(N, SenderNickname, MyNickname, DestinationNickname, Hops, NList, RTList) ->
    if MyNickname == DestinationNickname ->
        Answer = computeNthPrime(N),
        {Neighbour, _} = lookup(RTList, SenderNickname),
        if Neighbour =/= notFound ->
            Neighbour ! {receiveAnswer, N, Answer, MyNickname, Hops},
            ok;
        true ->
            ok
        end;
    true ->
        {Neighbour, _} = lookup(RTList, DestinationNickname),
        if Neighbour =/= notFound ->
            Neighbour ! {computeNthPrime, N, SenderNickname, DestinationNickname, Hops + 1},
            ok;
        true ->
            ok
        end
    end.

processMsgAnswer(N, M, SenderNickname, MyNickname, Hops, NList, RTList) when Hops >= 15 ->
    io:format("~p ~p : Message from ~p is more than 15 hops ~n",[self(), SenderNickname, MyNickname]);
processMsgAnswer(N, M, SenderNickname, MyNickname, Hops, NList, RTList) ->
    io:format("Answer received: N=~p, M=~p, Sender=~p, Destination=~p, Hops=~p~n",[N, M, SenderNickname, MyNickname, Hops]);
processMsgAnswer(N, M, Sender, Destination, Hops, NList, RTList) ->
    {Neighbour, _} = lookup(RTList, Destination),
    if Neighbour =/= notFound ->
            Neighbour ! {receiveAnswer, N, M, Destination, Sender, Hops},
            ok;
        true ->
            io:format("Error: Destination not found in the routing table")
    end.

rpc(PID, MSG)->
    PID!{self(),MSG},
    receive
        {PID,Reply}->
            io:format("Received answer: ~p~n", [Reply]),
            Reply;
        {From,Reply}->
            io:format("Received answer from ~p: ~p~n", [From, Reply]),
            Reply
    end.


lookup([], _) ->
    notFound;
lookup([{Sender, Neighbour, Hops} | Tail], Sender) ->
    {Neighbour, Hops};
lookup([{_, _, _} | Tail], Sender) ->
    lookup(Tail, Sender).

computeNthPrime(N) ->
    findPrime(N, 2).

% Find the nth Prime by checking for primes starting from the current number
findPrime(0, CurrentNum) ->
    CurrentNum - 1;
findPrime(N, CurrentNum) ->-module(erlProject).
-export([start/1, launchNode/1, connectNode/4, printTable/1, computeNthPrime/3]).

%
% Node startup
%
launchNode(Nickname)->
    PID=spawn(fun()->erlProject:start(Nickname) end),
    io:fwrite("~p : Created node as ~p ~n",[PID,Nickname]),
    PID.
%
% Node connections
%
connectNode(FirstNickname, FirstPID, SecondNickname, SecondPID) ->
    FirstPID ! {connect, SecondNickname, SecondPID},
    SecondPID ! {connect, FirstNickname, FirstPID}.
%
% Display routing table
%
printTable(PID) ->
  PID ! {routing_table}.

computeNthPrime(N, SourceNickname, DestinationNickname) ->
  SourceNickname ! {computeNthPrime, N, DestinationNickname, SourceNickname, 1}.

start(Nickname)->
    register(Nickname,self()),
    ripNode(Nickname,[],[]).

ripNode(NodeName, NeighborList, RT) ->
  receive
  % Message to display routing Table -> display content
    {routing_table} ->
      io:fwrite("~p ~p's Routing Table : ~p ~n", [self(), NodeName, RT]),
      ripNode(NodeName, NeighborList, RT);

  % here connects a new node
    {connect, Nickname, Pid} ->
      io:fwrite("~p ~p is connecting to ~p ~n", [self(), NodeName, Nickname]),
      ripNode(NodeName, NeighborList ++ [{Nickname, Pid}], RT);

  % Hops is over 15 for searching answer
    {computeNthPrime, _N, DestinationNickname, _SenderNickname, Hops} when Hops > 15 ->
      io:fwrite("~p~p : cannot send message to ~p  because is over 15 hops ~n", [self(), NodeName, DestinationNickname]),
      ripNode(NodeName, NeighborList, RT);

  % Hops is over 15 for receiving answer
    {receiveAnswer, _N, _M, DestinationNickname, _SenderNickname, Hops} when Hops > 15 ->
      io:fwrite("~p~p - Message to ~p is over 15 hops ~n", [self(), NodeName, DestinationNickname]),
      ripNode(NodeName, NeighborList, RT);

  % Hop limit ok for computing answer
  % ComputeNthPrime or forward message and increase hops
    {computeNthPrime, N, DestinationNickname, SenderNickname, Hops} ->
      case DestinationNickname of
        % Computation task is for current node -> Compute and send response to sender
        % sender becomes destination of message
        NodeName ->
          io:fwrite("~p (~p) Computing prime ~p for ~p by ~p at ~p ~n", [self(), NodeName, N, SenderNickname, DestinationNickname, Hops]),
          NthPrime = computeNthPrime(N),
          MessageReply = {receiveAnswer, N, NthPrime, SenderNickname, DestinationNickname, Hops + 1},
          NewLocation = SenderNickname;

        % Computation is for some other node -> forward
        _ ->
          MessageReply = {computeNthPrime, N, DestinationNickname, SenderNickname, Hops + 1},
          NewLocation = DestinationNickname
      end,
      % Send updated messages to new destinations
      case sendMsg(NewLocation, MessageReply, NeighborList, RT) of
        {error, unreachable} ->
          io:fwrite("~p~p is unreachable to ~p  for ~p ~n", [self(), NodeName, NewLocation, MessageReply]);
        {ok, _} ->
          io:fwrite("~p~p is sending ~p a message ~p ~n", [self(), NodeName, NewLocation, MessageReply])
      end,
      ripNode(NodeName, NeighborList, RT);

  % Hop limit is okay to receive answer
  % Show result or forward to correct node
    {receiveAnswer, N, M, DestinationNickname, SenderNickname, Hops} ->
      case DestinationNickname of
        % Message is for current node -> display the information
        NodeName ->
          io:fwrite("~p~p's ~p Prime is ~p  ~n", [self(), NodeName, N, M]);

        % Message is for someone else -> forward
        _ ->
          Message = {receiveAnswer, N, M, DestinationNickname, SenderNickname, Hops + 1},
          sendMsg(DestinationNickname, Message, NeighborList, RT)
      end,
      ripNode(NodeName, NeighborList, RT);

  % Timer reminder to send updates to neighbors
  % Create routing information using neighbors and RT and send to neighbors
    {send_updates} ->
      ModifiedRoutes = createNodeMessage(NodeName, NeighborList, RT),
      updateNeighbor(NodeName, NeighborList, ModifiedRoutes),
      ripNode(NodeName, NeighborList, RT);

  % Routing updates from neighbors -> check and update routes
    {route_updates, NeighborNickname, NeighborRT} ->
      UpdatedRT = update_routes(NodeName, NeighborList, RT, NeighborNickname, NeighborRT),
      ripNode(NodeName, NeighborList, UpdatedRT)
  end.

computeNthPrime(N) ->
    findPrime(N, 2).

% Find the nth Prime by checking for primes starting from the current number
findPrime(0, CurrentNum) ->
    CurrentNum - 1;
findPrime(N, CurrentNum) ->
    case isPrime(CurrentNum) of
        true ->
            findPrime(N - 1, CurrentNum + 1);
        false ->
            findPrime(N, CurrentNum + 1)
    end.

isPrime(1) ->
    false;
isPrime(2) ->
    true;
isPrime(N) ->
    isPrime(N, 2).

% Check if a num is prime by dividing with all numbers starting from 2
isPrime(N, N) ->
    true;
isPrime(N, CheckNum) ->
    case N rem CheckNum of
        0 ->
            false;
        _ ->
            isPrime(N, CheckNum + 1)
    end.
    case isPrime(CurrentNum) of
        true ->
            findPrime(N - 1, CurrentNum + 1);
        false ->
            findPrime(N, CurrentNum + 1)
    end.

isPrime(1) ->
    false;
isPrime(2) ->
    true;
isPrime(N) ->
    isPrime(N, 2).

% Check if a num is prime by dividing with all numbers starting from 2
isPrime(N, N) ->
    true;
isPrime(N, CheckNum) ->
    case N rem CheckNum of
        0 ->
            false;
        _ ->
            isPrime(N, CheckNum + 1)
    end.