-module(erlProject).
-export([computeNthPrime/4, ripNode/3, launchNode/1, start/1, connectNode/4, printTable/1,rpc/2]).
% Create a new node with a given Nickname
launchNode(Nickname)->
    PID=spawn(fun()->erlProject:start(Nickname) end),
    io:fwrite("~p : Created node as ~p ~n",[PID,Nickname]),
    PID.

% Connect two nodes by their Nicknames and PIDs
connectNode(FirstNickname, FirstPID, SecondNickname, SecondPID) ->
    FirstNickname ! {connect, SecondNickname, SecondPID},
    SecondNickname ! {connect, FirstNickname, FirstPID}.
% Print the routing table of a given node, including the distance to each node
printTable(PID) ->
    PID ! {requestRoutingTable, self()},
    receive
        {routingTable, RTList} ->
            DistanceList = lists:map(
                fun({Target, _, Hops}) -> {Target, Hops} end, RTList),
            io:fwrite("Routing table for node ~p: ~n~p~n", [PID, DistanceList])
    end.

% Initiate the computation of the Nth prime number
computeNthPrime(N, SenderNickname, DestinationNickname, Hops) ->
    if Hops >= 15 ->
        io:fwrite("~p ~p : Message to ~p is more than 15 hops ~n", [self(), SenderNickname, DestinationNickname]),
        ok;
    true ->
        io:fwrite("~p ~p : Computing prime number ~p, sending to ~p~n", [self(), SenderNickname, N, DestinationNickname]),
        SenderNickname ! {computeNthPrime, N, SenderNickname, DestinationNickname, Hops}
    end.


% Start the node with a given Nickname
start(Nickname)->
    timer:send_interval(5000, {send_updates}),
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
            lists:foreach(fun({_, PID}) ->
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
            ripNode(MyNickname, NList ++ [{Nickname, PID}], UpdatedRTList);
        {send_updates} ->
            ModifiedRoutes = createNodeMessage(MyNickname, NList, RTList),
            updateNeighbor(MyNickname, NList, ModifiedRoutes),
            ripNode(MyNickname, NList, RTList);
          % Routing updates from neighbors -> check and update routes
        {route_updates, NeighborNickname, NeighborRT} ->
            UpdatedRT = update_routes(MyNickname, NList, RTList, NeighborNickname, NeighborRT),
            ripNode(MyNickname, NList, UpdatedRT)
    end.

% Given the node's nickname, a list of its neighbors, and its current routing table, create a message to send to each neighbor with the routes to each neighbor plus the current routing table
createNodeMessage(MyNickname, NeighborList, RT) ->
  RoutesToNeighbors = lists:map(fun({NeighborNickname, _Pid}) -> {NeighborNickname, MyNickname, 1} end, NeighborList),
  RoutesToNeighbors ++ RT.

% Given the node's nickname, a list of its neighbors, and its current routing table, update the routing table based on the neighbor's information
updateNeighbor(MyNickname, [{NeighborNickname, _NeighborPid}| NeighborList], Routes) ->
  NeighborNickname ! {route_updates, MyNickname, Routes},
  updateNeighbor(MyNickname, NeighborList, Routes);
updateNeighbor(_, [], _) ->
  ok.

% Given the node's nickname, a list of its neighbors, its current routing table, the nickname of the neighbor, and a list of routes to check, update the routing table based on the route information
update_routes(_, _NeighborList, RT, _NeighborNickname, []) ->
  RT;
update_routes(MyNickname, NeighborList, RT, NeighborNickname, [{MyNickname, _ConnectionNode, _Distance}| RemainingRoutes]) ->
  update_routes(MyNickname, NeighborList, RT, NeighborNickname, RemainingRoutes);
update_routes(MyNickname, NeighborList, RT, NeighborNickname, [{DestinationNickname, _ConnectionNode, Distance}| RemainingRoutes]) ->
  % Check if the destination is a neighbor. If not, it's possible there are changes to the routing table. If it is, ignore it.
  case lists:filter(fun(I) -> I == DestinationNickname end, NeighborList) of
    % Not a neighbor
    [] ->
      % Check if there is a route in the routing table. If there is, compare distances and update if the new distance is shorter. If there isn't, just append the new route to the routing table.
      case lists:keyfind(DestinationNickname, 1, RT) of
        {DestinationNickname, _Connection, OldDistance} ->
          case OldDistance > Distance + 1 of
            false ->
              update_routes(MyNickname, NeighborList, RT, NeighborNickname, RemainingRoutes);
            true ->
              UpdatedRoutingTable = lists:keyreplace(DestinationNickname, 1, RT, {DestinationNickname, NeighborNickname, Distance + 1}),
              update_routes(MyNickname, NeighborList, UpdatedRoutingTable, NeighborNickname, RemainingRoutes)
          end;
        false ->
          UpdatedRoutingTable = RT ++ [{DestinationNickname, NeighborNickname, Distance + 1}],
          update_routes(MyNickname, NeighborList, UpdatedRoutingTable, NeighborNickname, RemainingRoutes)
      end;
    % A neighbor
    _ ->
      update_routes(MyNickname, NeighborList, RT, NeighborNickname, RemainingRoutes)
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
processMsgQuestion(_, SenderNickname, MyNickname, _, Hops, _, _) when Hops >= 15 ->
    io:fwrite("~p ~p : Message to ~p is more than 15 hops ~n", [self(), SenderNickname, MyNickname]),
ok;
processMsgQuestion(N, SenderNickname, MyNickname, DestinationNickname, Hops, _, RTList) ->
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

processMsgAnswer(N, M, Sender, Destination, Hops, _, RTList) ->
    if Hops >= 15 ->
        io:fwrite("~p ~p : Message from ~p is more than 15 hops ~n",[self(), Sender, Destination]);
    Destination == self() ->
        ok;
    true ->
        {Neighbour, _} = lookup(RTList, Destination),
        if Neighbour =/= notFound ->
            Neighbour ! {receiveAnswer, N, M, Destination, Sender, Hops},
            ok;
        true ->
            io:format("Error: Destination not found in the routing table")
        end
    end.

rpc(PID, MSG) ->
    PID ! {self(), MSG},
    receive
        {_, {computeNthPrime, _, SenderNickname, DestinationNickname, Hops}} when Hops >= 15 ->
            io:fwrite("~p ~p : Message to ~p is more than 15 hops~n", [self(), SenderNickname, DestinationNickname]),
            rpc(PID, MSG);
        {From, Reply} ->
            io:format("Received answer from ~p~n", [From]),
            Reply
    end.

lookup([], _) ->
    notFound;
lookup([{Sender, Neighbour, Hops} | _], Sender) ->
    {Neighbour, Hops};
lookup([{_, _, _} | Tail], Sender) ->
    lookup(Tail, Sender).
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