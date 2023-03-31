% Author Mohsin Tahir
% License = This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 
% International License. To view a copy of this license, 
% visit http://creativecommons.org/licenses/by-nc-sa/4.0/ or send a letter to Creative Commons, 
% PO Box 1866, Mountain View, CA 94042, USA.
% Date 21/03/2023
-module(erlProject).
-export([computeNthPrime/4, ripNode/3, launchNode/1, start/1, connectNode/4, printTable/1]).
% Create a new node with a given Nickname
launchNode(Nickname)->
    PID=spawn(fun()->erlProject:start(Nickname) end),
    io:fwrite("~p : Created node as ~p ~n",[PID,Nickname]),
    PID.

% Connect two nodes by their Nicknames and PIDs
connectNode(FirstNickname, FirstPID, SecondNickname, SecondPID) ->
    FirstNickname ! {connect, SecondNickname, SecondPID},
    SecondNickname ! {connect, FirstNickname, FirstPID}.
printTable(PID) ->
    PID ! {routingTable}.

% Initiate the computation of the Nth prime number
computeNthPrime(N, SenderNickname, DestinationNickname, Hops) ->
    if Hops >= 15 ->
        io:fwrite("~p ~p : Message to ~p is more than 15 hops ~n", [self(), SenderNickname, DestinationNickname]),
        ok;
    true ->
        io:fwrite("~p ~p : Computing prime number ~p, sending to ~p~n", [self(), SenderNickname, N, DestinationNickname]),
        Result = rpc(SenderNickname, {computeNthPrime, N, SenderNickname, DestinationNickname, Hops}),
        P = computeNthPrime(N),
         io:format("Computed Nth prime result: ~p~n", [P])
        % case Result of
        %     {computeNthPrimeResult, Result} ->
        %         io:format("Computed Nth prime result: ~p~n", [Result]);
        %     _ ->
        %         ok
        % end
    end.

% Start the node with a given Nickname
start(Nickname)->
    timer:send_interval(5000, {sendUpdates}),
    register(Nickname,self()),
    ripNode(Nickname,[],[]).
% Main loop for processing messages
ripNode(MyNickname, NList, RTList) ->
    receive
        % Print the routing table
        {routingTable} ->
            io:fwrite("~p ~p : Routing Table ~p ~n", [self(), MyNickname, RTList]),
            ripNode(MyNickname, NList, RTList);
         % Hops is over 15 for searching answer
        {computeNthPrime, _N, DestinationNickname, _SenderNickname, Hops} when Hops > 15 ->
        io:fwrite("~p~p - Message to ~p is over 15 hops ~n", [self(), MyNickname, DestinationNickname]),
        ripNode(MyNickname, NList, RTList);

  % Hops is over 15 for receiving answer
        {receiveAnswer, _N, _M, DestinationNickname, _SenderNickname, Hops} when Hops > 15 ->
        io:fwrite("~p~p - Message to ~p is over 15 hops ~n", [self(), MyNickname, DestinationNickname]),
        ripNode(MyNickname, NList, RTList);
    % Compute the Nth prime number
    {computeNthPrime, N, SenderNickname, DestinationNickname, Hops} ->
        case DestinationNickname of
            % Computation task is for current node -> Compute and send response to sender
            % sender becomes destination of message
            MyNickname ->
            processMsgQuestion(N, SenderNickname, MyNickname, DestinationNickname, Hops, NList, RTList);
            % Computation is for some other node -> forward
            _ ->
            {computeNthPrime, N, DestinationNickname, SenderNickname, Hops + 1}
        end,
        ripNode(MyNickname, NList, RTList);
        {computeNthPrimeResult, M} ->
            io:format("Computed Nth prime result: ~p~n", [M]),
            ripNode(MyNickname, NList, RTList);
        % Receive the answer to the Nth prime computation
        {receiveAnswer, N, M, Destination, Sender, Hops} ->
            case Destination of
                % Message is for current node -> display the information
                MyNickname ->
                processMsgAnswer(N, Sender, MyNickname, Destination, Hops, NList, RTList);
                % Message is for someone else -> forward
                _ ->
                {receiveAnswer, N, M, Destination, Sender, Hops + 1}
            end;
        {From, {computeNthPrime, N, SenderNickname, DestinationNickname, Hops}} ->
            processMsgQuestion(N, SenderNickname, MyNickname, DestinationNickname, Hops, NList, RTList),
            From ! {self(), computeNthPrime(N)},
            ripNode(MyNickname, NList, RTList);
        % Connect to another node
        {connect, Nickname, PID} ->
            % UpdatedRTList = update_routing_table(RTList, Nickname, PID, 1),
            io:fwrite("~p ~p : Connected to node ~p ~n", [self(), PID, Nickname]),
            ripNode(MyNickname, NList ++ [{Nickname, PID}], RTList);
        {sendUpdates} ->
            ModifiedRoutes = createNodeMessage(MyNickname, NList, RTList),
            updateNeighbor(MyNickname, NList, ModifiedRoutes),
            ripNode(MyNickname, NList, RTList);
          % Routing updates from neighbors -> check and update routes
        {routeUpdates, NeighborNickname, NeighborRT} ->
            UpdatedRT = update_routes(MyNickname, NList, RTList, NeighborNickname, NeighborRT),
            ripNode(MyNickname, NList, UpdatedRT)
    end.

% Given the node's nickname, a list of its neighbors, and its current routing table, create a message to send to each neighbor with the routes to each neighbor plus the current routing table
createNodeMessage(MyNickname, NeighborList, RT) ->
  RoutesToNeighbors = lists:map(fun({NeighborNickname, _Pid}) -> {NeighborNickname, MyNickname, 1} end, NeighborList),
  RoutesToNeighbors ++ RT.

% Given the node's nickname, a list of its neighbors, and its current routing table, update the routing table based on the neighbor's information
updateNeighbor(MyNickname, [{NeighborNickname, _NeighborPid}| NeighborList], Routes) ->
  NeighborNickname ! {routeUpdates, MyNickname, Routes},
  updateNeighbor(MyNickname, NeighborList, Routes);
updateNeighbor(_, [], _) ->
  ok.

% Given the node's nickname, a list of its neighbors, its current routing table, the nickname of the neighbor, and a list of routes to check, update the routing table based on the route information
update_routes(_, _NList, RTList, _NeighborNickname, []) ->
  RTList;
update_routes(MyNickname, NList, RTList, NeighborNickname, [{MyNickname, _ConnectionNode, _Distance}| RemainingRoutes]) ->
  update_routes(MyNickname, NList, RTList, NeighborNickname, RemainingRoutes);
update_routes(MyNickname, NList, RTList, NeighborNickname, [{DestinationNickname, _ConnectionNode, Distance}| RemainingRoutes]) ->
  % Check if the destination is a neighbor. If not, it's possible there are changes to the routing table. If it is, ignore it.
  case lists:filter(fun(X) -> X == DestinationNickname end, NList) of
    % Not a neighbor
    [] ->
      % Check if there is a route in the routing table. If there is, compare distances and update if the new distance is shorter. If there isn't, just append the new route to the routing table.
      case lists:keyfind(DestinationNickname, 1, RTList) of
        {DestinationNickname, _Connection, OldDistance} ->
          case OldDistance > Distance + 1 of
            false ->
              update_routes(MyNickname, NList, RTList, NeighborNickname, RemainingRoutes);
            true ->
              UpdatedRoutingTable = lists:keyreplace(DestinationNickname, 1, RTList, {DestinationNickname, NeighborNickname, Distance + 1}),
              update_routes(MyNickname, NList, UpdatedRoutingTable, NeighborNickname, RemainingRoutes)
          end;
        false ->
          UpdatedRoutingTable = RTList ++ [{DestinationNickname, NeighborNickname, Distance + 1}],
          update_routes(MyNickname, NList, UpdatedRoutingTable, NeighborNickname, RemainingRoutes)
      end;
    % A neighbor
    _ ->
      update_routes(MyNickname, NList, RTList, NeighborNickname, RemainingRoutes)
  end.
% update_routing_table(RTList, Target, PID, Hops) ->
%     case lists:keyfind(Target, 1, RTList) of
%         {Target, _, OldHops} when Hops < OldHops ->
%             lists:keyreplace(Target, 1, RTList, {Target, PID, Hops});
%         {Target, _, _} ->
%             RTList;
%         false ->
%             RTList ++ [{Target, PID, Hops}]
%     end.
processMsgQuestion(_, SenderNickname, MyNickname, _, Hops, _, _) when Hops >= 15 ->
    io:fwrite("~p ~p : Message to ~p is more than 15 hops ~n", [self(), SenderNickname, MyNickname]),
ok;
processMsgQuestion(N, SenderNickname, MyNickname, DestinationNickname, Hops, _, RTList) ->
    if MyNickname == DestinationNickname ->
        Answer = computeNthPrime(N),
        {Neighbour, _} = lookup(RTList, SenderNickname),
        if Neighbour =/= notFound ->
            Neighbour ! {receiveAnswer, N, Answer, MyNickname, Hops},
            SenderNickname ! {computeNthPrimeResult, Answer},
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

% Compute the Nth prime number by starting the search from number 2
computeNthPrime(N) ->
    findPrime(N, 2).

% Recursively find the Nth prime number
% If N is 0, return the last prime number found (CurrentNum - 1)
% Otherwise, check if the current number is prime
% If it is, decrement N and continue the search with the next number (CurrentNum + 1)
% If it isn't, continue the search with the next number (CurrentNum + 1)
findPrime(0, CurrentNum) ->
    CurrentNum - 1;
findPrime(N, CurrentNum) ->
    case isPrime(CurrentNum) of
        true ->
            findPrime(N - 1, CurrentNum + 1);
        false ->
            findPrime(N, CurrentNum + 1)
    end.

% Check if a number is prime
% 1 is not prime, and 2 is the smallest prime number
isPrime(1) ->
    false;
isPrime(2) ->
    true;
isPrime(N) ->
    isPrime(N, 2).

% Check if a number (N) is prime by dividing it with all numbers (CheckNum) starting from 2
% If N is equal to CheckNum, it means that N is only divisible by 1 and itself, so it's prime
% If N is divisible by CheckNum (N rem CheckNum == 0), then it's not prime
% If N is not divisible by CheckNum (N rem CheckNum > 0), continue the search with the next number (CheckNum + 1)
isPrime(N, N) ->
    true;
isPrime(N, CheckNum) ->
    case N rem CheckNum of
        0 ->
            false;
        _ ->
            isPrime(N, CheckNum + 1)
    end.