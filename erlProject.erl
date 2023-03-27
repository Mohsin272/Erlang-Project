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
    SecondNickname ! {connect, FirstNickname, FirstPID},
    FirstNickname ! {updateRT, SecondNickname, SecondPID, 1},
    SecondNickname ! {updateRT, FirstNickname, FirstPID, 1},
    propagate_rt(FirstNickname, SecondNickname, SecondPID, 1).

propagate_rt(From, Target, TargetPID, Hops) ->
    From ! {propagateRT, Target, TargetPID, Hops},
    From ! {requestNeighbours, self()},
    receive
        Neighbours ->
            [Neighbour ! {propagateRT, Target, TargetPID, Hops + 1} || {_, Neighbour} <- Neighbours]
    end.
% Print the routing table of a given node
printTable(PID) ->
    PID ! {requestRoutingTable, self()},
    receive
        {routingTable, RTList} ->
            lists:map(fun({Target, _, Hops}) -> {Target, Hops} end, RTList)
    end.

% Initiate the computation of the Nth prime number
computeNthPrime(N, SenderNickname, DestinationNickname, Hops)->
    SenderNickname ! {computeNthPrime, N, SenderNickname, DestinationNickname, Hops}.

% Start the node with a given Nickname
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