-module(erlProject).
-export([computeNthPrime/1,ripNode/3,createNode/1,start/1,connectNode/4,setup/0,printTable/1,computeNthPrime/4,printNeighbours/1]).

setup()->
    [createNode(john),
     createNode(bob),
     createNode(fred),
     createNode(joe),
     createNode(jimbob),
     createNode(mo),
     createNode(charlie),
     createNode(kate),
     createNode(jessy),
     createNode(walter)].

createNode(Nickname)->
    PID=spawn(fun()->erlProject:start(Nickname) end),
    io:fwrite("~p : Created node as ~p ~n",[PID,Nickname]),
    PID.

connectNode(FirstNickname,FirstPID,SecondNickname,SecondPID)->
    FirstNickname!{connect,SecondNickname,SecondPID},
    SecondNickname!{connect,FirstNickname,FirstPID}.

printTable(PID)->
    PID!{printRoutingTable}.
printNeighbours(PID)->
    PID!{printNeighboursTable}.
computeNthPrime(N,Sender,Desination,Hops)->
    Sender ! {computeNthPrime,N,Sender,Desination,Hops}.
start(Nickname)->
    register(Nickname,self()),
    ripNode(Nickname,[],[]).

ripNode(MyNickname, NList, RTList) ->
    receive
        % {printNeighbours} ->
        %     io:fwrite("~p ~p : Neighbours Table ~p ~n", [self(), MyNickname, NList]),
        %     ripNode(MyNickname, NList, RTList);
        {printRoutingTable} ->
            io:fwrite("~p ~p : Routing Table ~p ~n", [self(), MyNickname, RTList]),
            ripNode(MyNickname, NList, RTList);
        {computeNthPrime, N, Sender, Destination, Hops} ->
            processMsgQuestion(N, Sender, Destination, Hops, NList, RTList);
        {recieveAnswer, N, M, Sender, Destination, Hops} ->
            processMsgAnswer(N, M, Sender, Destination, Hops);
        {updateRT, Sender, RTList} ->
            processRT(RTList);
        {connect, Nickname, PID} ->
            io:fwrite("~p ~p : Connected to node ~p ~n", [self(), PID, Nickname]),
            ripNode(MyNickname, NList ++ [{Nickname, PID}], RTList);
        {printNeighboursTable} ->
            lists:foreach(fun({Nickname, PID}) ->
                                    io:fwrite("~p ~p: Neighbour ~p with PID ~p~n", [self(), MyNickname, Nickname, PID])
                            end, NList),
            ripNode(MyNickname, NList, RTList)
    end,
    ripNode(MyNickname, NList, RTList).
    

% ripNode(MyNickname, NList, RTList) ->
%     receive
%         {printNeighbours}->
%             io:fwrite("~p ~p : Neighbours Table ~p ~n",[self(),MyNickname,NList]),
%             ripNode(MyNickname,NList,RTList);
%         {printRoutingTable}->
%             io:fwrite("~p ~p : Routing Table ~p ~n",[self(),MyNickname,RTList]),
%             ripNode(MyNickname,NList,RTList);
%         {computeNthPrime, N, Sender, Desination, Hops} ->
%             processMsgQuestion(N, Sender, Desination, Hops, NList, RTList);
%         {recieveAnswer, N, M, Sender, Desination, Hops} ->
%             processMsgAnswer(N, M, Sender, Desination, Hops);
%         {updateRT, Sender, RTList} ->
%             processRT(RTList);
%         {connect,Nickname,PID}->
%             io:fwrite("~p ~p : Connected to node ~p ~n",[self(),PID,Nickname]),
%             ripNode(MyNickname,NList ++ [{Nickname,PID}],RTList)
%     end,
%     ripNode(MyNickname, NList, RTList).

processMsgQuestion(N, Sender, Desination, Hops, NList, RTList) when Hops > 15 ->
    io:fwrite("~p ~p : Message to ~p is more than 15 hops ~n",[self(),Sender,Desination]);
processMsgQuestion(N, Sender, MyNickname, Hops, NList, RTList) ->
    Answer = computeNthPrime(N),
    Neighbour = lookup(RTList, Sender),
    if Neighbour =/= notFound ->
            Neighbour ! {recieveAnswer, N, Answer, MyNickname, Sender, Hops},
            ok;
        true ->
            ok
    end,
    processMsgQuestion(N, Sender, MyNickname, Hops + 1, NList, RTList);
processMsgQuestion(N, Sender, Desination, Hops, NList, RTList) ->
    Neighbour = lookup(RTList, Desination),
    if Neighbour == notFound ->
            ok;
        true ->
            Neighbour ! {computeNthPrime, N, Sender, Desination, Hops + 1},
            ok
    end.

lookup([], _) ->
    {notFound, 0};
lookup([{Sender, Neighbour, Hops} | Tail], Sender) ->
    {Neighbour, Hops};
lookup([{_, _, _} | Tail], Sender) ->
    lookup(Tail, Sender).
    

processMsgAnswer(N, M, Sender, Destination, Hops) ->
    io:format("Answer received: N=~p, M=~p, Sender=~p, Destination=~p, Hops=~p~n",[N, M, Sender, Destination, Hops]).

processRT(RTList) ->
    io:format("RT List: ~p~n", [RTList]),
    ok.

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
