-module(ring).
-export([start/1, crear_procesosRing/3, loop/2, send/3, stop/1]).

start(N) ->
    spawn(ring, crear_procesosRing, [0, N-1, self()]). 
 

crear_procesosRing(N, Max, PrimerPID) ->
    if 
        N == Max ->
            link(PrimerPID),
            %io:format("Proceso ~p creado, PID: ~p, siguiente ~p~n", [N+1,self(),PrimerPID]),
            loop(PrimerPID,N);  

        N == 0 ->
            Next = spawn_link(ring, crear_procesosRing, [N+1, Max, self()]),  
            %io:format("Proceso ~p creado, PID: ~p, siguiente ~p~n", [N, self(), Next]),
            loop(Next, N);  
            
        true ->
            Next = spawn_link(ring, crear_procesosRing, [N+1, Max, PrimerPID]),
            %io:format("Proceso ~p, PID del primero: ~p, PID siguiente: ~p~n", [N, PrimerPID, Next]),
            loop(Next, N)  
    end.

loop(Next, N) -> 
    receive
        stop -> 
            exit(final);

        {Msg, 0} ->  
            io:format("~p recibió mensaje: ~p con ~p por mandar~n", [N, Msg, 0]);
            loop(Next, N);

        {Msg, Num} ->  
            io:format("~p recibió mensaje: ~p con ~p por mandar~n", [N, Msg, Num]),
            Next ! {Msg, Num-1},
            loop(Next, N)  
    end.
    
  

send(Pid, Num, Msg) -> 
    Pid ! {Msg, Num-1},
    ok. 

stop(Pid) -> 
    Pid ! stop,
    ok.     
