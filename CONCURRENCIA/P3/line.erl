-module(line).
-export([start/1, crear_procesos/2, send/2, stop/1]).

start(N) ->
    spawn(line, crear_procesos, [0,N-1]).

crear_procesos(N,Max) ->
    if 
        N == Max ->
           % io:format("Proceso Final~n"),
            ultimo_loop(N); 
        true ->
           % io:format("proceso ~p~n", [N]),
            Next = spawn(line, crear_procesos, [N+1,Max]),
            loop(Next, N)
    end.
    
ultimo_loop(N) ->
    receive
        stop -> 
            io:format("Proceso ~p detenido~n", [N]);
        {send, Msg} ->
            io:format("Proceso ~p recibió mensaje: ~p~n", [N, Msg]),
            ultimo_loop(N)  
    end.
	

loop(Next, N) -> 
    receive
        stop -> 
            io:format("Proceso ~p detenido~n", [N]),
            Next ! stop;
        {send,Msg} ->
            io:format("Proceso ~p recibió mensaje: ~p~n", [N, Msg]),
            Next ! {send,Msg},
            loop(Next, N)  
    end.

send(Pid, Msg) -> 
    Pid ! {send, Msg},
    ok. 

stop(Pid) -> 
    Pid ! stop,
    ok.
