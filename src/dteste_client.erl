%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Adrián Moreira <adrianmoreirapereiro@gmail.com>
%%% @copyright (C) 2014, aian
%%% @doc DTestE_Client
%%%
%%% Module to handle a client associated with a node in the peer-to-peer (P2P) network.
%%% The client distributes a task list between all active peers, to send the tasks and receive the results.
%%% Interface is:
%%%
%%% Start a client:
%%% dteste_client:request([task1,task2,task3,...]).
%%%
%%%
%%% This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option)%%% any later version.
%%%
%%% This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for m%%% ore details.   
%%% @end
%%% Created :  26 Oct 2014 by aian <aian@GrecoPrttl>
%%%-------------------------------------------------------------------
-module(dteste_client).

%% PUBLIC API
-export([request/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%CLIENT%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @doc 
%% -Inicia el cliente, para cada cliente se abre un proceso que ejecuta la función cliente pasandole la lista de tareas que se ha solicitado.
%% @end
%%--------------------------------------------------------------------
request(TaskList)->
    %io:format(user,"[INFO]     DTestE.Client->New Request!~n",[]),
    spawn(fun()->client(TaskList) end).

%%--------------------------------------------------------------------
%% @doc
%% -La función cliente se encarga de gestionar la lista de tareas.
%% -Primero llama a la función "ping" pasandole los nodos conocidos para saber cuales están activos.
%% -Segundo llama a la función "distribute" para que distribuya las tareas entre los nodos activos.
%% -Tercero guarda el su diccionario la lista de relaciones tarea nodo ordenadas por un número correlativo.
%% -En último lugar llama a la funcion "send" para enviar a cada nodo sus tarea asignadas.
%% @end
%%--------------------------------------------------------------------
client(L)->
    put('taskNodeList',[]),
    %TaskList = parse(L),
    TaskList = L,
    auxClient(TaskList, []).
auxClient(TaskList,Results)->
    NodeList = ping([node()|nodes()]),
    TaskNodeList = distribute(TaskList, NodeList),
    send(TaskNodeList),
    put('taskNodeList', lists:append(get('taskNodeList'),TaskNodeList)),
    loopClient(Results).
    
parse(L)->
    auxParse(L,[]).
auxParse([],TaskList)->
    TaskList;
auxParse([{M,F,A}|T], TaskList) ->
    auxParse(T, [{list_to_atom(M),list_to_atom(F),A}|TaskList]).


%%--------------------------------------------------------------------
%% @doc
%% Función "ping" se encarga de enviar un mensaje "ping" a cada nodo de la lista de nodos, si el nodo responde en menos de 500 ms se añade a la lista de nodos activos.
%% @end
%%--------------------------------------------------------------------
ping(L)->
    auxPing(L,[]).
auxPing([],NodeList) ->
    NodeList;
auxPing([H|T], NodeList)->
    {pid_peer, H}!{ping,self(),node()},
    receive
	{pong,_,Node} ->
	    %io:format(user,"[INFO]     DTestE.Client->Node ~p available!~n",[Node]),
	    auxPing(T,[Node|NodeList])
    after 
	1000 ->
	    %io:format(user,"[WARNING]  DTestE.Client->Node ~p NOT available!~n", [H]),
	    auxPing(T,NodeList)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Función distribute, distribuye la lista de tareas entre los nodos activos
%% @end
%%--------------------------------------------------------------------
distribute(TaskL,NodeL)->
    auxDistribute(TaskL,NodeL,NodeL,0,[]).
auxDistribute([],_,_,_,L)->
    L;
auxDistribute(T,[],N,Num,L)->
    auxDistribute(T,N,N,Num,L);
auxDistribute([Ht|Tt], [Hn|Tn],N, Num,L)->
    auxDistribute(Tt,Tn,N,Num+1,[{Num+1,Ht,Hn}|L]).
    
%%--------------------------------------------------------------------
%% @doc
%% Función "send", envia un mensaje con la tarea al proceso registrado con nombre peer el el nodo correspondiente.
%% @end
%%--------------------------------------------------------------------
send([])->
    ok;
send([{Num,Task,Node}|T])->
    {pid_peer, Node} ! {self(), node(),{Num ,Task}},
    %io:format(user,"[INFO]     DTestE.Client->Task has been sent to node: ~p~n",[{Node, Num,Task}]),
    send(T).

%%--------------------------------------------------------------------
%% @doc
%% Buzón de mensajes
%% Almacena los resultados enviados desde los nodos peer, el resultado es un mensaje de confirmación de que la tarea se ha realizado.
%% si tarda más de 10000 ms en llegar un mensaje se llama a "controller" que mira si quedan tareas pendientes de llegar.
%% @end
%%--------------------------------------------------------------------
loopClient(Results) ->
    receive
	{error, _, Node, Num, Task, R}->
	    %io:format(user,"[FAIL]     DTestE.Client->Has received a {error} of the task ~p~n",[Num]),
	    %io:format(user," in node ~p~n",[Node]),
	    loopClient([{{Num,Task,Node},{error,R}}|Results]);
	{ok, _, Node, Num, Task, R}->
	    %io:format(user,"[INFO]     DTestE.Client->Has received a {ok} of the task ~p~n",[Num]),
	    %io:format(user," in node ~p~n",[Node]),
	    loopClient([{{Num,Task,Node},{ok,R}}|Results]);
	{stop} ->
	    io:format(user,"[END LOOP]~n",[])    
    after 2000 ->
	    controller(Results)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Función "controller" comprueba si falta algún resultado por llegar, compara los resultados que han llegado con la lista de relaciones tarea nodo almacenada en el diccionario. 
%% Si detecta que faltan resultados y que los nodos no están activos, extrae las tareas y se las envia a la función cliente para que vuelva a distribuirlas.
%% @end
%%--------------------------------------------------------------------
controller(Results)->
    TaskNodeList = get('taskNodeList'),
    TaskRun = lists:subtract(TaskNodeList,getTaskNodeList(Results)),
    case TaskRun of
	[]->
	    %io:format(user,"~n***********The client ends!************~n",[]),
	    print_header(),
	    print(Results, 0, 0),
	    self() ! {stop};
	L  ->
	    TaskError = active(L),
	    put('taskNodeList', lists:subtract(get('taskNodeList'),TaskError)),
	    %io:format(user,"[WARNING]  DTestE.Client->Lost ~p tasks~n", [length(TaskError)]),
	    case TaskError of
		[] ->
		    loopClient(Results);
		_ ->
		    auxClient(getTasks(TaskError),Results)
	    end
    end.

%%--------------------------------------------------------------------
%% @doc
%% función getTasks devuelve la lista de tareas de una lista de relaciones tarea nodo.
%% @end
%%--------------------------------------------------------------------
getTasks(L)->
    auxGetTasks(L,[]).
auxGetTasks([], TaskList)->  
    TaskList;
auxGetTasks([{_,Task,_}|T], TaskList) -> 
    auxGetTasks(T,[Task|TaskList]).

getTaskNodeList(Results)->
    auxGetTaskNodeList(Results, []).
auxGetTaskNodeList([],L)->
    L;
auxGetTaskNodeList([{TaskNode, _}|T],L)->
    auxGetTaskNodeList(T,[TaskNode|L]).


%%--------------------------------------------------------------------
%% @doc
%% función active, mira si los nodos de la lista siguen activos, en caso contrario extrae la lista de tareas asignadas a eses nodos.
%% @end
%%--------------------------------------------------------------------
active(TaskError)->
    auxActive([], TaskError).
auxActive(TaskError,[])->
    TaskError;
auxActive(TaskError,[{Num, Task, Node}|T])->
    {pid_peer, Node}!{ping,self(),node()},
    receive
	{pong,_,Node} ->
	    auxActive(TaskError, T)
    after 
	2000 ->
	    auxActive([{Num,Task,Node}|TaskError], T)
    end.  

print_header() ->
    print_newLine(),
    print_title(),
    print_newLine(),
    print_subTitle(),
    print_newLine().

print_line() ->
    io:format(user,"________________________________________________________________________________________~n",[]).

print_newLine()->
    io:format(user,"                                                                                        ~n",[]).

print_title()->
    io:format(user,"      ****************************************************************************      ~n",[]),
    io:format(user,"      *                                                                          *      ~n",[]),
    io:format(user,"      *                                   DTestE                                 *      ~n",[]),
    io:format(user,"      *                                                                          *      ~n",[]),
    io:format(user,"      ****************************************************************************      ~n",[]).

print_subTitle()->
    io:format(user,"*********************************      Test Summary      *******************************~n",[]).
    
print_numT(NumT)->    
    io:format(user,"     NumT:    ~p~n",[NumT]).
print_node(Node)->
    io:format(user,"     Node:    ~p~n",[Node]).
print_module(Module)->
    io:format(user,"     Module:  ~p~n",[Module]).
print_status(Status)->
    io:format(user,"     Status:  ~p~n",[Status]).
print_result(Result)->
    io:format(user,"     Result:  ~p~n",[Result]).

print([],NumPassed, NumFail)->
    print_newLine(),
    io:format(user,"     Overall ~p PASSED and ",[NumPassed]),
    io:format(user,"~p FAIL~n",[NumFail]);
print([{{Num,{M,_,[]},Node},{S, R}}|T],NumPassed,NumFail)->
    print_numT(Num),
    print_node(Node),
    print_module(M),
    print_status(S),
    print_result(R),
    print_line(),
    case S of
	ok-> print(T, NumPassed + 1, NumFail);
	error->print(T,NumPassed, NumFail + 1)
    end.
