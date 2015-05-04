%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Adrián Moreira <adrianmoreirapereiro@gmail.com>
%%% @copyright (C) 2014, aian
%%% @doc DTestE_Peer
%%%
%%% Module to handle a peer associated with a node in the peer-to-peer (P2P) network.
%%% The peer receives requests from other components with tasks and returns the result.
%%% Interface is:
%%%
%%% Start a peer:
%%% peer:start().
%%%
%%% Stop a peer:
%%% peer:stop().
%%%
%%% This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option)%%% any later version.
%%%
%%% This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for m%%% ore details.   
%%% @end
%%% Created :  8 Oct 2014 by aian <aian@GrecoPrttl>
%%%-------------------------------------------------------------------
-module(dteste_peer).

%% PUBLIC API
-export([start/0, stop/0, work/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%PEER%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @doc Start a peer.
%% Abre un proceso y lo registra con el nombre "peer". Este proceso se queda escuchando a la espera de mensajes.
%% @end
%%--------------------------------------------------------------------
start()->
    register(pid_peer,spawn(fun loopPeer/0)),
    io:format(user,"[INFO]          DTestE.Peer->Node ~p has joined!~n", [node()]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% -buzón de mensajes, tres tipos:
%% -1-> petición de un cliente para que realice una tarea.
%% -2-> mensaje ping, el cliente lo usa para saber si el peer sigue activo.
%% -3-> mensaje "stop", se usa para desactivar el peer.
%% @end
%%--------------------------------------------------------------------
loopPeer() ->
    receive
	{Pid, Node, {N, Task}} ->
	    Args = {Node, Pid, {N, Task}},
	    spawn_link(?MODULE,work,[Args]),
	    loopPeer();
	{ping, Pid, _}->
	    Pid ! {pong, self(), node()},
	    loopPeer();
	{stop} ->
	    io:format(user,"[WARNING]      DTestE.Peer->Node ~p has out!~n", [node()]);
	_ ->
	    io:format(user,"[WARNING]      DTestE.Peer->Undefined!~n",[])
    end.

%%--------------------------------------------------------------------
%% @doc
%% Realiza el trabajo solicitado y envia al cliente un mensaje confirmando que ha realizado la tarea correctamente.
%% @end
%%--------------------------------------------------------------------
work({_, Pid, {N,{M, F, A}}})->
    io:format(user,"[INFO]          DTestE.Peer->Run Test....~n",[]),
    	       
    try
	F()
    of R ->
	    Pid ! {ok, self(), node(), N, {M,F,A}, {ok,R}}
    catch
	{eunit_internal, Term} ->
	    Pid ! {error, self(), node(), N, {M,F,A}, {eunit_internal, Term}};
	Class:Reason ->
	    Pid ! {error, self(), node(), N, {M,F,A}, {error,{Class, Reason}}}
    end.
    

    %R = rpc:call(node(),M,F,[]),
    %io:format(user,"**************RESULT:~p~n***************",[R]),
    %Pid ! {ok, self(), node(), N, {M,F,A}, {ok,R}}.

%%--------------------------------------------------------------------
%% @doc Stop a peer.
%% Envia un mesnaje de "stop" al buzón del peer donde se ejecuta para desconectarlo.
%% @end
%%--------------------------------------------------------------------
stop() ->
    pid_peer ! {stop},
    unregister(pid_peer),
    ok.
