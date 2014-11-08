%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Adrián Moreira <adrian.moreira@udc.es>
%%% @copyright (C) 2014, aian
%%% @doc Peer
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
-module(peer).

%% PUBLIC API
-export([start/0, stop/0]).


%%%%%%%%%%%%%%%%%%%%%%%%%PEER%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
%%--------------------------------------------------------------------
%% @doc Start a peer.
%% Abre un proceso y lo registra con el nombre "peer". Este proceso se queda escuchando a la espera de mensajes.
%% @end
%%--------------------------------------------------------------------
start()->
    register(peer,spawn(fun loopPeer/0)),
    io:format("Node ~p has joined!~n", [node()]),
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
	    %io:format("###PEER###          Opens process ~n"),
	    spawn_link(fun()->work({Node, Pid, {N,Task}}) end),
	    loopPeer();
	{ping, Pid, _}->
	    %io:format(".........ping~n"),
	    Pid ! {pong, self(), node()},
	    loopPeer();
	{stop} ->
	    io:format("###PEER###          Node ~p has out!~n", [node()]),
	    exit(peer,normal);
	_ ->
	    io:format("undefined!~n")
    end.

%%--------------------------------------------------------------------
%% @doc
%% Realiza el trabajo solicitado y envia al cliente un mensaje confirmando que ha realizado la tarea correctamente.
%% @end
%%--------------------------------------------------------------------
work({_, Pid, {N,Task}})->
    
    %io:format("###PEER### Sleeping ~p seconds!~n", [T/1000]),
    io:format("###PEER###          Run...........~n"),
    timer:sleep(random:uniform(20000) + 5000),
    io:format("###PEER###          end run ~p!~n",[{N,Task}]),
    Pid ! {ok,self(), node(), N, Task}.
 
%%--------------------------------------------------------------------
%% @doc Stop a peer.
%% envia un mesnaje de "stop" al buzón del peer donde se ejecuta para desconectarlo.
%% @end
%%--------------------------------------------------------------------
stop() ->
    peer ! {stop},
    unregister(peer),
    ok.
