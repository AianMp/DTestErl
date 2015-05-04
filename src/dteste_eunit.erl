%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Adrián Moreira <adrianmoreirapereiro@gmail.com>
%%% @copyright (C) 2015, aian
%%% @doc DTestE_Eunit
%%%
%%% Interface is:
%%%
%%% Start a test:
%%% dteste_eunit:test(F, Info).
%%%
%%%
%%% This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option)%%% any later version.
%%%
%%% This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for m%%% ore details.   
%%% @end
%%% Created :  3 Apr 2015 by aian <aian@GrecoPrttl>
%%%-------------------------------------------------------------------
-module(dteste_eunit).

%% PUBLIC API
-export([test/2, loopEunit/1, run/0]).

%%--------------------------------------------------------------------
%% @doc
%% -Inicia y registra un nuevo proceso con nombre "pid_eunit", en caso de que exista devuelve el Pid. Este proceso será un daemon que encolará los Tests que EUnit quiera ejecutar.
%% @end
%%--------------------------------------------------------------------
getPid()->
    case whereis(pid_eunit) of
    	undefined -> 
	    register(pid_eunit,spawn(dteste_eunit,loopEunit, [[]]));
	Pid ->
	    Pid
    end.
	 
%%--------------------------------------------------------------------
%% @doc
%% -Añade cada Test que EUnit quiere ejecutar en una lista, cuando EUnit termina llama al cliente pasandole la lista.
%% @end
%%--------------------------------------------------------------------
loopEunit(TestList) ->
    receive
	{add, Test} ->
	    loopEunit([Test|TestList]);
	{run}->
	    %print_title(),
	    dteste_client:request(TestList)
    end.

%%--------------------------------------------------------------------
%% @doc 
%% -Inicia la integración de EUnit con DTestE, recive el test de EUnit y se lo pasa al componente cliente.
%% @end
%%--------------------------------------------------------------------
test(Fun, Info)->
    %io:format(user,"[INFO]          **DTestE**~n",[]),
    M = proplists:get_value(module, Info),
    %F = proplists:get_value(name, Info),
    %io:format(user,"is atom~p~n",[is_atom(F)]),
    getPid(),
    {pid_eunit, node()} ! {add, {M,Fun,[]}}.
    %R = dteste_client:request([{M,F,[]}]),
    %io:format(user,"*********************************************~n",[]),
    %io:format(user,"*****   Dtest.Euxfnit->The Result is:~p   ***~n",[R]),
    %io:format(user,"*********************************************~n",[]),
    %R.

run()->
    {pid_eunit, node()} ! {run}.

%print_title()->
%    io:format(user,"      ****************************************************************************      ~n",[]),
%    io:format(user,"      *                                                                          *      ~n",[]),
%    io:format(user,"      *                                   DTestE                                 *      ~n",[]),
%    io:format(user,"      *                                                                          *      ~n",[]),
%    io:format(user,"      ****************************************************************************      ~n",[]).
