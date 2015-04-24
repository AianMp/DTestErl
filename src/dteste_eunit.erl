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
-export([test/2]).

%%--------------------------------------------------------------------
%% @doc 
%% -Inicia la integración de EUnit con DTestE, recive el test de EUnit y se lo pasa al componente cliente.
%% @end
%%--------------------------------------------------------------------
test(F, Info)->
    io:format(user,"[INFO]          **DTestE**~n",[]),
    M = proplists:get_value(module, Info),
    R = dteste_client:request([{M,F,[]}]),
    io:format(user,"*********************************************~n",[]),
    io:format(user,"*****   Dtest.Euxfnit->The Result is:~p   ***~n",[R]),
    io:format(user,"*********************************************~n",[]),
    R.
