%%% @author aian <aian@GrecoPrttl>
%%% @copyright (C) 2014, aian
%%% @doc 
%%%
%%% @end
%%% Created :  7 Nov 2014 by aian <aian@GrecoPrttl>

-module(initEscenario).

%% PUBLIC API
-export([start/0]).

%%--------------------------------------------------------------------
%% @doc
%% Inicia el escenario para ejecuciones, conecta los nodos creados.
%% @end
%%--------------------------------------------------------------------
start()->
    Host = net_adm:localhost(),
    net_adm:ping(list_to_atom("nodeA@"++Host)),
    net_adm:ping(list_to_atom("nodeB@"++Host)),
    net_adm:ping(list_to_atom("nodeC@"++Host)),
    net_adm:ping(list_to_atom("nodeD@"++Host)),
   ok.
