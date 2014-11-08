DTestErl
========

Sistema distribuido para ejecución de pruebas en Erlang.

--EJECUCIÓN--

*-Compilar:
-make compile (directorio: /DTestE/ )
-make shell (directorio: /DTestE/ )

*-Abre 4 terminales, son cuatro nodos con nombre (nodA, nodoB, nodoC, nodoD) que estan conectados.

*-Los nodos nodoA, nodoB y nodoC por defecto estan activos (se ha hecho "peer:start()." en cada uno) por lo que estan listos para responder a un cliente.

*-El nodoD no esta activo, para activarlo es necesario:
"peer:start()."

*-Desactivar un nodo, ejecutamos en el terminal correspondiente:
"peer:stop()."

*-Lanzar una lista de peticiones desde cualquier nodo activo o no activo, ejecutamos en el terminal correspondiente:
client:request([Task1,Task2,Task3])
TaskN = Numero | String

*-Para crear más nodos es necesario abrir un nuevo terminal de la siguiente forma:
"cd /ebin/"
"erl -sname nameNodo -setcookie clave" (la cookie tiene que ser igual a "clave" para que se pueda conectar con los nodos anteriores)
Dentro de la máquina creada:
"net_adm:ping('nodeA@localhost')." (localhost = nombre de la maquina)
