herramientasRequeridas(ordenarCuarto, [aspiradora(100), trapeador, plumero]).
herramientasRequeridas(limpiarTecho, [escoba, pala]).
herramientasRequeridas(cortarPasto, [bordedadora]).
herramientasRequeridas(limpiarBanio, [sopapa, trapeador]).
herramientasRequeridas(encerarPisos, [lustradpesora, cera, aspiradora(300)]).

% 1)
tiene(egon,aspiradora(200)).
tiene(egon,trapeador).
tiene(peter,trapeador).
tiene(winston,varitaDeNeutrones).
tiene(tito,sopapa).
tiene(tito,trapeador).

dustBuster(ray).
dustBuster(winston).
dustBuster(Personaje):- tiene(Personaje,_).

% 2)
satisfaceHerramientaRequerida(Personaje,Herramienta):-
    tiene(Personaje,Herramienta).

satisfaceHerramientaRequerida(Personaje,aspiradora(PotenciaRequerida)):-
    tiene(Personaje,aspiradora(Potencia)),
    between(0,Potencia,PotenciaRequerida). % -> Para que sea inverisble 

% 3)
puedeRealizar(Personaje,Tarea):-
    dustBuster(Personaje),
    herramientasRequeridas(Tarea,_),
    forall(herramientaNecesaria(Herramienta,Tarea),
        satisfaceHerramientaRequerida(Personaje,Herramienta)).

puedeRealizar(Personaje,Tarea):-
    herramientasRequeridas(Tarea,_),
    tiene(Personaje,varitaDeNeutrones).

herramientaNecesaria(Herramienta,Tarea):-
    herramientasRequeridas(Tarea,Herramientas),
    member(Herramienta,Herramientas).

% 4)
tareaPedida(ordenarCuarto, dana, 20).
tareaPedida(cortarPasto, walter, 50).
tareaPedida(limpiarTecho, walter, 70).
tareaPedida(limpiarBanio, louis, 15).

precio(ordenarCuarto, 13).
precio(limpiarTecho, 20).
precio(limpiarBanio, 55).
precio(cortarPasto, 10).
precio(encerarPisos, 7).

cobroTotal(Cliente,Total):-
    tareaPedida(_,Cliente,_),
    findall(Cobro,cobro(_,_,Cobro),Cobros),
    list_to_set(Cobros, CobrosFilt),
    sumlist(CobrosFilt, Total).
    
cobro(Tarea,Cliente,Cobro):-
    tareaPedida(Tarea,Cliente,Metros),
    precio(Tarea,PrecioPorMetro),
    Cobro is Metros*PrecioPorMetro.    

% 5)
aceptaPedido(Personaje,Pedido,Cliente):-
    estaDispuesto(Personaje,Pedido),
    forall(tareaPedida(Tarea,Cliente,_),
        puedeRealizar(Personaje,Tarea)).

estaDispuesto(Personaje,Pedido):-
    tareaPedida(Pedido,_,_),
    cumpleRequisitos(Pedido,Personaje).

cumpleRequisitos(Pedido,ray):- Pedido \= limpiarTecho.
cumpleRequisitos(Pedido,winston):-
    cobro(Pedido,_,Cantidad),
    Cantidad > 500.
cumpleRequisitos(Pedido,egon):-
    tareaPedida(Pedido,_,_),
    not(tareaCompleja(Pedido)).
cumpleRequisitos(_,peter).

tareaCompleja(limpiarTecho).
tareaCompleja(Pedido):-
    herramientasRequeridas(Pedido,Herramientas),
    length(Herramientas, Cantidad),
    Cantidad > 2.

% 6)












