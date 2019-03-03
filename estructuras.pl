% Open a file: consult('/home/raul/Escritorio/IA/estructuras.pl').

% Method
busca(Llave,[(Llave,Valor)|Dict],Valor).
busca(Llave,[(Llave1,_)|Dict],Valor) :-
Llave \= Llave1,
busca(Llave,Dict,Valor).

arbol_binario(vacio).
arbol_binario(arbol(Nodo,Izq,Der)) :-
arbol_binario(Izq),
arbol_binario(Der).
elemento_arbol(N,arbol(N,_,_)).
elemento_arbol(N,arbol(_,Izq,_)) :-
elemento_arbol(N,Izq).
elemento_arbol(N,arbol(_,_,Der)) :-
elemento_arbol(N,Der).


busca(Llave,dict(Llave,X,Izq,Der),Valor) :-
!, X = Valor.
busca(Llave,dict(Llave1,X,Izq,Der), Valor) :-
Llave < Llave1,
busca(Llave,Izq,Valor).
busca(Llave,dict(Llave1,X,Izq,Der), Valor) :-
Llave > Llave1,
busca(Llave,Der,Valor).