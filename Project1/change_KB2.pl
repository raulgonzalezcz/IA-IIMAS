% Open a file: consult('/home/raul/Escritorio/IA/Project1/change_KB2.pl').

%--------------------------------------------------
% Load and Save from files
%--------------------------------------------------


%KB open and save

open_kb(Route,KB):-
	open(Route,read,Stream),
	readclauses(Stream,X),
	close(Stream),
	atom_to_term(X,KB).

save_kb(Route,KB):-
	open(Route,write,Stream),
	writeq(Stream,KB),
	close(Stream).

%definicion de los operadores ---------------------------------------------------------------------------

:- op(500,xfy,'=>').  %operador de asignacion
:- op(801,xfy,'=>>'). %operador de implicación
%--------------------------------------------------------------------------------------------------------

%------------------------------
% Ejemplo:  
%------------------------------

%Cargar la base en una lista, imprimir la lista en consola y guardar todo en un nuevo archivo.
%No olvides poner las rutas correctas para localizar el archivo kb.txt en tu computadora!!!

abrir(KB):-
	open_kb('/home/raul/Escritorio/IA/Project1/KB1.txt',KB),
	write('\nReading actual data...'),
	write('\nKB: '),
	write(KB).

guardar(KB):-
	write('\nSaving new data...'),
	save_kb('/home/raul/Escritorio/IA/Project1/New_KB.txt',KB).


%predicado para abrir un archivo -------------------------------------------------------------------------
abrir1(KB):-
	open('/home/raul/Escritorio/IA/Project1/KB1.txt',read,Stream),
	readclauses(Stream,X),
	close(Stream),
	atom_to_term_conversion(X,KB).

%--------------------------------------------------------------------------------------------------------


% predicado para guardar un archivo ---------------------------------------------------------------------
guardar1(KB):-
	open('/home/raul/Escritorio/IA/Project1/New_KB.txt',write,Stream),
	writeq(Stream,KB),
	close(Stream).

%--------------------------------------------------------------------------------------------------------

%predicados auxiliares para el manejo de archivos -------------------------------------------------------

readclauses(InStream,W) :-
        get0(InStream,Char),
        checkCharAndReadRest(Char,Chars,InStream),
	atom_chars(W,Chars). 

checkCharAndReadRest(-1,[],_) :- !.  % End of Stream	
checkCharAndReadRest(end_of_file,[],_) :- !.

checkCharAndReadRest(Char,[Char|Chars],InStream) :-
        get0(InStream,NextChar),
        checkCharAndReadRest(NextChar,Chars,InStream).

atom_to_term_conversion(ATOM, TERM) :-
	 atom(ATOM),
	 atom_to_chars(ATOM,STR),
	 atom_to_chars('.',PTO),
	 append(STR,PTO,STR_PTO),
	 read_from_chars(STR_PTO,TERM).

%--------------------------------------------------------------------------------------------------------


%----------------------------------------
% Administration of lists
%----------------------------------------


%Change all ocurrences of an element X in a list for the value Y
%cambiarGrupoObjetos(X,Y,InputList,OutputList).
%Example (a,b,[p,a,p,a,y,a],[p,b,p,b,y,b])

cambiarGrupoObjetos(_,_,[],[]).

cambiarGrupoObjetos(X,Y,[X|T],[Y|N]):-
	cambiarGrupoObjetos(X,Y,T,N).

cambiarGrupoObjetos(X,Y,[H|T],[H|N]):-
	cambiarGrupoObjetos(X,Y,T,N).

cambiarGrupoObjetos(X,Y,[[X|T]],[[Y|N]]):-
	cambiarGrupoObjetos(X,Y,[X|T],[Y|N]).

cambiarGrupoObjetos(X,Y,[[H|T]],[[H|N]]):-
	cambiarGrupoObjetos(X,Y,[H|T],[H|N]).

%Delete all ocurrences of an element X in a list
%borrarElemento(X,InputList,OutputList).
%Example (a,[p,a,p,a,y,a],[p,p,y])

borrarElemento(_,[],[]).

borrarElemento(X,[[id=>Lista|_]|T],N):-
	member(X,Lista),
	borrarElemento(X,T,N).

borrarElemento(X,[H|T],[H|N]):-
	borrarElemento(X,T,N),
	X\=H.


%Delete all ocurrences of an element X in a list
%borrarElementoPref(X,InputList,OutputList).
%Example (a,[p,a,p,a,y,a],[p,p,y])

borrarElementoPref(_,_,[],[]).

borrarElementoPref(X,Peso,[ [_]=>>X=>(_,Peso)|T ],N):-
	borrarElementoPref(X,T,N).

borrarElementoPref(X,_,[[H]=>>A=>_|T],[[H]=>>A=>_|N]):-
	borrarElementoPref(X,T,N),
	X\=A.

%Verify if an element X is in a list 
%esObjeto(X,List)
%Example (n,[b,a,n,a,n,a])

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

esObjeto(X,[[id=>Lista|_]|_]):-
	member(X,Lista).

esObjeto(X,[ _ |T]):-
	esObjeto(X,T).

%Convert in a single list a list of lists
%Example ([[a],[b,c],[],[d]],[a,b,c,d]).

append_list_of_lists([],[]).

append_list_of_lists([H|T],X):-
	append(H,TList,X),
	append_list_of_lists(T,TList).


%Delete all elements with a specific property in a property-value list
%borrarElementosConPropiedad(P,InputList,OutputList).
%Example (p2,[p1=>v1,p2=>v2,p3=>v3,p2=>v4,p4=>v4],[p1=>v1,p3=>v3,p4=>v4])

borrarElementosConPropiedad(_,[],[]).

borrarElementosConPropiedad(X,[X=>_|T],N):-
	write("T:"),
	write(T),
	borrarElementosConPropiedad(X,T,N).

borrarElementosConPropiedad(X,[H|T],[H|N]):-
	write("H:"),
	write(H),
	borrarElementosConPropiedad(X,T,N).


%Delete all elements with a specific property preference in a property-value list
%borrarElementosConPropiedadPref(P,InputList,OutputList).
%Example (pref, peso,[ [trabaja=>(X)]=>>pref=>(X,peso),p2=>v2,p3=>v3,p2=>v4,p4=>v4],[p1=>v1,p3=>v3,p4=>v4])

borrarElementosConPropiedadPref(_,_,[],[]).

borrarElementosConPropiedadPref(X,Peso,[ [_|_]=>>X=>(_,Peso)|T ],N):-
	write("Entra"),
	borrarElementosConPropiedadPref(X,Peso,T,N).

borrarElementosConPropiedadPref(X,Peso,[H|T],[H|N]):-
	write("H:"),
	write(H),
	borrarElementosConPropiedadPref(X,Peso,T,N).


%Delete all elements with a specific negated property in a property-value list
%borrarElementosConPropiedadNegada(P,InputList,OutputList).
%Example (p2,[p1=>v1,no(p2=>v2),no(p3=>v3),p2=>v4,p4=>v4],[p1=>v1,no(p3=>v3),p2=>v4,p4=>v4])

borrarElementosConPropiedadNegada(_,[],[]).

borrarElementosConPropiedadNegada(X,[no(X=>_)|T],N):-
	borrarElementosConPropiedadNegada(X,T,N).

borrarElementosConPropiedadNegada(X,[H|T],[H|N]):-
	borrarElementosConPropiedadNegada(X,T,N).

%Delete all elements with a specific negated property in a property-value list
%borrarElementosConPropiedadNegadaPref(P,InputList,OutputList).
%Example (p2,[p1=>v1,no(p2=>v2),no(p3=>v3),p2=>v4,p4=>v4],[p1=>v1,no(p3=>v3),p2=>v4,p4=>v4])

borrarElementosConPropiedadNegadaPref(_,_,[],[]).

borrarElementosConPropiedadNegadaPref(X,Peso,[ [_|_]=>>no(X=>(_,Peso))|T ],N):-
	borrarElementosConPropiedadNegadaPref(X,Peso,T,N).

borrarElementosConPropiedadNegadaPref(X,Peso,[H|T],[H|N]):-
	borrarElementosConPropiedadNegadaPref(X,Peso,T,N).

%--------------------------------------------------------------------------------------------------
%Operations for removing classes, objects or properties into the Knowledge Base
%--------------------------------------------------------------------------------------------------

% 3a) Remove a class
% Example: abrir1(KB), eliminar_clase(raton,KB,NewKB), guardar1(NewKB).
% Example: abrir1(KB), eliminar_clase(humano,KB,NewKB), guardar1(NewKB).
% Example: abrir1(KB), eliminar_clase(mamiferos,KB,NewKB), guardar1(NewKB).

eliminar_clase(Class,OriginalKB,NewKB) :-
	borrar_clase(class(Class,Father,_,_,_),OriginalKB,TemporalKB),
	cambiarPadre(Class,Father,TemporalKB,TemporalKB2),
	borrar_relacion_con_clase(Class,TemporalKB2,NewKB).

cambiarPadre(_,_,[],[]).

cambiarPadre(OldFather,NewFather,[class(C,OldFather,P,R,O)|T],[class(C,NewFather,P,R,O)|N]):-
	cambiarPadre(OldFather,NewFather,T,N).

cambiarPadre(OldFather,NewFather,[H|T],[H|N]):-
	cambiarPadre(OldFather,NewFather,T,N).

borrar_clase(_,[],[]).

borrar_clase(X,[X|T],N):-
	borrar_clase(X,T,N).

borrar_clase(X,[H|T],[H|N]):-
	borrar_clase(X,T,N),
	X\=H.

borrar_clase(X,[P],N):-
	borrar_clase(X,P,N).

borrar_relacion_con_clase(_,[],[]).

borrar_relacion_con_clase(Object,[class(C,M,P,R,O)|T],[class(C,M,P,NewR,NewO)|NewT]):-
	write("R:"),
	write(R),
	cancel_relation_class(Object,R,NewR),
	del_relations_class(Object,O,NewO),
	borrar_relacion_con_clase(Object,T,NewT).

del_relations_class(_,[],[]).

del_relations_class(Object,[[id=>N,P,R]|T],[[id=>N,P,NewR]|NewT]):-
	cancel_relation_class(Object,R,NewR),
	del_relations_class(Object,T,NewT).

cancel_relation_class(_,[],[]).

cancel_relation_class(Object,[[_=>(Object,_)|_]|T],NewT):-
	write("Entra:"),
	cancel_relation_class(Object,T,NewT).

cancel_relation_class(Object,[[no(_=>(Object,_))|_]|T],NewT):-
	write("Entra:"),
	cancel_relation_class(Object,T,NewT).

cancel_relation_class(Object,[H|T],[H|NewT]):-
	write("H:"),
	write(H),
	cancel_relation_class(Object,T,NewT).

%Remove an object where Object is IDObject (id=>Object) --Borrar relación con objectos
% Example: abrir1(KB), eliminar_objeto(pinocho,KB,NewKB), guardar1(NewKB).
% Example: abrir1(KB), eliminar_objeto(monstruo,KB,NewKB), guardar1(NewKB).
% Example: abrir1(KB), eliminar_objeto(miky,KB,NewKB), guardar1(NewKB).
% Example: abrir1(KB), eliminar_objeto(timothy,KB,NewKB), guardar1(NewKB).

eliminar_objeto(Object,OriginalKB,NewKB) :-
	cambiarGrupoObjetos(class(Class,Father,Props,Rels,Objects),class(Class,Father,Props,Rels,NewObjects),OriginalKB,TemporalKB),
	esObjeto(Object,Objects),
	borrarElemento(Object,Objects,NewObjects),
	borrar_relacion_con_objeto(Object,TemporalKB,NewKB).
	
borrar_relacion_con_objeto(_,[],[]).

borrar_relacion_con_objeto(Object,[class(C,M,P,R,O)|T],[class(C,M,P,NewR,NewO)|NewT]):-
	cancel_relation(Object,R,NewR),
	write("Relaciones:"),
	write(R),
	del_relations(Object,O,NewO),
	borrar_relacion_con_objeto(Object,T,NewT).

del_relations(_,[],[]).

del_relations(Object,[[id=>N,P,R]|T],[[id=>N,P,NewR]|NewT]):-
	write("Resto:"),
	write(R),
	cancel_relation(Object,R,NewR),
	del_relations(Object,T,NewT).

cancel_relation(_,[],[]).
cancel_relation(Object,[[_=>(Object,_)|_]|T],NewT):-
	write("Entra:"),
	cancel_relation(Object,T,NewT).

cancel_relation(Object,[[no(_=>(Object,_))|_]|T],NewT):-
	write("Entra:"),
	cancel_relation(Object,T,NewT).

cancel_relation(Object,[H|T],[H|NewT]):-
	write("H:"),
	write(H),
	cancel_relation(Object,T,NewT).

%Remove a class property
% Example: abrir1(KB), eliminar_clase_propiedad(ballena,ponen_huevos,KB,NewKB), guardar1(NewKB).
% Example: abrir1(KB), eliminar_clase_propiedad(humano,muerde,KB,NewKB), guardar1(NewKB).

eliminar_clase_propiedad(Class,Property,OriginalKB,NewKB) :-
	cambiarGrupoObjetos(class(Class,Father,[Props,T],Rels,Objects),class(Class,Father,ListProp,Rels,Objects),OriginalKB,NewKB),
	borrarElementosConPropiedad(Property,Props,Aux),
	borrarElemento(no(Property),Aux,Aux2),
	borrarElemento(Property,Aux2,NewProps),
	write("A"),
	write(Aux),
	unificarListas(NewProps,T,ListProp).

eliminar_clase_propiedad(Class,no,Property,OriginalKB,NewKB) :-
	cambiarGrupoObjetos(class(Class,Father,[Props,T],Rels,Objects),class(Class,Father,ListProp,Rels,Objects),OriginalKB,NewKB),
	borrarElementosConPropiedadNegada(Property,Props,Aux),
	borrarElemento(no(Property),Aux,Aux2),
	borrarElemento(Property,Aux2,NewProps),
	write("A"),
	write(Props),
	unificarListas(NewProps,T,ListProp).

% Remove a class property preference
% Example: abrir1(KB), eliminar_clase_propiedad_preferencia(humano,carnivoro,2,KB,NewKB), guardar1(NewKB).

eliminar_clase_propiedad_preferencia(Class,Preference,Peso,OriginalKB,NewKB) :-
	cambiarGrupoObjetos(class(Class,Father,[H,Prefs],Rels,Objects),class(Class,Father,ListPref,Rels,Objects),OriginalKB,NewKB),
	write("Pref:"),
	write(Prefs),
	borrarElementosConPropiedadPref(Preference,Peso,Prefs,Aux),
	borrarElemento(no(Preference),Aux,Aux2),
	borrarElemento(Preference,Aux2,NewPrefs),
	unificarListas(H, NewPrefs, ListPref).

eliminar_clase_propiedad_preferencia(Class,no,Preference,Peso,OriginalKB,NewKB) :-
	cambiarGrupoObjetos(class(Class,Father,[H,Prefs],Rels,Objects),class(Class,Father,ListPref,Rels,Objects),OriginalKB,NewKB),
	write("Pref:"),
	write(Prefs),
	borrarElementosConPropiedadNegadaPref(Preference,Peso,Prefs,Aux),
	borrarElemento(no(Preference),Aux,Aux2),
	borrarElemento(Preference,Aux2,NewPrefs),
	unificarListas(H, NewPrefs, ListPref).

%Remove a class relation
%Example: abrir1(KB), eliminar_clase_relacion(elefante,odia,KB,NewKB), guardar1(NewKB). 

eliminar_clase_relacion(Class,no(Relation),OriginalKB,NewKB) :-
	cambiarGrupoObjetos(class(Class,Father,Props,[Rels,T],Objects),class(Class,Father,Props,ListRel,Objects),OriginalKB,NewKB),
	borrarElementosConPropiedadNegada(Relation,Rels,NewRels),
	unificarListas(NewRels,T,ListRel).

eliminar_clase_relacion(Class,Relation,OriginalKB,NewKB) :-
	cambiarGrupoObjetos(class(Class,Father,Props,[Rels,T],Objects),class(Class,Father,Props,ListRel,Objects),OriginalKB,NewKB),
	borrarElementosConPropiedad(Relation,Rels,NewRels),
	unificarListas(NewRels,T,ListRel).

%Remove a class relation preference
% Example: abrir1(KB), eliminar_clase_relacion_preferencia(mamiferos,amigo,2,KB,NewKB), guardar1(NewKB).
% Example: abrir1(KB), eliminar_clase_relacion_preferencia(mamiferos,dentro,0,KB,NewKB), guardar1(NewKB).
% Example: abrir1(KB), eliminar_clase_relacion_preferencia(humano,no(ecologista),1,KB,NewKB), guardar1(NewKB).

eliminar_clase_relacion_preferencia(Class,no(Preference),Peso,OriginalKB,NewKB) :-
	cambiarGrupoObjetos(class(Class,Father,Props,[H,Prefs],Objects),class(Class,Father,Props,ListPref,Objects),OriginalKB,NewKB),
	borrarElementosConPropiedadNegadaPref(Preference,Peso,Prefs,NewPrefs),
	unificarListas(H,NewPrefs,ListPref).

eliminar_clase_relacion_preferencia(Class,Preference,Peso,OriginalKB,NewKB) :-
	cambiarGrupoObjetos(class(Class,Father,Props,[H,Prefs],Objects),class(Class,Father,Props,ListPref,Objects),OriginalKB,NewKB),
	borrarElementosConPropiedadPref(Preference,Peso,Prefs,NewPrefs),
	unificarListas(H,NewPrefs,ListPref).

%Remove an object property
%Example: abrir1(KB), eliminar_objeto_propiedad(dumbito,vuela,KB,NewKB), guardar1(NewKB).
%Example: abrir1(KB), eliminar_objeto_propiedad(msJumbo,no(vuela),KB,NewKB), guardar1(NewKB).

eliminar_objeto_propiedad(Object,Property,OriginalKB,NewKB) :-
	cambiarGrupoObjetos(class(Class,Father,Props,Rels,Objects),class(Class,Father,Props,Rels,NewObjects),OriginalKB,NewKB),
	esObjetoCompleto([id=>Object,[Properties,T],Relations],Objects,AObject),
	write(Properties),
	cambiarGrupoObjetos([id=>AObject,[Properties,T],Relations],[id=>AObject,ListProp,Relations],Objects,NewObjects),
	borrarElementosConPropiedad(Property,Properties,Aux),
	borrarElemento(no(Property),Aux,Aux2),
	borrarElemento(Property,Aux2,NewProperties),
	unificarListas(NewProperties,T,ListProp).

eliminar_objeto_propiedad(Object,no,Property,OriginalKB,NewKB) :-
	cambiarGrupoObjetos(class(Class,Father,Props,Rels,Objects),class(Class,Father,Props,Rels,NewObjects),OriginalKB,NewKB),
	esObjetoCompleto([id=>Object,[Properties,T],Relations],Objects,AObject),
	write("A:"),
	write(Aux),
	cambiarGrupoObjetos([id=>AObject,[Properties,T],Relations],[id=>AObject,ListProp,Relations],Objects,NewObjects),
	borrarElementosConPropiedadNegada(Property,Properties,Aux),
	borrarElemento(no(Property),Aux,Aux2),
	borrarElemento(Property,Aux2,NewProperties),
	unificarListas(NewProperties,T,ListProp).

%Remove an object property preference
%Example: abrir1(KB), eliminar_objeto_propiedad_preferencia(miky,no(miedoso),0,KB,NewKB), guardar1(NewKB).
%Example: abrir1(KB), eliminar_objeto_propiedad_preferencia(timothy,animal,0,KB,NewKB), guardar1(NewKB).

eliminar_objeto_propiedad_preferencia(Object,Preference,Peso,OriginalKB,NewKB) :-
	cambiarGrupoObjetos(class(Class,Father,Props,Rels,Objects),class(Class,Father,Props,Rels,NewObjects),OriginalKB,NewKB),
	esObjetoCompleto([id=>Object,[H,Preferences],Relations],Objects,AObject),
	write("Pref:"),
	write(Preferences),
	cambiarGrupoObjetos([id=>AObject,[H,Preferences],Relations],[id=>AObject,ListPref,Relations],Objects,NewObjects),
	borrarElementosConPropiedadPref(Preference,Peso,Preferences,Aux),
	borrarElemento(no(Preference),Aux,Aux2),
	borrarElemento(Preference,Aux2,NewPreferences),
	unificarListas(H,NewPreferences,ListPref).

eliminar_objeto_propiedad_preferencia(Object,no,Preference,Peso,OriginalKB,NewKB) :-
	cambiarGrupoObjetos(class(Class,Father,Props,Rels,Objects),class(Class,Father,Props,Rels,NewObjects),OriginalKB,NewKB),
	esObjetoCompleto([id=>Object,[H,Preferences],Relations],Objects,AObject),
	write("Pref:"),
	write(Preferences),
	cambiarGrupoObjetos([id=>AObject,[H,Preferences],Relations],[id=>AObject,ListPref,Relations],Objects,NewObjects),
	borrarElementosConPropiedadNegadaPref(Preference,Peso,Preferences,Aux),
	borrarElemento(no(Preference),Aux,Aux2),
	borrarElemento(Preference,Aux2,NewPreferences),
	unificarListas(H,NewPreferences,ListPref).

%Remove an object relation
%Example: abrir1(KB), eliminar_objeto_relacion(dumbo,no(odia),KB,NewKB), guardar1(NewKB).
%Example: abrir1(KB), eliminar_objeto_relacion(timothy,come,KB,NewKB), guardar1(NewKB).

eliminar_objeto_relacion(Object,no(Relation),OriginalKB,NewKB) :-
	cambiarGrupoObjetos(class(Class,Father,Props,Rels,Objects),class(Class,Father,Props,Rels,NewObjects),OriginalKB,NewKB),
	esObjetoCompleto([id=>Object,Properties,[Relations,T]],Objects,AObject),
	cambiarGrupoObjetos([id=>AObject,Properties,[Relations,T]],[id=>AObject,Properties,ListRel],Objects,NewObjects),
	borrarElementosConPropiedadNegada(Relation,Relations,NewRelations),
	unificarListas(NewRelations,T,ListRel).

eliminar_objeto_relacion(Object,Relation,OriginalKB,NewKB) :-
	cambiarGrupoObjetos(class(Class,Father,Props,Rels,Objects),class(Class,Father,Props,Rels,NewObjects),OriginalKB,NewKB),
	esObjetoCompleto([id=>Object,Properties,[Relations,T]],Objects, AObject),
	write(Relations),
	cambiarGrupoObjetos([id=>AObject,Properties,[Relations,T]],[id=>AObject,Properties,ListRel],Objects,NewObjects),
	borrarElementosConPropiedad(Relation,Relations,NewRelations),
	unificarListas(NewRelations,T,ListRel).

esObjetoCompleto([id=>Object,Properties,Relations],[[id=>Lista,Properties,Relations]|_],Lista):-
	write("elements:"),
	write(Lista),
	member(Object,Lista).

esObjetoCompleto(X,[_|T],AObject):-
	write("T:"),
	write(T),
	esObjetoCompleto(X,T,AObject).	

%Remove an object relation preference
%Example: abrir1(KB), eliminar_objeto_relacion_preferencia(monstruo,ecologista,1,KB,NewKB), guardar1(NewKB).
%Example: abrir1(KB), eliminar_objeto_relacion_preferencia(monstruo,ecologista,2,KB,NewKB), guardar1(NewKB).
eliminar_objeto_relacion_preferencia(Object,no(Preference),Peso,OriginalKB,NewKB) :-
	cambiarGrupoObjetos(class(Class,Father,Props,Rels,Objects),class(Class,Father,Props,Rels,NewObjects),OriginalKB,NewKB),
	esObjetoCompleto([id=>Object,Properties,[Relations,Preferences]],Objects,AObject),
	write(Preferences),
	cambiarGrupoObjetos([id=>AObject,Properties,[Relations,Preferences]],[id=>AObject,Properties,ListPref],Objects,NewObjects),
	borrarElementosConPropiedadNegadaPref(Preference,Peso,Preferences,NewPreferences),
	unificarListas(Relations,NewPreferences,ListPref).

eliminar_objeto_relacion_preferencia(Object,Preference,Peso,OriginalKB,NewKB) :-
	cambiarGrupoObjetos(class(Class,Father,Props,Rels,Objects),class(Class,Father,Props,Rels,NewObjects),OriginalKB,NewKB),
	esObjetoCompleto([id=>Object,Properties,[Relations,Preferences]],Objects, AObject),
	write(Preferences),
	cambiarGrupoObjetos([id=>AObject,Properties,[Relations,Preferences]],[id=>AObject,Properties,ListPref],Objects,NewObjects),
	borrarElementosConPropiedadPref(Preference,Peso,Preferences,NewPreferences),
	unificarListas(Relations,NewPreferences,ListPref).

% Establish the adecuate structure for clases and objects
unificarListas([],[],[]).
unificarListas([A|C],[],[[A|C],[]]).
unificarListas([],[B|D],[[],[B|D]]).
unificarListas([A|C],[B|D],[[A|C],[B|D]]).
