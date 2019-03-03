acepta(Edo,[]) :-
final(Edo).
acepta(Edo,[H|T]) :-
trans(Edo,H,NEdo),
acepta(NEdo,T).
acepta(Edo,String) :-
trans_vacio(Edo,NEdo),
acepta(NEdo,String).

trans(s1,0,s1).      trans(s1,1,s2).
trans(s1,0,s4).      trans(s2,0,s2).
trans(s2,0,s3).      trans(s3,1,s5).
trans_vacio(s4,s2).  final(s5).