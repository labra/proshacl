
%% Contexts
% Context contains a graph, a schema and a typing

emptyContext(ctx([],[],[])).

%% 

matchNode(context(Graph,Schema,Typing), Node, Label, Result) :-
  shape(Label,Schema,Shape),
  triplesAround(Node,Graph,Triples),
  matchShape(context(Graph,Schema,Typing),Shape,Triples,Result) .
  
%%%%%%
% matchShape(Shape,Triples,Typing,Checked,Remaining,Context)

% empty - empty shape matches with any set of triples
matchShape(ctx(_,_,Typing),empty,Ts,result(Typing,[],Ts)).
  
% unbounded1 - single arc with cardinality 0, unbounded matches with any set of triples

matchShape(context(_,_,Typing), 
  arc(_,0,unbounded), 
  Ts, 
  result(Typing, [], Ts)).

% unbounded2 

matchShape(Ctx, 
   arc(Arc,M,unbounded), 
   Ts, 
   result(Typing, [T | Cs], Rs)) :-
    M > 0, M1 is M - 1,
	removeSet(Ts,T,Ts1)
    matchArc(Ctx,Arc,T,Typing1),
    matchShape(Ctx,arc(Arc,M1,unbounded),Ts1,result(Cs,Rs,Typing2)),
    combineTypings(Typing1,Typing2,Typing).

% bounded1

matchShape(context(_,_,Typing), 
  arc(Arc,0,N),
  Ts, 
  result(Typing, [], Ts)) :-
    N >= 0,
    noMatchArcAny(Ts,Ctx,Arc) .

% bounded2
matchShape(Ctx, 
  arc(Arc,0,N),
  Ts, 
  result(Typing,[T|Cs],Rs)) :-
    N >= 0, N1 is N - 1,
	removeTriple(Ts,T,Ts1),
	matchArc(Ctx,Arc,T,Typing1),
	matchShape(Ctx,arc(Arc,0,N1),Ts1,result(Cs,Rs,Typin2),
	combineTypings(Typing1,Typing2,Typing) .

% bounded3

matchShape(Ctx, 
  arc(Arc,M,N),
  Ts, 
  result(Typing,[T|Cs],Rs)) :-
    M > 0, N >= M, M1 is N - 1, N1 is N - 1,
	removeTriple(Ts,T,Ts1),
	matchArc(Ctx,Arc,T,Typing1),
	matchShape(Ctx,arc(Arc,M1,N1),Ts1,result(Cs,Rs,Typin2),
	combineTypings(Typing1,Typing2,Typing) .

% and

matchShape(Ctx,and(E1,E2),Ts,result(Typing,Cs,Rs)) :-
   matchShape(ctx,E1,Ts,result(Typing1,Cs1,Rs1)),
   matchShape(ctx,E2,Rs1,result(Typing2,Cs2,Rs)),
   combineTypings(Typing1,Typing2,Typing),
   union(Cs1,Cs2,Cs) .

% or1

matchShape(Ctx,or(E1,E2),Ts,R) :-
   matchShape(ctx,E1,Ts,R) .

% or2

matchShape(Ctx,or(E1,E2),Ts,R) :-
   matchShape(ctx,E2,Ts,R) .

   
%%%

matchBasicArc(arc(P,V),triple(S,P,O)):-matchValue(V,O) .

matchBasicArc(invArc(P,V),triple(S,P,O)):-matchValue(V,S) .

%%% 
matchValue(_,valueSet(Set),O):-member(O,Set) .

matchValue(_,valueType(T),O):- undefined .

matchValue(context(_,_,Typing), valueRef(Label), O):-
  containsType(Typing,X,Label) .
  
matchValue(Ctx, valueRef(Label), O, Typing) :-
  notContainsType(Ctx,X,Label),
  matchNode(Ctx,X,Label,result(_,_,Typing)) .

%%% Typings

combineTypings([],Typing,Typing).
combineTypings([type(N,T)|Ts1], Ts2, Ts) :-
    addType(N,T,Ts2,TsN),
	combineTypings(Ts1,TsN,Ts).
	
addType(N,T,[],[type(N,[T])]).
addType(N,T,[type(N,Typing)|Ts],[type(N,TypingN)|Ts]) :-
	addSet(T,Typing,TypingN) .
addType(N,T,[type(N1,Typing1)|Ts],[type(N1,Typing1)|TsN]) :-
	N \= N1,
	addType(N,T,Ts,TsN) .

containsType([type(X,Labels)|_],X,Label):- member(Label,Labels) .
containsType([type(Y,_)|Xs],X,Label):- 
   X \= Y, 
   containsType(Xs,X,Label).

%%% Simple Sets
addSet(X,[],[X]).
addSet(X,[X|L],[X|L]) .
addSet(X,[Y|L],[Y|LN]) :-
	X \= Y,
	addSet(X,L,LN).
	
union(Xs,[],Xs).
union([X|Xs],Ys,Zs):-
  union(Xs,Ys,Rs) ,
  addSet(X,Rs,Zs) .

% TODO: check that X is not in Xs
removeSet([X|Xs],X,Xs) .

member(X,[X]) .
member(X,[Y|Ys]):-
  X \= Y, 
  member(X,Ys).

undefined :- writeln("Undefined!").