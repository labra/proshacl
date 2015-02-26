
emptyContext(ctx([],[],[])).

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
  [T|Ts], 
  result(Typing, Cs, Rs)) :-
    M > 0, M1 is M - 1,
    matchArc(Ctx,Arc,T,Typing1),
    matchShape(Ctx,arc(Arc,M1,unbounded),Ts,result(Cs,Rs,Typing2)),
    combineTypings(Typing1,Typing2,Typing).

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

%%% Simple Sets
addSet(X,[],[X]).
addSet(X,[X|L],[X|L]) .
addSet(X,[Y|L],[Y|LN]) :-
	X \= Y,
	addSet(X,L,LN).


