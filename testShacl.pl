%%% Tests
:- include(shacl) .

:- begin_tests(set) .

test(addSet_empty) :- findall(X,addSet(3,[],X),Xs),assertion(Xs == [[3]]) .

test(addSet_nonExisting) :- 
  findall(X,addSet(3,[2,4],X),Xs), 
  assertion(Xs == [[2,4,3]]) .

test(addSet_existing) :- 
  findall(X,addSet(2,[2,4],X),Xs),
  assertion(Xs == [[2,4]])  .

test(addSet_existing2) :- 
  findall(X,addSet(4,[2,4],X),Xs),
  assertion(Xs == [[2,4]])  .

:- end_tests(set) .	

:- begin_tests(typings) .

test(addType_empty) :- 
  findall(X,addType(x,shape,[],X),Xs), 
  assertion(Xs == [[type(x,[shape])]]) .

test(addType_single_same) :- 
  findall(X,addType(a,t1,[type(a,[t1])],X),Xs), 
  assertion(Xs == [[type(a,[t1])]]) .

test(addType_single_different) :- 
  findall(X,addType(x,shape,[type(x,[shape1])],X),Xs), 
  assertion(Xs == [[type(x,[shape1,shape])]]) .

test(addType_several_same) :- 
  findall(X,addType(x,shape,[type(y,[shape1])],X),Xs), 
  assertion(Xs == [[type(y,[shape1]),
                   type(x,[shape])
				  ]]) .

test(addType_several_same) :- 
  findall(X,addType(x,shape,[type(y,[shape1]),type(z,[shape2])],X),Xs), 
  assertion(Xs == [[type(y,[shape1]),
                    type(z,[shape2]),
					type(x,[shape])
				  ]]) .

:- end_tests(typings) .

:- begin_tests(matchShape).
        
test(matchEmpty) :-
   emptyContext(Ctx),
   findall(Result, matchShape(Ctx,empty,[triple(x,p,y)],Result),Results) ,
   assertion(Results == [result([],[],[triple(x,p,y)])]) .


   
:- end_tests(matchShape).
