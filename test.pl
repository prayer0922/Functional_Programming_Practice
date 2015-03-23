mysubset([],_).
mysubset([A|T],B) :- member(A,B), mysubset(T,B).
