

/* Name: Zixuan You, zyou@ucsc.edu. I have not found programming pair yet. I will try to find one for next homework. */




father(al, bud).
father(al, kelly).
mother(peggy, kelly).
mother(peggy, bud).
mother(martha, peggy).



/* The following is answer for Question 8. */

grandma(X,Y) :-
	mother(Z,Y), 
	mother(X,Z).


/* The following is answer for Question 9. */

descendants(X,Y) :-
	father(X,Y);
	mother(X,Y);
	grandma(X,Y).
	

/* The folloiwing is answer for Question 10. */

/* If P is either mother or father of X, then parent(P,X) evaluates to yes. */
parent(P,X) :-
	mother(P,X);father(P,X).


/* Siblings(X,Y) is true if X and Y have a common parent but are not the same person. */
siblings(X,Y) :-
	parent(P,X),parent(P,Y),X\=Y.	



/* The following is answer for Question 11. */



%    ASCII-ART for the NFA:
%
%    (q0)  ---a-->  (q1)  ---b-->  (q2*)
%     |
%     a
%     |
%     V  / --<-- \
%    (q3*)        a
%        \ -->-- /

%  Transition relation:

transition(q0,q1,a).
transition(q1,q2,b).
transition(q0,q3,a).
transition(q3,q3,a).

% Accepting states:

accepting(q2).
accepting(q3).



accepts(State,[]):-
        accepting(State).
accepts(State, [H|T]) :- 
	transition(State,NextState,H),accepts(NextState,T).





